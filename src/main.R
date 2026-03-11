libs <- c("dplyr", "ggplot2", "readr", "mlr3verse", "mlr3mbo", "mlr3pipelines", "mlr3filters", "future", "mlr3extralearners", "mlr3torch", "lme4", "DiceKriging")
sapply(libs, require, character.only = TRUE)

set.seed(20260121)
source("src/utils.R")
source("src/functions.R")

data <- read_csv("data/2025-12-30_UA_combined_masterdf.csv") |> distinct(NetworkFilename, .keep_all = TRUE)
data$Diet <- factor(data$Diet, levels = c("AL", "1D", "2D", "20", "40")) 

data <- data |> select(-any_of(to_remove))
dataB6 <- data |> filter(Strain == "B6") |> tidyr::drop_na(Tester)
dataDO <- data |> filter(Strain == "DO")

dataB6 <- preprocess_data(df = dataB6, type = "B6")$df
dataDO <- preprocess_data(df = dataDO, type = "DO")$df
data <- preprocess_data(df = data, type = "combined")$df

# Options: "Fig3ABC", "Fig4ABC"
experiment <- "Fig3ABC"

task_specs <- switch(experiment,

  # Fig 3A,B,C — supervised (vFRIGHT, vFI, vPLL), unsupervised (uvFRIGHT, uvFI, uvPLL) features and combined feature set S+U (bvFRIGHT, bvFI, bvPLL)
  Fig3ABC = data.frame(
    id = c("vFRIGHT-B6DO", "vFRIGHT-B6", "vFRIGHT-DO", "uvFRIGHT-B6DO", "uvFRIGHT-B6", "uvFRIGHT-DO", "vFI-B6DO", "vFI-B6", "vFI-DO", "uvFI-B6DO", "uvFI-B6", "uvFI-DO", "vPLL-DO", "uvPLL-DO", "bvFRIGHT-B6DO", "bvFRIGHT-B6", "bvFRIGHT-DO", "bvFI-B6DO", "bvFI-B6", "bvFI-DO", "bvPLL-DO"),
    target = c(rep("AgeAtVid", 6), rep("score", 6), rep("FLL", 2), rep("AgeAtVid", 3), rep("score", 3), "FLL"),
    type   = c(rep("supervised", 3), rep("unsupervised", 3), rep("supervised", 3), rep("unsupervised", 3), "supervised", "unsupervised", rep("all", 7)),
    strain = c(rep(c("combined", "B6", "DO"), times = 4), rep("DO", 2),"combined", "B6", "DO", "combined", "B6", "DO", "DO"),
    stringsAsFactors = FALSE
  ),

  # Fig 4A,B,C — unsupervised feature categories:
  # discrete = syllable usage, continuous = latent embeddings, transition = syllable transition
  Fig4ABC = data.frame(
    id = rep(c("uvFRIGHT-B6", "uvFRIGHT-DO", "uvFRIGHT-B6DO","uvFI-B6", "uvFI-DO", "uvFI-B6DO", "uvPLL-DO"), each = 3),
    target = c(rep(c("AgeAtVid", "score"), each = 9), rep("FLL", 3)),
    type   = rep(c("discrete", "continuous", "transition"), times = 7),
    strain = c(rep(c("B6", "DO", "combined"), each = 3, times = 2), rep("DO", 3)),
    stringsAsFactors = FALSE
  ),

  stop("Unknown experiment: ", experiment)
)


make_task <- function(id, target, type, strain) {
  # Select dataset based on strain
  dataset <- switch(strain,
    "combined" = data,
    "B6" = dataB6,
    "DO" = dataDO,
    stop("Invalid strain: ", strain)
  )

  tsk <- as_task_regr(dataset, target = target, id = id)
  tsk$set_col_roles("MouseID", "group")

  if (type == "supervised") {
    tsk <- tsk$clone()$select(sup_ftrs)
  } else if (type == "unsupervised") {
    tsk <- tsk$clone()$select(c(unsup_ftrs))
  } else if (type == "all") {
    tsk <- tsk$clone()$select(c(sup_ftrs, unsup_ftrs))
  } else if (type == "discrete") {
    tsk <- tsk$clone()$select(intersect(unsup_disc_ftrs, tsk$feature_names))
  } else if (type == "continuous") {
    tsk <- tsk$clone()$select(intersect(unsup_cont_ftrs, tsk$feature_names))
  } else if (type == "transition") {
    tsk <- tsk$clone()$select(intersect(unsup_tp_ftrs, tsk$feature_names))
  }

  return(tsk)
}

tasks <- mapply(
  make_task,
  id = task_specs$id,
  target = task_specs$target,
  type = task_specs$type,
  strain = task_specs$strain,
  SIMPLIFY = FALSE
)

# Optional selector: CLI arg (numeric index or task id) or SLURM array id.
args <- commandArgs(trailingOnly = TRUE)
task_arg <- if (length(args) >= 1) args[1] else ""
if (!nzchar(task_arg)) {
  task_arg <- Sys.getenv("SLURM_ARRAY_TASK_ID", "")
}

task_id_for_file <- "all"
type_id_for_file <- "all"
selected_task_idx <- NULL
if (nzchar(task_arg)) {
  if (grepl("^[0-9]+$", task_arg)) {
    idx <- as.integer(task_arg)
  } else {
    idx <- match(task_arg, task_specs$id)
  }

  if (is.na(idx) || idx < 1 || idx > length(tasks)) {
    stop("Invalid task selector: ", task_arg)
  }

  selected_task_idx <- idx
  tasks <- list(tasks[[idx]])
  task_specs <- task_specs[idx, , drop = FALSE]
  task_id_for_file <- task_specs$id[[1]]
  type_id_for_file <- task_specs$type[[1]]
}

filtered_feature_counts <- sapply(1:length(tasks), function(x) tasks[x][[1]]$n_features)
measure <- msr("regr.rmse")
measures <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))
inner_cv <- rsmp("cv", folds = 2) #5
outer_cv <- rsmp("repeated_cv", folds = 5, repeats = 1) #10,5
resamplings <- rep(list(outer_cv), length(tasks))

source("src/models.R")

strain_resamplings <- list()

# Only create resamplings for strains that exist in the current task set
unique_strains <- unique(task_specs$strain)

for (strain in unique_strains) {
  # Find first task for this strain to instantiate resampling
  strain_task_idx <- which(task_specs$strain == strain)[1]

  # Clone and instantiate once per strain
  rsmp_strain <- outer_cv$clone()
  rsmp_strain$instantiate(tasks[[strain_task_idx]])

  strain_resamplings[[strain]] <- rsmp_strain
}

# Build design using strain-specific resamplings
design <- data.table::rbindlist(lapply(seq_along(tasks), function(i) {
  strain <- task_specs$strain[i]

  data.table::data.table(
    task = list(tasks[[i]]),
    learner = list(
      at_ranger_list[[i]],
      at_glmnet,
      at_xgboost
    ),
    resampling = list(strain_resamplings[[strain]])  # Use strain-specific resampling
  )
}))

bmr <- benchmark(design, store_models = TRUE)

results_dir <- file.path("results", sub("_combined$", "", experiment))
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

file_label <- if (experiment == "Fig4ABC") {
  paste(task_id_for_file, type_id_for_file, sep = "_")
} else {
  task_id_for_file
}

save(
  bmr,
  file = sprintf(
    "%s/benchmark_results_%s_%s_%s.RData",
    results_dir,
    file_label,
    tuning_strategy,
    format(Sys.Date(), "%Y_%m_%d")
  )
)