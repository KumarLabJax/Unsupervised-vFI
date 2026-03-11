libs <- c("dplyr", "ggplot2", "mlr3verse", "mlr3pipelines", "mlr3filters", "data.table", "stringr","ranger", "glmnet", "iml", "vip")
sapply(libs, require, character.only = TRUE)

source("src/functions.R")
source("src/utils.R")

files <- c(
    list.files("results/Fig4ABC", pattern = "\\.RData$", full.names = TRUE),
    list.files("results/Fig3ABC", pattern = "*bv(FI|FRIGHT|PLL).*\\.RData$", full.names = TRUE)
)

task_id <- sub(".*benchmark_results_(.+?)_(grid|continuous|discrete|transition)_.*", "\\1", files)
clock   <- sub(".*[a-z](FI|FRIGHT|PLL)-.*",   "\\1", files)
strain  <- sub(".*?(B6DO|B6|DO)_.*",            "\\1", files)
ftr_set <- ifelse(grepl("Fig3ABC", files) & grepl("bv(FI|FRIGHT|PLL)", files), "all", sub(".*_(continuous|discrete|transition)_.*", "\\1", files))

folder <- ifelse(grepl("Fig4ABC/", files), "Fig4", "Fig3")
dfres <- tibble(files = files, task = task_id, strain = strain, clock = clock, ftr_set = ftr_set, folder = folder) 
files <- dfres |> pull(files)

measures <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))

envs <- lapply(seq_along(files), function(i) {
  cat("Loading file:", files[i], "\n")
  env <- new.env()
  load(files[i], envir = env)
  # Store metadata about which folder this came from
  env$source_folder <- folder[i]
  env$source_file <- files[i]
  return(env)
})

tmp <- list()
lapply(seq(length(envs)), function(x) {
  bmr <- envs[[x]]$bmr
  scored <- bmr$score(measures) |> as_tibble()

  # Check available columns and use the correct iteration column name
  if("iteration" %in% names(scored)) {
    iter_col <- "iteration"
  } else if("iter" %in% names(scored)) {
    iter_col <- "iter"
  } else {
    # If no iteration column, create one from row number within each task-learner combo
    scored <- scored |>
      group_by(task_id, learner_id) |>
      mutate(iteration = row_number()) |>
      ungroup()
    iter_col <- "iteration"
  }

  tmp[[x]] <<- scored |>
    select(all_of(c('task_id', 'learner_id', iter_col, 'regr.rmse', 'regr.mae', 'regr.rsq'))) |>
    rename(iteration = all_of(iter_col)) |>
    mutate(learner_id = case_when(
      learner_id == "removeconstants.scale.regr.ranger.tuned" ~ "RF",
      learner_id == "removeconstants.scale.regr.xgboost.tuned" ~ "XGB",
      learner_id == "removeconstants.scale.regr.glmnet.tuned" ~ "LM",
      TRUE ~ learner_id
    )) |> rename(MAE = regr.mae, RMSE = regr.rmse, R2 = regr.rsq) |>
    mutate(strain = strain[x], clock = clock[x], ftr_set = ftr_set[x])
})

dftmp <- do.call(rbind, tmp) 

tmp2 <- list()
aggls <- list()
lapply(seq(length(envs)), function(x) {
  bmr <- envs[[x]]$bmr
  agg <- bmr$aggregate(measures)
  aggls[[x]] <<- agg |> mutate(folder = folder[x])
  tmp2[[x]] <<- agg |> mutate(learner_id = case_when(
                        learner_id == "removeconstants.scale.regr.ranger.tuned" ~ "RF",
                        learner_id == "removeconstants.scale.regr.xgboost.tuned" ~ "XGB",
                        learner_id == "removeconstants.scale.regr.glmnet.tuned" ~ "LM",
                        TRUE ~ learner_id
                        )) |> rename(MAE = regr.mae, RMSE = regr.rmse, R2 = regr.rsq) |> select(-c('nr', 'resampling_id')) |>
    mutate(strain = strain[x], clock = clock[x], ftr_set = ftr_set[x])
})
dftmp2 <- do.call(rbind, tmp2)
dfagg <- do.call(rbind, aggls)

best_models <- dftmp2 |> group_by(task_id, ftr_set) |> slice_min(MAE, n = 1) |> ungroup() 
best_models2 <- best_models |> filter(ftr_set == "all")


# Extract unique task patterns
unique_patterns <- best_models2 |> pull(ftr_set) |> unique() |> sort()

cat("Available task patterns:\n")
print(unique_patterns)

# Optional selector: CLI arg (numeric index or task pattern name) or SLURM array id.
args <- commandArgs(trailingOnly = TRUE)
pattern_arg <- if (length(args) >= 1) args[1] else ""
if (!nzchar(pattern_arg)) {
  pattern_arg <- Sys.getenv("SLURM_ARRAY_TASK_ID", "")
}

selected_pattern <- NULL
if (nzchar(pattern_arg)) {
  if (grepl("^[0-9]+$", pattern_arg)) {
    idx <- as.integer(pattern_arg)
  } else {
    idx <- match(pattern_arg, unique_patterns)
  }

  if (is.na(idx) || idx < 1 || idx > length(unique_patterns)) {
    stop("Invalid pattern selector: ", pattern_arg)
  }

  selected_pattern <- unique_patterns[idx]
  cat("Processing pattern:", selected_pattern, "(index:", idx, ")\n")
} else {
  stop("No pattern specified. Provide a numeric index, pattern name, or run via SLURM array.")
}

selected_pattern <- "all"

results <- compute_variable_importance_all_folds(task_pattern = selected_pattern, nsim = 10)

if(!dir.exists("results/ftr_imp")) {
  dir.create("results/ftr_imp", recursive = TRUE)
}

output <- sprintf(
  "results/ftr_imp/variable_importance_all_folds_%s_%s.RData",
  selected_pattern,
  format(Sys.Date(), "%Y_%m_%d")
)
save(results, file = output)
cat("Variable importance results saved to:", output, "\n")
