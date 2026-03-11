libs <- c("dplyr", "mlr3", "ggplot2")
sapply(libs, require, character.only = TRUE)
source("src/utils.R")

mytheme <- theme_minimal(base_size = 14) + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), panel.border = element_rect(color = "black", fill = NA, linewidth = 1), legend.title = element_text(size = 14), legend.text = element_text(size = 12))
setwd("results")
measures <- msrs(c("regr.rmse", "regr.mae", "regr.rsq"))

# Fig 3A,B,C — supervised (vFRIGHT, vFI, vPLL), unsupervised (uvFRIGHT, uvFI, uvPLL) features and combined feature set S+U (bvFRIGHT, bvFI, bvPLL)

if(!dir.exists("Fig3ABC/plots")){
    dir.create("Fig3ABC/plots")
}

files <- list.files("Fig3ABC", pattern = "\\.RData$", full.names = TRUE)

task_id <- gsub("benchmark_results_(.+)_(random|Bayes|grid)_.*\\.RData", "\\1", basename(files))
clock   <- gsub("-(B6DO|B6|DO)$", "", task_id)
dataset <- gsub(".+-(B6DO|B6|DO)$", "\\1", task_id)

dfres <- tibble(files = files, task = task_id, clock = clock, dataset = dataset)

dfres_supervised <- dfres #to be used later for Fig 4A,B,C

envs <- lapply(seq_along(files), function(i) {
  cat("Loading file:", files[i], "\n")
  env <- new.env()
  load(files[i], envir = env)
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
    )) |> rename(MAE = regr.mae, RMSE = regr.rmse, R2 = regr.rsq)
})

dftmp <- do.call(rbind, tmp) |> rename(task = task_id) |> left_join(dfres, by = "task") |> select(-files)

tmp2 <- list()
aggls <- list()
lapply(seq(length(envs)), function(x) {
  bmr <- envs[[x]]$bmr
  agg <- bmr$aggregate(measures)
  aggls[[x]] <<- agg 
  tmp2[[x]] <<- agg |> mutate(learner_id = case_when(
                        learner_id == "removeconstants.scale.regr.ranger.tuned" ~ "RF",
                        learner_id == "removeconstants.scale.regr.xgboost.tuned" ~ "XGB",
                        learner_id == "removeconstants.scale.regr.glmnet.tuned" ~ "LM",
                        TRUE ~ learner_id
                        )) |> rename(MAE = regr.mae, RMSE = regr.rmse, R2 = regr.rsq) |> select(-c('nr', 'resampling_id'))
})

dftmp2 <- do.call(rbind, tmp2) |> rename(task = task_id) |> left_join(dfres, by = "task") |> select(-files)
dfagg <- do.call(rbind, aggls) |> rename(task = task_id) |> left_join(dfres, by = "task") |> select(-files)

best_models <- dftmp2 |> group_by(task) |> slice_min(MAE, n = 1) |> ungroup()

dfplot <- dftmp |> semi_join(best_models, by = c("task", "learner_id")) |> mutate(yvar = ifelse(grepl("FRIGHT", clock), "Age", ifelse(grepl("FI", clock), "Frailty", "PLL")))

dfplot |> filter(yvar == "Frailty") |> mutate(FtrSet = case_when(clock == "bvFI" ~ "S+U", clock == "vFI" ~ "S", clock == "uvFI" ~ "U")) |> ggplot(aes(x = dataset, y = MAE, fill = FtrSet)) + geom_boxplot(position = position_dodge(width = 0.85)) + mytheme + scale_fill_manual(values = c("U" = "#DF8F44FF", "S" = "#00A1D5FF", "S+U" = "#374E55FF")) + labs(title = "Age", x = "Strain", y = "MAE (weeks)", fill = "Feature Set") #For Fig 3A, change filter to clock == "Age", for Fig 3C, change filter to clock == "PLL" and adjust FtrSet mapping accordingly

# Fig 4A,B,C — unsupervised feature categories:
# discrete = syllable usage, continuous = latent embeddings, transition = syllable transition

if(!dir.exists("Fig4ABC/plots")){
    dir.create("Fig4ABC/plots")
}

files <- c(
    list.files("Fig4ABC", pattern = "\\.RData$", full.names = TRUE),
    list.files("Fig3ABC", pattern = "\\.RData$", full.names = TRUE)
)

is_fig4 <- grepl("^Fig4ABC/", files)

task_id <- ifelse(
  is_fig4,
  gsub("benchmark_results_(.+)_(continuous|discrete|transition)_(random|Bayes|grid)_.*\\.RData", "\\1", basename(files)),
  gsub("benchmark_results_(.+)_(random|Bayes|grid)_.*\\.RData", "\\1", basename(files))
)
clock <- gsub("-(B6DO|B6|DO)$", "", task_id)
dataset <- gsub(".+-(B6DO|B6|DO)$", "\\1", task_id)
feature_set <- ifelse(
  is_fig4,
  gsub("benchmark_results_.+_(continuous|discrete|transition)_(random|Bayes|grid)_.*\\.RData", "\\1", basename(files)),
  "supervised"
)

dfres <- tibble(files = files, task = task_id, clock = clock, dataset = dataset, ftr_set = feature_set, folder = ifelse(is_fig4, "Fig4", "Fig3"))

envs <- lapply(seq_along(files), function(i) {
  cat("Loading file:", files[i], "\n")
  env <- new.env()
  load(files[i], envir = env)
  # Store metadata about which folder this came from
  env$source_folder <- ifelse(grepl("^Fig4ABC/", files[i]), "Fig4", "Fig3")
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
    mutate(files = envs[[x]]$source_file)
})

dftmp <- do.call(rbind, tmp) |> rename(task = task_id) |> left_join(dfres, by = c("files","task")) |> select(-files)

tmp2 <- list()
aggls <- list()
lapply(seq(length(envs)), function(x) {
  bmr <- envs[[x]]$bmr
  agg <- bmr$aggregate(measures)
  aggls[[x]] <<- agg |> mutate(files = envs[[x]]$source_file)
  tmp2[[x]] <<- agg |> mutate(learner_id = case_when(
                        learner_id == "removeconstants.scale.regr.ranger.tuned" ~ "RF",
                        learner_id == "removeconstants.scale.regr.xgboost.tuned" ~ "XGB",
                        learner_id == "removeconstants.scale.regr.glmnet.tuned" ~ "LM",
                        TRUE ~ learner_id
                        )) |> rename(MAE = regr.mae, RMSE = regr.rmse, R2 = regr.rsq) |> select(-c('nr', 'resampling_id')) |>
    mutate(files = envs[[x]]$source_file)
})

dftmp2 <- do.call(rbind, tmp2) |> rename(task = task_id) |> left_join(dfres, by = c("files","task")) |> select(-files)
dfagg <- do.call(rbind, aggls) |> rename(task = task_id) |> left_join(dfres, by = c("files","task")) |> select(-files)
best_models <- dftmp2 |> group_by(task, ftr_set) |> slice_min(MAE, n = 1) |> ungroup()

dfplot <- dftmp |> semi_join(best_models, by = c("task", "learner_id", "ftr_set")) |> mutate(yvar = ifelse(grepl("FRIGHT", clock), "Age", ifelse(grepl("FI", clock), "Frailty", "PLL")))

dfplot |> filter(yvar == "Frailty") |> mutate(FtrSet = case_when(ftr_set == "continuous" ~ "Latent Embeddings", ftr_set == "discrete" ~ "Syllable Usage", ftr_set == "transition" ~ "Syllable Transition", ftr_set == "supervised" ~ "Supervised")) |> ggplot(aes(x = dataset, y = MAE, fill = FtrSet)) + geom_boxplot(position = position_dodge(width = 0.85)) + mytheme + scale_fill_manual(values = c("Syllable Usage" = "#EFC000FF", "Latent Embeddings" = "#CD534CFF", "Syllable Transition" = "#868686FF", "Supervised" = "#00A1D5FF")) + labs(title = "Frailty", x = "Strain", y = "MAE (weeks)", fill = "Feature Set") #For Fig 4A, change filter to clock == "Age", for Fig 4C, change filter to clock == "PLL" 


#Fig4 D, E, F - Feature importance for best models from each feature set category
if(!dir.exists("Fig4DEF/plots")){
    dir.create("Fig4DEF/plots")
}

files <- list.files("Fig4DEF", pattern = "\\.RData$", full.names = TRUE)
envs <- lapply(files, function(x) {
  cat("Loading", x, "\n")
  env <- new.env()  
  load(x, envir = env)  
  return(env)
})

res <- envs[[1]]$results$aggregated

dfbvFRIGHT <- res[['bvFRIGHT-B6DO']] |> mutate(clock = "bvFRIGHT", dataset = "B6DO") |> mutate(ftr_set = case_when(Variable %in% unsup_disc_ftrs ~ "Syllable Usage", Variable %in% unsup_cont_ftrs ~ "Latent Embeddings", Variable %in% unsup_tp_ftrs ~ "Syllable Transition", TRUE ~ "Supervised"))
dfbvFI <- res[['bvFI-B6DO']] |> mutate(clock = "bvFI", dataset = "B6DO") |> mutate(ftr_set = case_when(Variable %in% unsup_disc_ftrs ~ "Syllable Usage", Variable %in% unsup_cont_ftrs ~ "Latent Embeddings", Variable %in% unsup_tp_ftrs ~ "Syllable Transition", TRUE ~ "Supervised"))
dfbvPLL <- res[['bvPLL-DO']] |> mutate(clock = "bvPLL", dataset = "DO") |> mutate(ftr_set = case_when(Variable %in% unsup_disc_ftrs ~ "Syllable Usage", Variable %in% unsup_cont_ftrs ~ "Latent Embeddings", Variable %in% unsup_tp_ftrs ~ "Syllable Transition", TRUE ~ "Supervised"))

dftmp <- dfbvFRIGHT #For Fig 4D, change to dfbvFI for Fig 4E, dfbvPLL for Fig 4F

dftmp |> group_by(Type) |> summarise(mean_shapley = mean(Shapley_Importance_Mean, na.rm = TRUE), mean_permutation = mean(Permutation_Importance_Mean, na.rm = TRUE), .groups = "drop") |> ggplot(aes(x = Type, y = mean_permutation, fill = Type)) + geom_bar(stat = "identity", position = position_dodge(width = 0.9)) + mytheme + labs(title = "Permutation Importance", x = "Feature Type", y = "Mean Importance", fill = "Strain") + scale_fill_manual(values = c("Latent Embedding" = "#CD534CFF", "Syllable Usage" = "#EFC000FF", "Syllable Transition" = "#868686FF", "Supervised" = "#00A1D5FF")) + scale_y_continuous(expand = expansion(mult = c(0, 0.05)))