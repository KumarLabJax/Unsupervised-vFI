
preprocess_data <- function(df, type){

	#'''
	#The preprocess_data function adjusts frailty scores for tester effect in the raw dataset
	#'''

	if (type == "frailty"){

		df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		return(df)


	} else if (type == "B6") {
		mod.lmm <- lmer(score ~ AgeW + Weight + Sex + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",], ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], df$score - ranef(mod.lmm)$Tester["Mackenzie",])))
		df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch["Batch1",], ifelse(df$Batch == "Batch2", df$score - ranef(mod.lmm)$Batch["Batch2",], df$score - ranef(mod.lmm)$Batch["Batch3",]))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_scorer <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1]) 
		df_scorer$Scorer <- c("Scorer1", "Scorer2", "Scorer3", "Scorer4")
		p_scorer <- ggplot(df_scorer, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[3]), expression(S[4]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_bw(base_size = 16) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE")
		return(list(df = df, p_scorer = p_scorer, RLRT = RLRT, p_batch = p_batch, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))
	} else if (type == "combined"){
        df$Diet <- factor(df$Diet, levels = c("AL", "20", "40", "1D", "2D"))
        mod.lmm <- lmer(score ~ AgeW + Sex + (1|Diet) + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA #Need to sex and add once I get the metadata for DO
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
        mod.lmm2 <- update(mod.lmm, . ~ . - (1|Diet))
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",],ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], ifelse(df$Tester == "Mackenzie", df$score - ranef(mod.lmm)$Tester["Mackenzie",], ifelse(df$Tester == "Farrar Cobb", df$score - ranef(mod.lmm)$Tester["Farrar Cobb",], ifelse(df$Tester == "Sean Deats", df$score - ranef(mod.lmm)$Tester["Sean Deats",], df$score - ranef(mod.lmm)$Tester["TesterNA",]))))))
		df$score <- ifelse(df$Batch == 'Batch1', df$score - ranef(mod.lmm)$Batch["Batch1",], ifelse(df$Batch == 'Batch2', df$score - ranef(mod.lmm)$Batch["Batch2",], ifelse(df$Batch == 'Batch3', df$score - ranef(mod.lmm)$Batch["Batch3",], ifelse(df$Batch == 'New', df$score - ranef(mod.lmm)$Batch["New",], df$score - ranef(mod.lmm)$Batch["Old",]))))
        df$score <- ifelse(df$Diet == 'AL', df$score - ranef(mod.lmm)$Diet["AL",], ifelse(df$Diet == '20', df$score - ranef(mod.lmm)$Diet["20",], ifelse(df$Diet == '40', df$score - ranef(mod.lmm)$Diet["40",], ifelse(df$Diet == '1D', df$score - ranef(mod.lmm)$Diet["1D",], df$score - ranef(mod.lmm)$Diet["2D",]))))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_score <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1])
		df_score$Scorer <- c("Scorer1", "Scorer2", "Scorer3", "Scorer4", "Scorer5", "Scorer6", "Scorer7")
		p_scorer <- ggplot(df_score, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[3]), expression(S[4]), expression(S[5]), expression(S[6]), expression(S[7]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_diet <- data.frame(Diet = row.names(ranef(mod.lmm)$Diet), RE = ranef(mod.lmm)$Diet[,1])
        p_diet <- ggplot(df_diet, aes(x = Diet, y = RE, fill = Diet)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = CalicoColors) + xlab("Diet") + ylab("Estimated RE") + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE") + scale_x_discrete("Batch", labels = c(expression(B[1]), expression(B[2]), expression(B[3]), expression(B[4]), expression(B[5]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		return(list(df = df, p_scorer = p_scorer, p_batch = p_batch, p_diet = p_diet, RLRT = RLRT, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, diet_effects = ranef(mod.lmm)$Diet, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))
    }
    else {

		df$Diet <- factor(df$Diet, levels = c("AL", "20", "40", "1D", "2D"))
        mod.lmm <- lmer(score ~ AgeW + Sex + (1|Diet) + (1|Tester) + (1|Batch), data = df[!duplicated(df$MouseID),]) #mA #Need to sex and add once I get the metadata for DO
		mod.lmm1 <- update(mod.lmm, . ~ . - (1|Tester)) #m0
        mod.lmm2 <- update(mod.lmm, . ~ . - (1|Diet))
		m.tester <- update(mod.lmm, . ~ . - (1|Batch))
		RLRT <- c(RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$statistic,RLRT_stats <- RLRsim::exactRLRT(m.tester,mod.lmm,mod.lmm1)$p.value)
		df$score <- ifelse(df$Tester == "Amanda", df$score - ranef(mod.lmm)$Tester["Amanda",],ifelse(df$Tester == "Gaven", df$score - ranef(mod.lmm)$Tester["Gaven",],ifelse(df$Tester == "Hannah", df$score - ranef(mod.lmm)$Tester["Hannah",], ifelse(df$Tester == "Mackenzie", df$score - ranef(mod.lmm)$Tester["Mackenzie",], ifelse(df$Tester == "Farrar Cobb", df$score - ranef(mod.lmm)$Tester["Farrar Cobb",], ifelse(df$Tester == "Sean Deats", df$score - ranef(mod.lmm)$Tester["Sean Deats",], df$score - ranef(mod.lmm)$Tester["TesterNA",]))))))
		df$score <- ifelse(df$Batch == 'New', df$score - ranef(mod.lmm)$Batch["New",], df$score - ranef(mod.lmm)$Batch["Old",])
        df$score <- ifelse(df$Diet == 'AL', df$score - ranef(mod.lmm)$Diet["AL",], ifelse(df$Diet == '20', df$score - ranef(mod.lmm)$Diet["20",], ifelse(df$Diet == '40', df$score - ranef(mod.lmm)$Diet["40",], ifelse(df$Diet == '1D', df$score - ranef(mod.lmm)$Diet["1D",], df$score - ranef(mod.lmm)$Diet["2D",]))))
		df$score[df$score < 0] <- 0
		#df <- df[,-which(names(df) %in% c("Tester","Sex","Weight","Batch","NetworkFilename"))]
		df_score <- data.frame(Scorer = row.names(ranef(mod.lmm)$Tester), RE = ranef(mod.lmm)$Tester[,1])
		df_score$Scorer <- c("Scorer1", "Scorer2", "Scorer5", "Scorer6","Scorer7")
		p_scorer <- ggplot(df_score, aes(x = Scorer, y = RE, fill = Scorer)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 16) + theme(legend.position = "none") + scale_fill_manual(values = c("Scorer1" = "#800000FF", "Scorer2" = "#D6D6CEFF", "Scorer3" = "#FFB547FF", "Scorer4" = "#ADB17DFF", "Scorer5" = "#5B8FA8FF", "Scorer6" = "#D49464FF", "Scorer7" = "#B1746FFF", "Scorer8" = "#8A8B79FF")) + xlab("Scorer") + ylab("Estimated RE") + scale_x_discrete("Scorer", labels = c(expression(S[1]), expression(S[2]), expression(S[5]), expression(S[6]), expression(S[7]))) + theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
		df_diet <- data.frame(Diet = row.names(ranef(mod.lmm)$Diet), RE = ranef(mod.lmm)$Diet[,1])
        p_diet <- ggplot(df_diet, aes(x = Diet, y = RE, fill = Diet)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 14) + theme(legend.position = "none") + scale_fill_manual(values = CalicoColors) + xlab("Diet") + ylab("Estimated RE") + theme(axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12))
		df_batch <- data.frame(Batch = row.names(ranef(mod.lmm)$Batch), RE = ranef(mod.lmm)$Batch[,1])
		p_batch <- ggplot(df_batch, aes(x = Batch, y = RE, fill = Batch)) + geom_bar(stat = "identity", color = "black") + theme_classic(base_size = 14) + theme(legend.position = "none") + scale_fill_brewer(palette = "Set1") + xlab("Batch") + ylab("Estimated RE") + theme(axis.text.y = element_text(size = 14), axis.text.x = element_text(size = 14))
		return(list(df = df, p_scorer = p_scorer, p_batch = p_batch, p_diet = p_diet, RLRT = RLRT, scorer_effects = ranef(mod.lmm)$Tester, batch_effects = ranef(mod.lmm)$Batch, diet_effects = ranef(mod.lmm)$Diet, var_percent = summary(mod.lmm)$varcor |> as_tibble() |> mutate(percent = sdcor/sum(sdcor)) |> select(grp, percent)))

    }

}

#Select tuning strategy for hyperparameter tuning: "grid", "random", or "Bayes"
tuning_strategy <- "grid"
threshold <- 0.1

tuning_registry <- list(
  grid = list(
    tuner = tnr("grid_search", resolution = 3, batch_size = 100),
    terminator = NULL
  ),
  random = list(
    tuner = tnr("random_search", batch_size = 10),
    terminator = trm("evals", n_evals = 30)
  ),
  Bayes = list(
    tuner = tnr("mbo"),
    terminator = trm("evals", n_evals = 60)
  )
)

make_auto_tuner <- function(learner, resampling, measure, strategy = tuning_strategy) {
  spec <- tuning_registry[[strategy]]
  if (is.null(spec)) {
    stop("Unknown tuning strategy: ", strategy)
  }

  if (is.null(spec$terminator)) {
    return(auto_tuner(
      tuner = spec$tuner,
      learner = learner,
      resampling = resampling,
      measure = measure
    ))
  }

  auto_tuner(
    tuner = spec$tuner,
    learner = learner,
    resampling = resampling,
    measure = measure,
    terminator = spec$terminator
  )
}

make_preprocessed_learner <- function(learner, cutoff = threshold) {
  graph <- po("removeconstants") %>>%
    #po("filter", filter = flt("find_correlation"), filter.cutoff = cutoff) %>>%
    po("scale") %>>%
    learner
  as_learner(graph)
}

get_filtered_feature_count <- function(task, cutoff = threshold) {
  graph <- po("removeconstants") %>>%
    #po("filter", filter = flt("find_correlation"), filter.cutoff = cutoff) %>>%
    po("scale")
  graph$train(task)
  length(graph$pipeops$filter$state$selected_features)
}


# Function to compute variable importance across all folds and aggregate
compute_variable_importance_all_folds <- function(task_pattern, nsim = 10) {
  # Find all tasks matching the pattern
  pattern_to_match <- task_pattern
  matching_tasks <- best_models %>%
    filter(grepl(pattern_to_match, ftr_set)) %>%
    pull(task_id)

  if (length(matching_tasks) == 0) {
    stop("No tasks found matching pattern: ", task_pattern)
  }

  aggregated_list <- list()
  iteration_list <- list()

  for (task_name in matching_tasks) {
    cat("Processing", task_name, "...\n")

    # Find the best learner for this task
    best_learner_info <- best_models %>% filter(task_id == task_name)
    best_learner_id <- best_learner_info$learner_id[1]

    # Map back to original learner name
    learner_map <- c("RF" = "removeconstants.scale.regr.ranger.tuned",
                     "XGB" = "removeconstants.scale.regr.xgboost.tuned",
                     "LM" = "removeconstants.scale.regr.glmnet.tuned")
    original_learner_id <- learner_map[best_learner_id]

    # Find the environment containing this task
    env_idx <- which(sapply(envs, function(e) {
      task_name_in_env <- e$bmr$tasks$task[[1]]$id
      if (grepl("results8", e$source_file)) {
        task_name_in_env <- gsub("Supervised", "All", task_name_in_env)
      }
      task_name_in_env == task_name
    }))

    if (length(env_idx) == 0) {
      warning("Environment not found for task: ", task_name)
      next
    }

    bmr <- envs[[env_idx[1]]]$bmr
    scored_data <- bmr$score(measures)

    # Get all iterations for this learner
    all_iterations <- scored_data %>%
      filter(learner_id == original_learner_id) %>%
      pull(iteration) %>%
      unique()

    cat("  Processing", length(all_iterations), "iterations\n")

    # Store importance results for all iterations
    iteration_results <- list()

    for (iter in all_iterations) {
      cat("    Iteration", iter, "...\n")

      # Filter for the best learner and current iteration
      row_idx <- which(scored_data$learner_id == original_learner_id &
                       scored_data$iteration == iter)

      if (length(row_idx) == 0) {
        warning("No data found for iteration: ", iter)
        next
      }

      learner_obj <- scored_data$learner[[row_idx[1]]]
      task_obj <- scored_data$task[[row_idx[1]]]
      resampling_obj <- scored_data$resampling[[row_idx[1]]]

      # Extract training data
      train_indices <- resampling_obj$train_set(iter)
      train_task_raw <- task_obj$clone()$filter(train_indices)

      # Get preprocessed data
      preproc <- po("removeconstants") %>>% po("scale")
      train_task_prep <- preproc$train(train_task_raw)[[1]]

      x_var_prep <- train_task_prep$feature_names
      y_var <- train_task_prep$target_names
      x_train_prep <- as.data.frame(train_task_prep$data(cols = x_var_prep))
      y_train_prep <- train_task_prep$data(cols = y_var)[[1]]

      # Extract the fitted model
      if (best_learner_id == "XGB") {
        xgb_model <- learner_obj$learner$model$regr.xgboost$model[[1]]
        fitted_model <- attr(xgb_model, "model")

        pfun_shap <- function(object, newdata) {
          pred <- predict(object, newdata = as.matrix(newdata))
          matrix(pred, ncol = 1, dimnames = list(NULL, "pred"))
        }

        pfun_perm <- function(object, newdata) {
          predict(object, newdata = as.matrix(newdata))
        }

      } else if (best_learner_id == "RF") {
        fitted_model <- learner_obj$learner$model$regr.ranger$model[[1]]

        pfun_shap <- function(object, newdata) {
          predict(object, data = as.matrix(newdata))$predictions
        }

        pfun_perm <- function(object, newdata) {
          predict(object, data = as.matrix(newdata))$predictions
        }

      } else if (best_learner_id == "LM") {
        fitted_model <- learner_obj$learner$model$regr.glmnet$model[[1]]

        pfun_shap <- function(object, newdata) {
          as.vector(predict(object, newx = as.matrix(newdata), s = object$lambda.min))
        }

        pfun_perm <- function(object, newdata) {
          as.vector(predict(object, newx = as.matrix(newdata), s = object$lambda.min))
        }
      }

      # Compute Shapley importance
      vi_shap <- tryCatch({
        vi(
          fitted_model,
          method = "shap",
          train = x_train_prep,
          pred_wrapper = pfun_shap,
          feature_names = colnames(x_train_prep)
        )
      }, error = function(e) {
        warning("Error computing Shapley for iteration ", iter, ": ", e$message)
        NULL
      })

      # Compute permutation importance
      vi_perm <- tryCatch({
        vi(
          fitted_model,
          method = "permute",
          train = x_train_prep,
          target = as.vector(y_train_prep),
          metric = "rmse",
          pred_wrapper = pfun_perm,
          feature_names = colnames(x_train_prep),
          nsim = nsim
        )
      }, error = function(e) {
        warning("Error computing permutation importance for iteration ", iter, ": ", e$message)
        NULL
      })

      # Store results for this iteration
      if (!is.null(vi_shap) && !is.null(vi_perm)) {
        iteration_results[[as.character(iter)]] <- tibble(
          Variable = vi_shap$Variable,
          Shapley_Importance = vi_shap$Importance,
          Permutation_Importance = vi_perm$Importance,
          Permutation_StDev = vi_perm$StDev,
          iteration = iter
        )
      }
    }

    # Aggregate across all iterations
    if (length(iteration_results) > 0) {
      all_iterations_df <- bind_rows(iteration_results)

      aggregated_results <- all_iterations_df %>%
        group_by(Variable) %>%
        summarise(
          Shapley_Importance_Mean = mean(Shapley_Importance, na.rm = TRUE),
          Shapley_Importance_SD = sd(Shapley_Importance, na.rm = TRUE),
          Permutation_Importance_Mean = mean(Permutation_Importance, na.rm = TRUE),
          Permutation_Importance_SD = sd(Permutation_Importance, na.rm = TRUE),
          n_iterations = n(),
          .groups = "drop"
        )

      aggregated_list[[task_name]] <- aggregated_results
      iteration_list[[task_name]] <- all_iterations_df
    }
  }

  return(list(
    aggregated = aggregated_list,
    by_iteration = iteration_list
  ))
}