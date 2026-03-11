make_ranger_learner <- function(max_mtry_val) {
	make_auto_tuner(
		learner = make_preprocessed_learner(
		lrn(
		"regr.ranger",
		num.threads = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1")),
		num.trees = 500,                       
      	#mtry = to_tune(p_int(1, max_mtry_val)),
      	min.node.size = to_tune(p_int(1, 50)),  # much broader
      	sample.fraction = to_tune(p_dbl(0.632, 1))
      	#max.depth = to_tune(p_int(2, 30)),
      	#splitrule = to_tune(c("variance", "extratrees"))
      	)
      ),
      resampling = inner_cv,
      measure = measure
	)
}

at_ranger_list <- lapply(filtered_feature_counts, make_ranger_learner)

at_xgboost <- make_auto_tuner(
  learner = make_preprocessed_learner(lrn(
    "regr.xgboost",
    nrounds = 2000,
    eta = to_tune(c(0.03, 0.1, 0.3)),
    max_depth = to_tune(c(3, 6, 9)),
    min_child_weight = to_tune(c(1, 10)),
    subsample = 0.8,
    colsample_bytree = 0.8
    )
  ),
  resampling = inner_cv,
  measure = measure
)

at_glmnet <- make_auto_tuner(
  learner = make_preprocessed_learner(
  	lrn("regr.glmnet",
  	  alpha = to_tune(c(0, 0.5, 1)),
  	  lambda = to_tune(p_dbl(1e-4, 10, logscale = TRUE))
  	)
  ),
  resampling = inner_cv,
  measure = measure
)

at_lightgbm <- make_auto_tuner(
  learner = make_preprocessed_learner(
    lrn("regr.lightgbm", predict_type = "response",
      num_iterations = to_tune(c(50, 100, 150)),
      learning_rate = to_tune(c(0.01, 0.05, 0.1)),
      num_leaves = to_tune(c(15, 31, 63)),
      feature_fraction = to_tune(c(0.6, 0.8, 1.0)),
      bagging_fraction = to_tune(c(0.6, 0.8, 1.0))
    ),
  ),
  resampling = inner_cv,
  measure = measure
)

at_tabpfn <- make_auto_tuner(
  learner = make_preprocessed_learner(
    lrn("regr.tabpfn",
    #n_estimators = to_tune(c(50, 100, 150)),
    softmax_temperature = to_tune(c(0.5)),ignore_pretraining_limits = TRUE
    ) |> set_threads(n = future::availableCores())
  ),
  resampling = inner_cv,
  measure = measure
)