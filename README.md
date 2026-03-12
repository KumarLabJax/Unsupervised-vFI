# Unsupervised-vFI

**Integrating supervised and unsupervised behavior segmentation reveals latent frailty signatures and improves aging clocks in isogenic and outbred mice**

This repository contains the analysis code accompanying the manuscript. It compares **supervised** versus **unsupervised** (latent embeddings, syllable usage, and meta-syllable transition probabilities) feature sets for predicting chronological age, frailty, and proportion of life lived in aged mice using machine learning.

---

## Overview

Two mouse populations — **C57BL/6J (B6J)** and **Diversity Outbred (DO)** — were assessed using automated video analysis. Unsupervised features were derived from a pose estimation and behavioral segmentation pipeline (Keypoint-MoSeq [[3]](#references)), yielding latent embeddings, syllable usage statistics, and meta-syllable transition probabilities. Supervised features consist of hand-crafted kinematic and activity measurements [[1,2]](#references). Multiple machine learning models (Random Forest, XGBoost, Elastic Net) were benchmarked under nested cross-validation to compare predictive performance across feature sets and predict chronological age, frailty, and proportion of life lived.

---

## Repository Structure

```
Unsupervised-vFI/b
├── container/
│   └── RML.def                                # Singularity container definition
├── data/
│   └── 2025-12-30_UA_combined_masterdf.csv   # Master dataset (B6J + DO mice)
├── scripts/
│   ├── main.sh                                # SLURM submission: model benchmarking
│   └── ftr_imp.sh                             # SLURM submission: feature importance
└── src/
    ├── utils.R                                # Feature definitions and metadata
    ├── models.R                               # ML model configurations
    ├── functions.R                            # Core analysis functions
    ├── main.R                                 # Main benchmarking pipeline
    ├── ftr_imp.R                              # Feature importance analysis
    └── postprocess.R                          # Results post-processing and figure generation
```

---

## File Descriptions

### `container/`

| File | Description |
|------|-------------|
| `RML.def` | Singularity container definition file. Builds from `rocker/tidyverse:latest` and installs all required R packages and Python dependencies needed to reproduce the analysis. Build with `singularity build RML.sif RML.def`. |

---

### `data/`

| File | Description |
|------|-------------|
| `2025-12-30_UA_combined_masterdf.csv` | Master dataset containing behavioral features (supervised and unsupervised), frailty scores, age, and metadata for B6 and DO mice across multiple batches and dietary conditions. |

---

### `scripts/`

Shell scripts for submitting jobs to an HPC cluster via SLURM. Both scripts use a Singularity-containerized R environment (`RML.sif`) and allocate 10 CPU cores and 1024 GB memory per job.

| Script | Description |
|--------|-------------|
| `main.sh` | Submits `src/main.R` as a SLURM array job (21 tasks) to run all model benchmarking tasks in parallel, one per feature set × target variable × strain combination. |
| `ftr_imp.sh` | Submits `src/ftr_imp.R` as a single SLURM job to run post-hoc feature importance analysis on trained models. |

---

### `src/`

| Script | Description |
|--------|-------------|
| `utils.R` | Defines all feature sets (supervised, unsupervised, and subsets), target variables, covariates, meta-variables, and color palettes used throughout the analysis. |
| `models.R` | Configures tuned machine learning learners using the `mlr3` framework: Random Forest (`ranger`), XGBoost, Elastic Net (`glmnet`), LightGBM, and TabPFN, each with hyperparameter search spaces. |
| `functions.R` | Core library of analysis functions: mixed-model-based preprocessing to correct for tester/batch/diet effects (`preprocess_data`), automated hyperparameter tuning wrappers, and cross-validated feature importance computation using Shapley values and permutation importance (`compute_variable_importance_all_folds`). |
| `main.R` | Primary pipeline: loads and preprocesses strain-specific data, defines benchmark tasks (Fig. 3: supervised vs. unsupervised vs. combined; Fig. 4: unsupervised subsets), fits and tunes all models under nested cross-validation, and saves benchmark results. |
| `ftr_imp.R` | Post-hoc feature importance pipeline: loads saved benchmark results, identifies the best-performing model per task, and computes aggregated Shapley-based and permutation-based feature importance across cross-validation folds. |
| `postprocess.R` | Post-processing and visualization pipeline: loads saved benchmark results from Fig3ABC, Fig4ABC, and Fig4DEF output folders, scores and aggregates model performance across cross-validation folds, selects best models per task, and generates manuscript figures (Fig. 3A–C comparing supervised vs. unsupervised vs. combined feature sets; Fig. 4A–C comparing unsupervised feature categories; Fig. 4D–F visualizing feature importance by category). |

---

## Computational Environment

All analyses were run on an HPC cluster using the R environment defined in `container/RML.def`. Build the container with:

```bash
singularity build RML.sif container/RML.def
```

---

## Related Repositories

- [**B6JDO-KPMS-Pipeline**](https://github.com/KumarLabJax/B6JDO-KPMS-Pipeline) — Pipeline for training the B6J-DO KPMS model, running inference, and extracting unsupervised behavioral features (Keypoint-MoSeq embeddings, syllable usage, and meta-syllable transition probabilities) from the B6J-DO dataset used in this manuscript.

---

## References

[1] Hession, L.E., Sabnis, G.S., Churchill, G.A., & Kumar, V. (2022). A machine-vision-based frailty index for mice. *Nature Aging*, 2(8), 756–766.

[2] Sabnis, G.S., Churchill, G.A., & Kumar, V. (2025). Machine vision-based frailty assessment for genetically diverse mice. *GeroScience*, 1–14.

[3] Weinreb, C., Pearl, J.E., Lin, S., et al. (2024). Keypoint-MoSeq: parsing behavior by linking point tracking to pose dynamics. *Nature Methods*, 21(7), 1329–1339. https://doi.org/10.1038/s41592-024-02318-2

---

## License

This work is licensed under a [Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](LICENSE).

[![CC BY-NC-SA 4.0](https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png)](https://creativecommons.org/licenses/by-nc-sa/4.0/)

You are free to share and adapt this material for non-commercial purposes, provided you give appropriate credit and distribute your contributions under the same license.
