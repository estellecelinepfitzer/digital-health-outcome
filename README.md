# Digital Health Outcome

**Short description.**  
Reproducible R code to engineer features and build classification models that predict whether a digital-health company achieves a *successful* outcome (IPO/M&A/public) versus *no success* (still private without backing / out of business), using structured firmographics, financing timelines, IP/clinical indicators, and geospatial proximity to VC/company hubs. Feature engineering and models are implemented with the *tidyverse*/*tidymodels* ecosystem, with additional utilities for class imbalance, SHAP/vip explanations, and benchmarking on a fixed or sampled set of companies.

---

## Repository layout

```
digital-health-outcome/
├─ FeatureEngineering/
│  ├─ DataEngineering.R
│  └─ Hubs.R
├─ ModelDev/
│  ├─ Predictions.R
│  ├─ Predictions_without_LMM_derived_feature.R
│  └─ Benchmarking_.R
└─ .gitignore
```

- **FeatureEngineering/DataEngineering.R** — end-to-end data cleaning + feature engineering (CAGR/YoY from `employee_history`, investor counts, financing-timing features, geospatial hub features, missingness flags, correlation filter).  
- **FeatureEngineering/Hubs.R** — helper logic for hub coordinates/distances.  
- **ModelDev/Predictions.R** — trains models with tidymodels and produces an ensemble.  
- **ModelDev/Predictions_without_LMM_derived_feature.R** — ablation: compare *with vs. without* LLM-derived features.  
- **ModelDev/Benchmarking_.R** — samples the test set for a compact performance table.

---

## What the code does (method overview)

### Target definition
`ownership_status` is normalized into a 2-class factor **status** with levels `Success` vs `No Success` (e.g., “Publicly Held”, “Acquired/Merged” → `Success`; “Privately Held (no backing)”, “Out of Business” → `No Success`).  

### Feature engineering highlights

1. **String normalization** to snake_case across categorical variables (`hq_city`, `hq_country_territory_region`, etc.).  
2. **Employee growth**: CAGR and YoY from `employee_history`.  
3. **Investor signals**: merged counts of active/former investors.  
4. **Count imputations**: NA→0 for trial/patent/financing metrics.  
5. **Accounting recency**: derive fiscal year, drop outdated `revenue`/`ebitda`.  
6. **Financing dynamics**: time deltas between rounds and missing-indicator flags.  
7. **Hub proximity** (via `maps::world.cities` + Haversine): distance to VC hubs and company hubs, plus binary membership within 64 km.  
8. **Missingness flags** and pruning of high-NA (>80%) or highly correlated (>0.9) columns.

### Modeling & evaluation

- **Tidymodels workflow** with cross-validation, AUC as the primary metric.  
- **Imbalance handling** via SMOTE (`themis` recipes).  
- **Ensemble predictions** consolidated in `ensemble_preds_weighted` (`row_id`, `pred_class`, `pred_success`, `pred_no_success`).  
- **Ablation study**: repeated CV comparing models *with* and *without* LLM-derived features.  

### Benchmarking

`Benchmarking_.R` draws *k per class* from the test data and prints a summary table (company, actual vs. predicted, success probabilities) after verifying inputs and setting a deterministic seed.

---

## Data requirements

> **No data is included** — you must supply a CSV with the columns referenced by the scripts.

**Required fields (non-exhaustive):**

- Target: `ownership_status` → engineered `status`
- Identifiers: `row_id`, `company_name`
- Firmographics: `employees`, `employee_history`, `year_founded`, `hq_city`, `hq_country_territory_region`, `hq_global_region`
- Financing: `first_financing_date`, `last_financing_date`, `fiscal_period`
- IP/clinical: `total_clinical_trials`, `total_patent_documents`, `total_patent_families`, `active_patent_documents`
- Capital & investors: `number_active_investors`, `former_investors`, `total_raised`, `number_of_ucc_filings`

Update the file path at the top of `FeatureEngineering/DataEngineering.R` if your dataset has a different name (default: `clean_dataset.csv`).

---

## Quick start

```r
# install dependencies
install_pkgs <- c(
  "tidymodels","dplyr","purrr","stringr","lubridate","smotefamily","vip",
  "caret","themis","doParallel","future","yardstick","rlang","shapviz","pROC",
  "geosphere","maps","readr","knitr","tidyr","glue"
)
install.packages(setdiff(install_pkgs, rownames(installed.packages())))

# 1) Feature engineering
source("FeatureEngineering/DataEngineering.R")

# 2) Modeling (ensemble)
source("ModelDev/Predictions.R")

# 3) Ablation study (optional)
source("ModelDev/Predictions_without_LMM_derived_feature.R")

# 4) Benchmarking (requires test_data & ensemble_preds_weighted)
source("ModelDev/Benchmarking_.R")
```

---

## Outputs

- Engineered dataset with derived and geospatial features.  
- Cross-validated metrics (AUC, etc.).  
- Ensemble prediction tibble (`ensemble_preds_weighted`).  
- Optional benchmark table from sampled test cases.

---

## Reproducibility notes

- Fixed seeds (`set.seed(8)`) for deterministic sampling.  
- Parallel execution via `doParallel`/`future`.  
- Feature pruning at correlation > 0.9 (toggle inside script).

---

## Design rationale

- **Two-class outcome** simplifies downstream evaluation and interpretability.  
- **Temporal + spatial signals** explicitly engineered (financing lags, hub proximity).  
- **Transparent ablation** quantifies LLM-feature contribution using repeated CV.

---

## Citation

If you use this repository, please cite:

> Pfitzer, E. (2025). *Beyond Gut Feel: Predicting Outcomes of Digital Health Companies* GitHub: [estellecelinepfitzer/digital-health-outcome](https://github.com/estellecelinepfitzer/digital-health-outcome)

```bibtex
@misc{pfitzer2025digitalhealthoutcome,
  author       = {Estelle Pfitzer, Christoph Kausch, Tobias Kowatsch},
  title        = {Beyond Gut Feel: Predicting Outcomes of Digital Health Companies},
  year         = {2025},
  howpublished = {\url{https://github.com/estellecelinepfitzer/digital-health-outcome}},
  note         = {main branch, accessed YYYY-MM-DD}
}
```

---

## Ethical use & data licensing

- **Data:** Not included; ensure you have rights to any dataset you use.  
- **Code:** All rights reserved unless otherwise stated. Contact the author for reuse permissions.  
- **Responsible ML:** Validate external performance before applying in decision contexts.

---

## Maintainer

**Estelle Pfitzer** — initial author and maintainer  
For inquiries, open a GitHub Issue or contact directly.

---

