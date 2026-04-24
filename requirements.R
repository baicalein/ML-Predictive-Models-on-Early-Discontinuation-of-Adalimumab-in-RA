# requirements.R
# Install required R packages for the RA adalimumab discontinuation ML project

required_packages <- c(
  "tidymodels",
  "tidyverse",
  "discrim",
  "doParallel",
  "knitr",
  "dplyr",
  "ggcorrplot",
  "patchwork",
  "grid",
  "probably",
  "parallel",
  "yardstick",
  "tidyr",
  "xgboost",
  "finetune",
  "readxl",
  "ggplot2",
  "ranger",
  "glmnet",
  "kknn",
  "kernlab"
)

# Install packages that are not already installed
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

message("All required packages are installed and loaded.")
