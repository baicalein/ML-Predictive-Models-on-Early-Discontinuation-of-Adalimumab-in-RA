## ----hide-code, include=FALSE-----------------------------------------------------------------------
#| warning: False
#| message: False
knitr::opts_chunk$set(echo=FALSE)
knitr::opts_chunk$set(cache=TRUE, autodep=TRUE)
knitr::opts_chunk$set(fig.align="center", fig.pos="tbh")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
library(tidymodels)
library(tidyverse)
library(discrim)
library(doParallel)
library(knitr)
library(dplyr)
library(ggcorrplot)
library(patchwork)
library(grid)
library(probably)
library(parallel)
library(yardstick)
library(tidyr)
library(xgboost)
library(finetune)    


## ----setup-parallel---------------------------------------------------------------------------------
#| cache: FALSE
#| message: false
cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE))
doParallel::registerDoParallel(cl)


## ---------------------------------------------------------------------------------------------------
library(readxl)
data <- read_excel("datatable5.xlsx")


## ---------------------------------------------------------------------------------------------------
data<-data%>%
  mutate(across(c(class, sex, Humira, DzSeverity, serology, RF, ANA, CCP, swell,
                  erosion, BoneHealth, VitD, Infxn, mental, JointInjury,
                  SteroidHx, supple, Hr, FHx, alc, Insomnia, FMA, Auto, smoke), as.factor))

data$DzSeverity <- factor(data$DzSeverity,
                          levels = c("high", "moderate", "low", "remission"))

#rename levels in class
levels(data$class) <- c("regular", "high")  # original levels: "no", "yes"
data$class <- factor(data$class, levels = c("high", "regular"))

#check level
levels(data$class)
levels(data$DzSeverity)
levels(data$swell)


## ---------------------------------------------------------------------------------------------------
data <- data %>%
  mutate(QOLdz = case_when(
    !is.na(QOL) ~ QOL,  # Use original QOL if present
    
    DzSeverity == "remission" & pain < 1 ~ 5,
    DzSeverity == "remission" & pain >= 1 ~ 4,
    DzSeverity == "low" ~ 3,
    DzSeverity == "moderate" ~ 2,
    DzSeverity == "high" ~ 1,
    
    TRUE ~ NA_real_  # In case both QOL and DzSeverity are missing
  ))


## ---------------------------------------------------------------------------------------------------
summary(data)


## ---------------------------------------------------------------------------------------------------
#total number of missing value
sum(is.na(data))
# missing value rates
n_total <- nrow(data)
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing Count") %>%
  mutate(
    `Missing Percent (%)` = round(`Missing Count` / n_total * 100, 2)
  ) %>%
  arrange(desc(`Missing Percent (%)`))  # optional: sort by % missing

kable(
  missing_summary,
  caption = "Summary of Missing Values by Variable",
  digits = 2,
  align = "lrr")


## ---------------------------------------------------------------------------------------------------
#Humira: new, cot
data_humira <- data %>%
  group_by(Humira) %>%
  summarise(Counts=n()) %>%
  mutate(Percent=Counts/nrow(data))
data_humira
#Humira across class
humira_table1<-table(data$Humira, data$class)
humira_table1
#proportion by percent
round(prop.table(humira_table1, 1) * 100, 1)


## ---------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Humira, fill=class))+
  geom_bar(position = "stack") + #stack- by count
  labs(x="Humira Therapy Type", y="Count", title="Humira by Class (count)")
ggplot(data, aes(x=Humira, fill=class))+
  geom_bar(position = "fill") +
  labs(x="Humira Therapy Type", y="Proportion", title="Humira by Class (proportion)")
ggplot(data, aes(x=Humira, y=pain))+
  geom_boxplot(fill="Blue")+
  labs(title = "Pain by Humira Therapy Type")
ggplot(data, aes(x=Humira, y=age))+
  geom_boxplot(fill="Blue")+
  labs(title = "Age by Humira Therapy Type")
ggplot(data, aes(x=Humira, y=RAdue))+
  geom_boxplot(fill="Blue")+
  labs(title = "RA duration by Humira Therapy Type")


## ---------------------------------------------------------------------------------------------------
# Dz severity per class
data %>%
  group_by(DzSeverity, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(class) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = DzSeverity, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") + #fill(one bar) or dodge(parrell bars) for proportion
  labs(title = "Proportion of Dz Severity by class",
       y = "Proportion", x = "Dz severity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
ggplot(data, aes(x=DzSeverity, fill=class))+
  geom_bar(position = "stack") + #stack- by count
  labs(x="Dz Severity", y="Count", title="Dz Severity by Class (count)")
ggplot(data, aes(x=DzSeverity, fill=class))+
  geom_bar(position = "fill") +
  labs(x="Dz Severity", y="Proportion", title="Dz Severity by Class (proportion)")

## ---------------------------------------------------------------------------------------------------
ggplot(data, aes(x=DzSeverity, y=pain))+
  geom_boxplot(fill="Blue")+
  labs(title = "Pain by Disease Severity")


## ---------------------------------------------------------------------------------------------------
ggplot(data, aes(x=DzSeverity, fill=class))+
  geom_bar(position = "fill") +
  facet_wrap(~Humira) + 
  labs(y="Proportion", title = "Class and Dz Severity across Humira Therapy Type (new vs. COT)")


## ---------------------------------------------------------------------------------------------------
#Bar chart by count
bar_cat_by_class_count <- function(var1, var2, var3) {
  cat_vars <- c(var1, var2, var3)

  long_data <- data %>%
    dplyr::select(all_of(c(cat_vars, "class"))) %>%
    drop_na() %>%
    pivot_longer(cols = all_of(cat_vars), names_to = "Variable", values_to = "Category")

  plot_data <- long_data %>%
    group_by(Variable, Category, class) %>%
    summarise(count = n(), .groups = "drop")

  ggplot(plot_data, aes(x = Category, y = count, fill = class)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Variable, scales = "free_x") +
    labs(
      title = "Disctribution of Categorical Variables by Class (count)",
      x = NULL,
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


## ---------------------------------------------------------------------------------------------------
#Bar chart by proportion
bar_cat_by_class_proportion<- function(var1, var2, var3) {
  cat_vars <- c(var1, var2, var3)

  long_data <- data %>%
    dplyr::select(all_of(c(cat_vars, "class"))) %>%
    drop_na() %>%
    pivot_longer(cols = all_of(cat_vars), names_to = "Variable", values_to = "Category")

  plot_data <- long_data %>%
    group_by(Variable, Category, class) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(Variable, class) %>%
    mutate(proportion = count / sum(count))

  ggplot(plot_data, aes(x = Category, y = proportion, fill = class)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ Variable, scales = "free_x") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      title = "Distribution of Categorical variables by Class (proportion)",
      x = NULL,
      y = "Proportion"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("sex", "smoke", "serology")
bar_cat_by_class_proportion("sex", "smoke", "serology")


## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("swell", "erosion", "BoneHealth")
bar_cat_by_class_proportion("swell", "erosion", "BoneHealth")

## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("VitD", "Infxn", "mental")
bar_cat_by_class_proportion("VitD", "Infxn", "mental")

## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("JointInjury", "SteroidHx", "supple")
bar_cat_by_class_proportion("JointInjury", "SteroidHx", "supple")


## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("Hr", "FHx", "alc")
bar_cat_by_class_proportion("Hr", "FHx", "alc")


## ---------------------------------------------------------------------------------------------------
bar_cat_by_class_count("Insomnia", "FMA", "Auto")
bar_cat_by_class_proportion("Insomnia", "FMA", "Auto")


## ---------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
num_vars <- data %>%
  select(where(is.numeric)) %>%
  names()

long_data <- data %>%
  select(all_of(num_vars), class) %>%
  pivot_longer(cols = all_of(num_vars), names_to = "variable", values_to = "value")

ggplot(long_data, aes(x = class, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y", ncol = 4) +
  labs(title = "Distribution of Numeric Variables by Class",
       x = "Class", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## ---------------------------------------------------------------------------------------------------
long_data <- long_data %>%
  mutate(value = ifelse(variable %in% c("CRP", "Mstiff", "RAdue"),
                        log1p(value),
                        value))
ggplot(long_data, aes(x = class, y = value)) +
  geom_boxplot() +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Distribution of Numeric Variables by Class (Log-Transformed CRP, Mstiff, RAdue)",
       x = "Class", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


## ---------------------------------------------------------------------------------------------------
box_quant_by_class <- function(var1, var2, var3) {
  quant_vars <- c(var1, var2, var3)

  long_data <- data %>%
    dplyr::select(all_of(c(quant_vars, "class"))) %>%
    drop_na() %>%
    pivot_longer(cols = all_of(quant_vars), names_to = "Variable", values_to = "Value")

  ggplot(long_data, aes(x = class, y = Value, fill = class)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.2, color = "gray30", size = 0.5) +
    facet_wrap(~ Variable, scales = "free_y") +
    labs(
      title = "Comparison of Quantitative Variables by Class",
      x = "Class",
      y = "Value"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
}


## ---------------------------------------------------------------------------------------------------
box_quant_by_class("ALC", "WBC", "PLT")
box_quant_by_class("age", "RAdue", "pain")
box_quant_by_class("QOL", "Mstiff", "comorbid")


## ---------------------------------------------------------------------------------------------------
#Histogram of age
ggplot(data, aes(x=age))+
  geom_histogram(binwidth=2, fill="Blue", color="orange") +
  labs(
    title = "Distribution of Age",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()
#Histogram of age by class
ggplot(data, aes(x = age, fill = class)) +
  geom_histogram(binwidth=2, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of Age by Class",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()
#Density plot of age
ggplot(data, aes(x = age)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Age",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()
#Density plot of age by class
ggplot(data, aes(x = age, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Age by Class",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of age
ggplot(data, aes(x=RAdue))+
  geom_histogram(binwidth=1, fill="Blue", color="orange") +
  labs(
    title = "Distribution of RA duration",
    x = "RA duration",
    y = "Density"
  ) +
  theme_minimal()
#Histogram of RAdue by class
ggplot(data, aes(x = RAdue, fill = class)) +
  geom_histogram(binwidth=1, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of RA duration by Class",
    x = "Age",
    y = "Count"
  ) +
  theme_minimal()
#Density plot of RAdue
ggplot(data, aes(x = RAdue)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of RA Duration",
    x = "RA duration",
    y = "Density"
  ) +
  theme_minimal()
#Density plot of RAdue by class
ggplot(data, aes(x = RAdue, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of RA duration by Class",
    x = "RA duration",
    y = "Density"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of BMI
ggplot(data, aes(x=BMI))+
  geom_histogram(binwidth=2, fill="Blue", color="orange") +
  labs(
    title = "Distribution of BMI",
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal()
#Histogram of BMI by class
ggplot(data, aes(x = BMI, fill = class)) +
  geom_histogram(binwidth=2, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of BMI by Class",
    x = "BMI",
    y = "Count"
  ) +
  theme_minimal()
#Density plot of BMI
ggplot(data, aes(x = BMI)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of BMI",
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal()
#Density plot of BMI by class
ggplot(data, aes(x = BMI, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of BMI by Class",
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of pain
ggplot(data, aes(x=pain))+
  geom_histogram(binwidth=1, fill="Blue", color="orange") +
  labs(
    title = "Distribution of pain",
    x = "Pain",
    y = "Density"
  ) +
  theme_minimal()
#Histogram of pain by class
ggplot(data, aes(x = pain, fill = class)) +
  geom_histogram(binwidth=1, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of Pain by Class",
    x = "Pain",
    y = "Count"
  ) +
  theme_minimal()
#Density plot of Pain
ggplot(data, aes(x = pain)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Pain",
    x = "Pain",
    y = "Density"
  ) +
  theme_minimal()
#Density plot of Pain by class
ggplot(data, aes(x = pain, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Pain by Class",
    x = "Pain",
    y = "Density"
  ) +
  theme_minimal()

#Density plot of Pain by Dz Severity
ggplot(data, aes(x = pain, fill = DzSeverity)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Pain by Dz Severity",
    x = "Pain",
    y = "Density"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of Mstiff
ggplot(data, aes(x=Mstiff))+
  geom_histogram(binwidth=1, fill="Blue", color="orange") +
  labs(
    title = "Distribution of Morning Stiffness",
    x = "Morning Stiffness(hour)",
    y = "Density"
  ) +
  theme_minimal()
#Histogram of Morning Stiffness(hour) by class
ggplot(data, aes(x = Mstiff, fill = class)) +
  geom_histogram(binwidth=1, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of Morning Stiffness by Class",
    x = "Morning Stiffness(hour)",
    y = "Count"
  ) +
  theme_minimal()
#Density plot of Morning Stiffness
ggplot(data, aes(x = Mstiff)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Morning Stiffness",
    x = "Morning Stiffness(hour)",
    y = "Density"
  ) +
  theme_minimal()
#Density plot of Morning Stiffness by class
ggplot(data, aes(x = Mstiff, fill = class)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Morning Stiffness by Class",
    x = "Morning Stiffness(hour)",
    y = "Density"
  ) +
  theme_minimal()

#Density plot of Pain by Dz Severity
ggplot(data, aes(x = Mstiff, fill = DzSeverity)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Density Plot of Morning Stiffness(hour) by Dz Severity",
    x = "Morning Stiffness(hour)",
    y = "Density"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of pain by class
ggplot(data, aes(x = nonbio_co, fill = class)) +
  geom_histogram(binwidth=1, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of nonbio_co by Class",
    x = "Concommitant nonbiologic DMARDs",
    y = "Count"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
#Histogram of Prior non-biologic DMARDs by class
ggplot(data, aes(x = nonbio_prior, fill = class)) +
  geom_histogram(binwidth=1, position = "identity", alpha = 0.6, color="orange") +
  labs(
    title = "Distribution of nonbio_co by Class",
    x = "Prior nonbiologic DMARDs",
    y = "Count"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
library(ggcorrplot)
corr<-data %>%
  select(where(is.numeric)) %>%
  cor()
ggcorrplot(corr)


## ---------------------------------------------------------------------------------------------------
corr2 <-data %>%
  select(age, RAdue, pain, BMI, Mstiff, nonbio_prior, nonbio_co, comorbid, QOLdz, QOL) %>%
  cor()
ggcorrplot(round(corr2, 2), lab = TRUE)


## ---------------------------------------------------------------------------------------------------
#Age vs. RAdue
ggplot(data, aes(x=age, y=RAdue, color=class)) +
  geom_point(alpha=0.5) +
  labs(x="Age (in year)", y="RA duration (in year)", title="Scatterplot of RA duration against Age")
#Age vs. Comorbidity
ggplot(data, aes(x=age, y=comorbid, color=class)) +
  geom_point(alpha=0.5) +
  labs(x="Age (in year)", y="Number of Comorbidity Dz", title="Scatterplot of Comorbidity against Age")


## ---------------------------------------------------------------------------------------------------
#Mstiff vs. pain
ggplot(data, aes(x=Mstiff, y=pain, color=class)) +
  geom_point(alpha=0.5) +
  labs(x="Morning Stiffness (in hour)", y="Pain", title="Scatterplot of Morning Stiffness against Pain")


## ---------------------------------------------------------------------------------------------------
set.seed(123)
initial_split <- initial_split(data, prop=0.8, strata=class)
train <- training(initial_split)
test <- testing(initial_split)


## ---------------------------------------------------------------------------------------------------
# check number of unique values per variable
train %>%
  group_by(class) %>%
  summarise(across(everything(), ~ n_distinct(.)), .groups = "drop")


## ---------------------------------------------------------------------------------------------------
# Check for predictors constant within class
train %>%
  select(all_of(c("class", "age", "sex", "Humira", "RAdue", "pain", "BMI", "serology", "swell", "Mstiff", "nonbio_prior", "nonbio_co", "comorbid", "BoneHealth", "Infxn", "mental", "JointInjury", "Insomnia", "FMA", "Auto"))) %>%
  group_by(class) %>%
  summarise(across(everything(), ~ n_distinct(.)), .groups = "drop") %>%
  summarise(across(everything(), ~ any(. == 1))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "is_constant") %>%
  filter(is_constant)


## ---------------------------------------------------------------------------------------------------
formula <- class ~ age + sex + Humira + RAdue + pain + BMI + serology + swell + Mstiff + nonbio_prior + nonbio_co + comorbid + BoneHealth + Infxn + mental + JointInjury + Insomnia + FMA + Auto

recipe<-recipe(formula, data = train) %>%
  step_impute_mode(all_nominal_predictors()) %>%  # impute missing categorical vars to mode
  step_impute_median(all_numeric_predictors()) %>% # impute missing numeric vars to median
  step_zv(all_predictors(), id = "zv_cleanup") #Removes zero variance (constant) predictors

# check removed column
rec_prep <- prep(recipe, training = train)
tidy(rec_prep, id = "zv_cleanup") #no column's removed: safeguard


## ---------------------------------------------------------------------------------------------------
logreg_recipe <- recipe %>% step_dummy(all_nominal_predictors())

logreg_spec <- logistic_reg(mode="classification", engine="glm")
lda_spec <- discrim_linear(mode="classification", engine="MASS")

logreg_wf <- workflow() %>%
    add_recipe(logreg_recipe) %>%
    add_model(logreg_spec)

lda_wf <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(lda_spec)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
resamples <- vfold_cv(train, v=10, strata=class)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
custom_metrics <- metric_set(accuracy, recall, precision, f_meas, roc_auc, pr_auc)
cv_control <- control_resamples(save_pred=TRUE)

logreg_cv <- fit_resamples(logreg_wf, resamples, metrics=custom_metrics, control=cv_control)
lda_cv <- fit_resamples(lda_wf, resamples, metrics=custom_metrics, control=cv_control)


## ---------------------------------------------------------------------------------------------------
logreg_cv_metrics <- collect_metrics(logreg_cv) %>% mutate(model = "Logistic Regression")
lda_cv_metrics <- collect_metrics(lda_cv) %>% mutate(model = "LDA")
logreg_lda_cv_comparison <- bind_rows(logreg_cv_metrics,lda_cv_metrics)%>% 
  dplyr::select(model, .metric, mean) %>%
  tidyr::pivot_wider(names_from = .metric, values_from = mean)
logreg_lda_cv_comparison


## ---------------------------------------------------------------------------------------------------
#Utility function: ROC curve
roc_cv_data <- function(model_cv, label) {
  collect_predictions(model_cv) %>%
    roc_curve(truth = class, .pred_high, event_level = "first") %>%
    mutate(model = label)}
bind_rows(
  roc_cv_data(logreg_cv, "Logistic Regression"),
  roc_cv_data(lda_cv, "LDA")) %>%
ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
labs(title = "ROC Curves on Cross validation(Logistic Regression, LDA", x = "1-specificity", y = "Sensitivity(Recall)")


## ---------------------------------------------------------------------------------------------------
# Utility function: PR curve
pr_cv_data <- function(model_cv, label) {
  collect_predictions(model_cv) %>%
    pr_curve(truth = class, .pred_high, event_level = "first") %>%
    mutate(model = label)}
bind_rows(
  pr_cv_data(logreg_cv, "Logisc Regression"),
  pr_cv_data(lda_cv, "LDA")) %>%
ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR curves on Cross validation set: Logistic Regression, LDA", x = "Sensitivity(Recall)", y = "Precision")


## ---------------------------------------------------------------------------------------------------
logreg_cv_preds <- collect_predictions(logreg_cv)%>%mutate(model = "Logistic Regression")
lda_cv_preds <- collect_predictions(lda_cv)%>% mutate(model = "LDA")
# LogReg
threshold_f1_logreg_cv <- logreg_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )
# LDA
threshold_f1_lda_cv <- lda_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
#LogReg
best_threshold_logreg_cv <- threshold_f1_logreg_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_logreg <- best_threshold_logreg_cv$.threshold[1]
best_threshold_logreg_cv

#LDA
best_threshold_lda_cv <- threshold_f1_lda_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_lda <- best_threshold_lda_cv$.threshold[1]
best_threshold_lda_cv


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
logreg_model <- fit(logreg_wf, data=train)
lda_model <- fit(lda_wf, data=train)

logreg_test_preds <- predict(logreg_model, test, type = "prob") %>%
  bind_cols(predict(logreg_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "Logistic Regression")

lda_test_preds <- predict(lda_model, test, type = "prob") %>%
  bind_cols(predict(lda_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "LDA")


## ---------------------------------------------------------------------------------------------------
logreg_test_opt <- logreg_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_logreg, "high", "regular") %>%
           factor(levels = levels(class)))

lda_test_opt <- lda_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_lda, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
#Logistic regression
logreg_test_metrics <- custom_metrics(
  data = logreg_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
logreg_cv_table <- logreg_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

logreg_test_table <- logreg_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

logreg_comparison <- bind_rows(logreg_cv_table, logreg_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)


## ---------------------------------------------------------------------------------------------------
#LDA
lda_test_metrics <- custom_metrics(
  data = lda_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
lda_cv_table <- lda_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

lda_test_table <- lda_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

lda_comparison <- bind_rows(lda_cv_table, lda_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)


## ---------------------------------------------------------------------------------------------------
logreg_comparison <- logreg_comparison %>% mutate(model = "Logistic Regression")
logreg_comparison
lda_comparison <- lda_comparison %>% mutate(model = "LDA")
lda_comparison
logreg_lda_comparison <- bind_rows(logreg_comparison, lda_comparison) %>%
  relocate(model, .before = set)
#logreg_lda_comparison


## ---------------------------------------------------------------------------------------------------
logreg_lda_f1_auc_plot_data <- logreg_lda_comparison %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_f1_auc_plot_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  facet_wrap(~ metric, scales = "free_y") +
  labs(title = "F1 Score and ROC AUC: CV vs Test (LogReg, LDA)",
       y = "Metric Value", x = "Model", fill = "Dataset") +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


## ---------------------------------------------------------------------------------------------------
elastic_net_recipe <- recipe %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

elastic_net_spec <- logistic_reg(penalty = tune(),mixture = tune()) %>% set_engine("glmnet") %>%
  set_mode("classification")

elastic_net_wf <- workflow() %>% add_model(elastic_net_spec) %>% add_recipe(elastic_net_recipe)


## ---------------------------------------------------------------------------------------------------
#| warning: FALSE
set.seed(123)
#parameters_elastic <- extract_parameter_set_dials(elastic_net_wf)
parameters_elastic <- parameters(
  penalty(range = c(-4, 0), trans = log10_trans()),
  mixture(range = c(0, 1)))

elastic_net_tune <- tune_bayes(
  elastic_net_wf,
  resamples = resamples,
  metrics = custom_metrics,
  param_info = parameters_elastic,
  control = control_bayes(save_pred = TRUE),
  iter = 10)

parameters_elastic

## ---------------------------------------------------------------------------------------------------
#metrics by parameters
elastic_net_tune_metrics <- collect_metrics(elastic_net_tune)
elastic_net_tune_metrics%>%
  select(penalty, mixture, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "Elastic Net CV Metrics by Parameters", digits = 3)
#best parameter on roc_auc
select_best(elastic_net_tune, metric="roc_auc")
#plot 
autoplot(elastic_net_tune, metric = "roc_auc")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
best_elastic_net_wf <- elastic_net_wf %>%
    finalize_workflow(select_best(elastic_net_tune, metric="roc_auc"))
elastic_net_cv <- fit_resamples(best_elastic_net_wf, resamples, metrics=custom_metrics, control=control_resamples(save_pred = TRUE))


## ---------------------------------------------------------------------------------------------------
elastic_net_cv_metrics <- collect_metrics(elastic_net_cv) %>% mutate(model = "Elastic Net")

elastic_net_cv_metrics %>%
  select(model, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)


## ---------------------------------------------------------------------------------------------------
roc_cv_data(elastic_net_cv, "Elastic Net") %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
labs(title = "ROC Curves on Cross validation(Elastic Net)", x = "1-Specificity", y = "Sensitivity(Recall)")


## ---------------------------------------------------------------------------------------------------
  pr_cv_data(logreg_cv, "Elastic Net") %>%
    ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR curves on Cross validation set: Elastic Net Logistic Regression", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
# threshold selection
elastic_net_cv_preds <- collect_predictions(elastic_net_cv) %>% mutate(model = "Elastic Net")

threshold_f1_elastic_net_cv <- elastic_net_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
# best threshold, fit it to model
best_threshold_elastic_net_cv <- threshold_f1_elastic_net_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_elastic_net <- best_threshold_elastic_net_cv$.threshold[1]
best_threshold_elastic_net_cv


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
elastic_net_model <- fit(best_elastic_net_wf, data=train)

elastic_net_test_preds <- predict(elastic_net_model, test, type = "prob") %>%
  bind_cols(predict(logreg_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "Elastic Net")


## ---------------------------------------------------------------------------------------------------
elastic_net_test_opt <- elastic_net_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_elastic_net, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
elastic_net_test_metrics <- custom_metrics(
  data = elastic_net_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
elastic_net_cv_table <- elastic_net_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

elastic_net_test_table <- elastic_net_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

elastic_net_comparison <- bind_rows(elastic_net_cv_table, elastic_net_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)
elastic_net_comparison


## ---------------------------------------------------------------------------------------------------
elastic_net_f1_auc_plot_data <- elastic_net_comparison %>%
  select(set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")


## ---------------------------------------------------------------------------------------------------
# Plot 
ggplot(elastic_net_f1_auc_plot_data, aes(x = metric, y = value, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cross-Validation" = "#F8766D", "Test" = "#00BFC4")) +
  labs(
    title = "F1 Score and ROC AUC: CV vs Test (Elastic Net)",
    x = "Metric",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
# bind elastic net to the existing data
logreg_lda_elasticnet_f1_auc_data <- bind_rows(
  logreg_lda_comparison,          # previous comparison table with LogReg and LDA
  elastic_net_comparison %>%      # add elastic net model
    mutate(model = "Elastic Net")
)

logreg_lda_elasticnet_f1_auc_data <- logreg_lda_elasticnet_f1_auc_data %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_elasticnet_f1_auc_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "F1 Score and ROC AUC: CV vs Test (LogReg, LDA, Elastic Net)",
       y = "Metric Value", x = "Model") +
  scale_fill_discrete(name = "Dataset") +
  facet_grid(~ metric)  


## ---------------------------------------------------------------------------------------------------
#recipe: no need to preprocess more
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
params_rf <- hardhat::extract_parameter_set_dials(rf_wf)
params_rf <- dials::finalize(params_rf, train)

rf_tune <- tune_bayes(
  rf_wf,
  resamples = resamples,
  metrics = custom_metrics,
  param_info = params_rf,
  control = control_bayes(save_pred = TRUE),
  iter = 10
)


## ---------------------------------------------------------------------------------------------------
rf_tune_metrics <- collect_metrics(rf_tune)
rf_tune_metrics %>%
  select(mtry, min_n, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "Random Forest CV Metrics by Parameters", digits = 3)
select_best(rf_tune, metric="roc_auc")
autoplot(rf_tune, metric = "roc_auc")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
best_rf_wf <- rf_wf %>%
    finalize_workflow(select_best(rf_tune, metric="roc_auc"))
rf_cv <- fit_resamples(best_rf_wf, resamples, metrics=custom_metrics, control=control_resamples(save_pred = TRUE))


## ---------------------------------------------------------------------------------------------------
rf_cv_metrics <- collect_metrics(rf_cv) %>% mutate(model = "Random Forest")

rf_cv_metrics %>%
  select(model, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)


## ---------------------------------------------------------------------------------------------------
roc_cv_data(rf_cv, "Random Forest") %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
labs(title = "ROC Curves on Cross validation(Random Forest)", x = "1-Specificity", y = "Sensitivity(Recall)")


## ---------------------------------------------------------------------------------------------------
 pr_cv_data(rf_cv, "Random Forest") %>%
    ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR curves on Cross validation set: Random Forest", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
# threshold selection
rf_cv_preds <- collect_predictions(rf_cv) %>% mutate(model = "Random Forest")

threshold_f1_rf_cv <- rf_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
# best threshold, fit it to model
best_threshold_rf_cv <- threshold_f1_rf_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_rf <- best_threshold_rf_cv$.threshold[1]
best_threshold_rf_cv


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
#fit tuned model
rf_model <- fit(best_rf_wf, data=train)
#fit test data to the tuned model
rf_test_preds <- predict(rf_model, test, type = "prob") %>%
  bind_cols(predict(rf_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "Random Forest")


## ---------------------------------------------------------------------------------------------------
#best threshold to model
rf_test_opt <- rf_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_rf, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
# metrics on test
rf_test_metrics <- custom_metrics(
  data = rf_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
#combine metrics of test and CV
rf_cv_table <- rf_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

rf_test_table <- rf_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

rf_comparison <- bind_rows(rf_cv_table, rf_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)
rf_comparison


## ---------------------------------------------------------------------------------------------------
#prepare data for plot
rf_f1_auc_plot_data <- rf_comparison %>%
  select(set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")


## ---------------------------------------------------------------------------------------------------
# Plot 
ggplot(rf_f1_auc_plot_data, aes(x = metric, y = value, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cross-Validation" = "#F8766D", "Test" = "#00BFC4")) +
  labs(
    title = "F1 Score and ROC AUC: CV vs Test (Random Forest)",
    x = "Metric",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
# bind random forest to the existing data
logreg_lda_elasticnet_rf_f1_auc_data <- bind_rows(
  logreg_lda_comparison,          # comparison table with LogReg and LDA
  elastic_net_comparison %>%
    mutate(model= "Elastic Net"), # add elastic net table
  rf_comparison %>%      # add random forest table
    mutate(model = "Random Forest")
)

logreg_lda_elasticnet_rf_f1_auc_data <- logreg_lda_elasticnet_rf_f1_auc_data %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_elasticnet_rf_f1_auc_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "F1 Score and ROC AUC: CV vs Test (LogReg, LDA, Elastic Net, Random Forest)",
       y = "Metric Value", x = "Model") +
  scale_fill_discrete(name = "Dataset") +
  facet_grid(~ metric)  


## ---------------------------------------------------------------------------------------------------
knn_recipe <- recipe %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
knn_grid <- grid_regular(neighbors(range = c(1, 50)), levels = 10)
# Gaussian
knn_spec_gaus <- nearest_neighbor(mode = "classification", neighbors = tune(), weight_func = "gaussian") %>%
  set_engine("kknn")
knn_wf_gaus <- workflow() %>% add_recipe(knn_recipe) %>% add_model(knn_spec_gaus)
knn_tune_gaus <- tune_grid(knn_wf_gaus, resamples = resamples, control = control_resamples(save_pred = TRUE),  grid = knn_grid, metrics = custom_metrics)

# Rectangular
knn_spec_rect <- nearest_neighbor(mode = "classification", neighbors = tune(), weight_func = "rectangular") %>%
  set_engine("kknn")
knn_wf_rect <- workflow() %>% add_recipe(knn_recipe) %>% add_model(knn_spec_rect)
knn_tune_rect <- tune_grid(knn_wf_rect, resamples = resamples, control = control_resamples(save_pred = TRUE), grid = knn_grid, metrics = custom_metrics)

# Optimal
knn_spec_opt <- nearest_neighbor(mode = "classification", neighbors = tune(), weight_func = "optimal") %>%
  set_engine("kknn")
knn_wf_opt <- workflow() %>% add_recipe(knn_recipe) %>% add_model(knn_spec_opt)
knn_tune_opt <- tune_grid(knn_wf_opt, resamples = resamples, control = control_resamples(save_pred = TRUE),  grid = knn_grid, metrics = custom_metrics)


## ---------------------------------------------------------------------------------------------------
set.seed(123)
# Gaussian 
knn_tune_metrics_gaus <- collect_metrics(knn_tune_gaus)
knn_tune_metrics_gaus %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "KNN Gaussian function model CV Metrics by Parameters", digits = 3)
#Rectangular
knn_tune_metrics_rect <- collect_metrics(knn_tune_rect)
knn_tune_metrics_rect %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "KNN Rectangular function model CV Metrics by Parameters", digits = 3)
#Optimal
knn_tune_metrics_opt <- collect_metrics(knn_tune_opt)
knn_tune_metrics_opt %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "KNN Optimal function model CV Metrics by Parameters", digits = 3)


## ---------------------------------------------------------------------------------------------------
library(patchwork)
autoplot(knn_tune_gaus) + 
autoplot(knn_tune_rect) + 
autoplot(knn_tune_opt)


## ---------------------------------------------------------------------------------------------------
# Tuning selection based on roc_auc
# Extract best number of neighbors by roc_auc for each weight function
best_gaus <- knn_tune_metrics_gaus %>%
  filter(.metric == "roc_auc") %>%
  slice_max(mean, n = 1) %>%
  mutate(weight_func = "gaussian")

best_rect <- knn_tune_metrics_rect %>%
  filter(.metric == "roc_auc") %>%
  slice_max(mean, n = 1) %>%
  mutate(weight_func = "rectangular")

best_opt <- knn_tune_metrics_opt %>%
  filter(.metric == "roc_auc") %>%
  slice_max(mean, n = 1) %>%
  mutate(weight_func = "optimal")

knn_best_roc_auc <- bind_rows(best_gaus, best_rect, best_opt) %>%
  select(weight_func, neighbors, mean) %>%
  rename(roc_auc = mean)
knitr::kable(knn_best_roc_auc, caption = "Best ROC AUC by Weight Function")

# Tuning selection based on roc_auc vs. based on the average of roc_auc/f_meas
# Gaussian
knn_best_gaus <- knn_tune_metrics_gaus %>%
  filter(.metric %in% c("f_meas", "roc_auc")) %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(avg_score = (f_meas + roc_auc) / 2) %>%
  slice_max(avg_score, n = 1) %>%
  mutate(weight_func = "gaussian")

# Rectangular
knn_best_rect <- knn_tune_metrics_rect %>%
  filter(.metric %in% c("f_meas", "roc_auc")) %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(avg_score = (f_meas + roc_auc) / 2) %>%
  slice_max(avg_score, n = 1) %>%
  mutate(weight_func = "rectangular")

# Optimal
knn_best_opt <- knn_tune_metrics_opt %>%
  filter(.metric %in% c("f_meas", "roc_auc")) %>%
  select(neighbors, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(avg_score = (f_meas + roc_auc) / 2) %>%
  slice_max(avg_score, n = 1) %>%
  mutate(weight_func = "optimal")

# Combine all into one summary table
knn_best_summary <- bind_rows(knn_best_gaus, knn_best_rect, knn_best_opt) %>%
  select(weight_func, neighbors, f_meas, roc_auc, avg_score)

# Show table
knitr::kable(knn_best_summary, caption = "Best Average Score (f_meas & roc_auc) by Weight Function", digits = 3)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
select_best(knn_tune_rect, metric="roc_auc")
best_knn_wf <- finalize_workflow(knn_wf_rect, select_best(knn_tune_rect, metric="roc_auc")) #K=50
knn_cv <- fit_resamples(best_knn_wf,resamples = resamples,metrics = custom_metrics,control = control_resamples(save_pred = TRUE))


## ---------------------------------------------------------------------------------------------------
knn_cv_metrics <- collect_metrics(knn_cv) %>% mutate(model = "KNN")

knn_cv_metrics %>%
  select(model, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)


## ---------------------------------------------------------------------------------------------------
roc_cv_data(knn_cv, "KNN") %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
labs(title = "ROC Curves on Cross validation(KNN)", x = "1-Specificity", y = "Sensitivity(Recall)")


## ---------------------------------------------------------------------------------------------------
 pr_cv_data(knn_cv, "KNN") %>%
    ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR curves on Cross validation set: KNN", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
# threshold selection
knn_cv_preds <- collect_predictions(knn_cv) %>% mutate(model = "KNN")

threshold_f1_knn_cv <- knn_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
# best threshold, fit it to model
best_threshold_knn_cv <- threshold_f1_knn_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_knn <- best_threshold_knn_cv$.threshold[1]
best_threshold_knn_cv
best_thresh_value_knn


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
#fit tuned model
knn_model <- fit(best_knn_wf, data=train)
#fit test data to the tuned model
knn_test_preds <- predict(knn_model, test, type = "prob") %>%
  bind_cols(predict(rf_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "KNN")


## ---------------------------------------------------------------------------------------------------
#best threshold to model
knn_test_opt <- knn_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_rf, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
# metrics on test
knn_test_metrics <- custom_metrics(
  data = knn_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
#combine metrics of test and CV
knn_cv_table <- knn_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

knn_test_table <- knn_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

knn_comparison <- bind_rows(knn_cv_table, knn_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)
knn_comparison


## ---------------------------------------------------------------------------------------------------
#prepare data for plot
knn_f1_auc_plot_data <- knn_comparison %>%
  select(set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")


## ---------------------------------------------------------------------------------------------------
# Plot 
ggplot(knn_f1_auc_plot_data, aes(x = metric, y = value, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cross-Validation" = "#F8766D", "Test" = "#00BFC4")) +
  labs(
    title = "F1 Score and ROC AUC: CV vs Test (KNN)",
    x = "Metric",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
# bind random forest to the existing data
logreg_lda_elasticnet_rf_knn_f1_auc_data <- bind_rows(
  logreg_lda_comparison,          # comparison table with LogReg and LDA
  elastic_net_comparison %>%
    mutate(model= "Elastic Net"), # add elastic net table
  rf_comparison %>%      # add random forest table
    mutate(model = "Random Forest"),
  knn_comparison %>%
    mutate(model = "KNN") # add KNN table
)

logreg_lda_elasticnet_rf_knn_f1_auc_data <- logreg_lda_elasticnet_rf_knn_f1_auc_data %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_elasticnet_rf_knn_f1_auc_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "F1 Score and ROC AUC: CV vs Test (LogReg, LDA, Elastic Net, Random Forest, KNN)",
       y = "Metric Value", x = "Model") +
  scale_fill_discrete(name = "Dataset") +
  facet_grid(~ metric)  


## ---------------------------------------------------------------------------------------------------
svm_recipe <- recipe%>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())


## ---------------------------------------------------------------------------------------------------
#SVM-linear kernel
svm_linear_spec <- svm_linear(cost=tune(), margin=tune()) %>% set_engine("kernlab") %>% set_mode("classification")
svm_linear_wf <- workflow() %>% add_recipe(svm_recipe) %>% add_model(svm_linear_spec)
parameter_svm_linear<- extract_parameter_set_dials(svm_linear_wf)
parameter_svm_linear


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
tune_svm_linear<-tune_bayes(svm_linear_wf, resamples=resamples,metrics=custom_metrics,
                            param_info = parameter_svm_linear, iter = 10)


## ---------------------------------------------------------------------------------------------------
svm_poly_spec <- svm_poly(cost = tune(),margin = tune(), degree = tune()) %>% set_engine("kernlab") %>%
  set_mode("classification")
svm_poly_wf <- workflow() %>% add_recipe(svm_recipe) %>% add_model(svm_poly_spec)
parameters_svm_poly <- extract_parameter_set_dials(svm_poly_wf) %>% update(degree = degree_int(range = c(2, 5)))
parameters_svm_poly

## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
tune_svm_poly <- tune_bayes(svm_poly_wf, resamples=resamples, metrics=custom_metrics, param_info=parameters_svm_poly, iter= 10)


## ---------------------------------------------------------------------------------------------------
svm_rbf_spec <- svm_rbf(cost = tune(),margin = tune(),rbf_sigma = tune()) %>%set_engine("kernlab") %>%
  set_mode("classification")
svm_rbf_wf <- workflow() %>% add_recipe(svm_recipe) %>% add_model(svm_rbf_spec)
parameters_svm_rbf <- extract_parameter_set_dials(svm_rbf_wf) %>%update(rbf_sigma = rbf_sigma(range = c(-4, 0)))
parameters_svm_rbf

## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
tune_svm_rbf <- tune_bayes(svm_rbf_wf, resamples=resamples,metrics=custom_metrics,
    param_info=parameters_svm_rbf, iter=10)


## ---------------------------------------------------------------------------------------------------
# SVM linear model 
svm_linear_tune_metrics <- collect_metrics(tune_svm_linear)
svm_linear_tune_metrics %>%
  select(cost, margin, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "SVM linear model CV Metrics by Parameters", digits = 3)
# SVM polynomial model
svm_poly_tune_metrics <- collect_metrics(tune_svm_poly)
svm_poly_tune_metrics %>%
  select(cost, margin, degree, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "SVM polynomial model CV Metrics by Parameters", digits = 3)
# SVM RBF model
svm_rbf_tune_metrics <- collect_metrics(tune_svm_rbf)
svm_rbf_tune_metrics %>%
  select(cost, margin, rbf_sigma, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "SVM RBF model CV Metrics by Parameters", digits = 3)


## ---------------------------------------------------------------------------------------------------
library(patchwork)
autoplot(tune_svm_linear) + 
autoplot(tune_svm_poly) + 
autoplot(tune_svm_rbf)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
select_best(tune_svm_linear, metric="roc_auc")
best_svm_wf <- finalize_workflow(svm_linear_wf, select_best(tune_svm_linear, metric="roc_auc"))
svm_cv <- fit_resamples(best_svm_wf, resamples = resamples,metrics = custom_metrics,control = control_resamples(save_pred = TRUE))


## ---------------------------------------------------------------------------------------------------
svm_cv_metrics <- collect_metrics(svm_cv) %>% mutate(model = "SVM")

svm_cv_metrics %>%
  select(model, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)


## ---------------------------------------------------------------------------------------------------
roc_cv_data(svm_cv, "SVM") %>% 
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "ROC Curve on Cross validation set: SVM", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
  pr_cv_data(svm_cv, "SVM") %>%
  ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR cure on Cross validation set: SVM", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
svm_cv_preds <- collect_predictions(svm_cv) %>% mutate(model = "SVM")
# threshold selection
threshold_f1_svm_cv <- svm_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
best_threshold_svm_cv <- threshold_f1_svm_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_svm <- best_threshold_svm_cv$.threshold[1]
best_threshold_svm_cv


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
#fit tuned model
svm_model <- fit(best_svm_wf, data=train)
#fit test data to the tuned model
svm_test_preds <- predict(svm_model, test, type = "prob") %>%
  bind_cols(predict(svm_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "SVM")


## ---------------------------------------------------------------------------------------------------
#best threshold to model
svm_test_opt <- svm_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_svm, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
# metrics on test
svm_test_metrics <- custom_metrics(
  data = svm_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
#combine metrics of test and CV
svm_cv_table <- svm_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

svm_test_table <- svm_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

svm_comparison <- bind_rows(svm_cv_table, svm_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)
svm_comparison


## ---------------------------------------------------------------------------------------------------
#prepare data for plot
svm_f1_auc_plot_data <- svm_comparison %>%
  select(set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")


## ---------------------------------------------------------------------------------------------------
# Plot 
ggplot(svm_f1_auc_plot_data, aes(x = metric, y = value, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cross-Validation" = "#F8766D", "Test" = "#00BFC4")) +
  labs(
    title = "F1 Score and ROC AUC: CV vs Test (SVM)",
    x = "Metric",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
# bind random forest to the existing data
logreg_lda_elasticnet_rf_knn_svm_f1_auc_data <- bind_rows(
  logreg_lda_comparison,          # comparison table with LogReg and LDA
  elastic_net_comparison %>%
    mutate(model= "Elastic Net"), # add elastic net table
  rf_comparison %>%      # add random forest table
    mutate(model = "Random Forest"),
  knn_comparison %>%
    mutate(model = "KNN"), #add KNN table
  svm_comparison %>%
    mutate(model = "SVM") # add KNN table
)

logreg_lda_elasticnet_rf_knn_svm_f1_auc_data <- logreg_lda_elasticnet_rf_knn_svm_f1_auc_data %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_elasticnet_rf_knn_svm_f1_auc_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "F1 Score,ROC AUC:CV vs Test(All models, 19 features)",
       y = "Metric Value", x = "Model") +
  scale_fill_discrete(name = "Dataset") +
  facet_grid(~ metric)  


## ---------------------------------------------------------------------------------------------------
xgb_recipe <- recipe %>% step_dummy(all_nominal_predictors())


## ---------------------------------------------------------------------------------------------------
xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  learn_rate = tune(),
  mtry = tune(),
  min_n = tune(),
  loss_reduction = tune(),   # gamma
  sample_size = tune()       # subsample ratio
) %>% 
  set_engine("xgboost") %>%
  set_mode("classification")

xgb_wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(xgb_recipe)


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
# Define tuning range
parameters_xgb <- parameters(xgb_spec) %>%
  update(
    tree_depth = tree_depth(c(3, 10)),
    learn_rate = learn_rate(range = c(-4, -1)),  # 1e-4 to 1e-1
    mtry = finalize(mtry(), train),
    min_n = min_n(c(2, 20)),
    loss_reduction = loss_reduction(c(0, 10)),
    sample_size = sample_prop(c(0.5, 1.0)))

# Bayesian tuning
set.seed(123)
xgb_tune <- tune_bayes(xgb_wf, resamples=resamples, metrics=custom_metrics,
    param_info=parameters_xgb, control = control_bayes(save_pred = TRUE, verbose = TRUE), initial=12, iter=25)


## ---------------------------------------------------------------------------------------------------
#metrics by parameters
xgb_tune_metrics <- collect_metrics(xgb_tune)
xgb_tune_metrics%>%
  select(mtry, min_n, tree_depth, learn_rate, loss_reduction, sample_size, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  knitr::kable(caption = "XGboost CV Metrics by Parameters", digits = 3)
#best parameter on roc_auc
select_best(xgb_tune, metric="roc_auc")
#plot 
autoplot(xgb_tune, metric = "roc_auc")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
best_xgb_wf <- xgb_wf %>%
    finalize_workflow(select_best(xgb_tune, metric="roc_auc"))
xgb_cv <- fit_resamples(best_xgb_wf, resamples, metrics=custom_metrics, control=control_resamples(save_pred = TRUE))


## ---------------------------------------------------------------------------------------------------
xgb_cv_metrics <- collect_metrics(xgb_cv) %>% mutate(model = "XGboost")

xgb_cv_metrics %>%
  select(model, .metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean)


## ---------------------------------------------------------------------------------------------------
roc_cv_data(xgb_cv, "XGBoost") %>%
  ggplot(aes(x = 1-specificity, y = sensitivity, color = model)) +
  geom_line() +
  theme_minimal() +
labs(title = "ROC Curves on Cross validation(XGboost)", x = "1-Specificity", y = "Sensitivity(Recall)")


## ---------------------------------------------------------------------------------------------------
 pr_cv_data(xgb_cv, "XGBoost") %>%
    ggplot(aes(x = recall, y = precision, color = model)) +
  geom_line() +
  theme_minimal() +
  labs(title = "PR curves on Cross validation set: XGboost", x = "Recall", y = "Precision")


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
# threshold selection
xgb_cv_preds <- collect_predictions(xgb_cv) %>% mutate(model = "XGboost")

threshold_f1_xgb_cv <- xgb_cv_preds %>%
  threshold_perf(
    truth = class,
    estimate = .pred_high,
    thresholds = seq(0.05, 0.95, by = 0.01),
    metrics = metric_set(f_meas)
  )


## ---------------------------------------------------------------------------------------------------
# best threshold, fit it to model
best_threshold_xgb_cv <- threshold_f1_xgb_cv %>%
  slice_max(order_by = .estimate, n = 1)
best_thresh_value_xgb <- best_threshold_xgb_cv$.threshold
best_threshold_xgb_cv


## ---------------------------------------------------------------------------------------------------
#| message: FALSE
#| warning: FALSE
set.seed(123)
#fit tuned model
xgb_model <- fit(best_xgb_wf, data=train)
#fit test data to the tuned model
xgb_test_preds <- predict(xgb_model, test, type = "prob") %>%
  bind_cols(predict(xgb_model, test)) %>%
  bind_cols(test %>% select(class)) %>%
  mutate(model = "XGBoost")


## ---------------------------------------------------------------------------------------------------
#best threshold to model
xgb_test_opt <- xgb_test_preds %>%
  mutate(pred_optimal = if_else(.pred_high >= best_thresh_value_xgb, "high", "regular") %>%
           factor(levels = levels(class)))


## ---------------------------------------------------------------------------------------------------
# metrics on test
xgb_test_metrics <- custom_metrics(
  data = xgb_test_opt,
  truth = class,
  estimate = pred_optimal,
  .pred_high
)
#combine metrics of test and CV
xgb_cv_table <- xgb_cv_metrics %>%
  select(.metric, mean) %>%
  pivot_wider(names_from = .metric, values_from = mean) %>%
  mutate(set = "Cross-Validation")

xgb_test_table <- xgb_test_metrics %>%
  select(.metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  mutate(set = "Test")

xgb_comparison <- bind_rows(xgb_cv_table, xgb_test_table) %>%
  select(set, accuracy, f_meas, pr_auc, precision, recall, roc_auc)
xgb_comparison


## ---------------------------------------------------------------------------------------------------
#prepare data for plot
xgb_f1_auc_plot_data <- xgb_comparison %>%
  select(set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")


## ---------------------------------------------------------------------------------------------------
# Plot 
ggplot(xgb_f1_auc_plot_data, aes(x = metric, y = value, fill = set)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(value, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Cross-Validation" = "#F8766D", "Test" = "#00BFC4")) +
  labs(
    title = "F1 Score and ROC AUC: CV vs Test (XGboost)",
    x = "Metric",
    y = "Metric Value",
    fill = "Dataset"
  ) +
  theme_minimal()


## ---------------------------------------------------------------------------------------------------
# bind random forest to the existing data
logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data <- bind_rows(
  logreg_lda_comparison,          # comparison table with LogReg and LDA
  elastic_net_comparison %>%
    mutate(model= "Elastic Net"), # add elastic net table
  rf_comparison %>%      # add random forest table
    mutate(model = "Random Forest"),
  knn_comparison %>%
    mutate(model = "KNN"), #add KNN table
  svm_comparison %>%
    mutate(model = "SVM"), # add SVM table
  xgb_comparison %>%
    mutate(model = "XGboost")) # add Xgboost table

logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data <- logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data %>%
  select(model, set, f_meas, roc_auc) %>%
  pivot_longer(cols = c(f_meas, roc_auc), names_to = "metric", values_to = "value")

ggplot(logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data, aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1)) +
  labs(title = "F1 Score,ROC AUC:CV vs Test(All models:19 features)",
       y = "Metric Value", x = "Model") +
  scale_fill_discrete(name = "Dataset") +
  facet_grid(~ metric)  

## ---------------------------------------------------------------------------------------------------
ggplot(logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data, 
       aes(x = model, y = value, fill = set)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(value, 3)),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 2.5) +  
  facet_wrap(~ metric, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "top",
        legend.title = element_text(size = 8),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.4, "cm"),
    strip.text = element_text(size = 11, face = "bold")) + 
  labs(title = "F1 Score and ROC AUC: CV vs Test (7 models: 19 features)",
       y = "Metric Value", x = "Model")



## ---------------------------------------------------------------------------------------------------
# Prepare the test set performance table
test_performance <- logreg_lda_elasticnet_rf_knn_svm_xgboost_f1_auc_data %>%
  filter(set == "Test") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  select(Model = model, `F1 Score` = f_meas, `ROC-AUC` = roc_auc) %>%
  arrange(desc(`F1 Score`)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))  # round to 3 decimals

# Print a nice-looking table
kable(test_performance, caption = "Test Set Performance by Model")


## ---------------------------------------------------------------------------------------------------
all_test_preds <- bind_rows(logreg_test_preds, lda_test_preds, elastic_net_test_preds, rf_test_preds, knn_test_preds, svm_test_preds, xgb_test_preds)

logreg_lda_elastic_test_preds <- bind_rows(logreg_test_preds, lda_test_preds, elastic_net_test_preds)
rf_knn_svm_xgb_test_preds <- bind_rows(rf_test_preds, knn_test_preds, svm_test_preds, xgb_test_preds)

all_roc_data <- all_test_preds %>%
  group_by(model) %>%
  roc_curve(truth = class, .pred_high)

logreg_lda_elastic_roc_data <- logreg_lda_elastic_test_preds %>%
  group_by(model) %>%
  roc_curve(truth = class, .pred_high)

rf_knn_svm_xgb_roc_data <- rf_knn_svm_xgb_test_preds %>%
  group_by(model) %>%
  roc_curve(truth = class, .pred_high)

# Plot the ROC curves
ggplot(all_roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  coord_equal() +
  labs(
    title = "ROC Curves on Test Set:All models",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(logreg_lda_elastic_roc_data, aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  coord_equal() +
  labs(
    title = "ROC Curves on Test Set:LogReg, LDA, Elastic Net",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(rf_knn_svm_xgb_roc_data , aes(x = 1 - specificity, y = sensitivity, color = model)) +
  geom_path(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  coord_equal() +
  labs(
    title = "ROC Curves on Test Set:RF,KNN, SVM, XGboost",
    x = "1 - Specificity (False Positive Rate)",
    y = "Sensitivity (True Positive Rate)"
  ) +
  theme_minimal(base_family = "Arial")


## ---------------------------------------------------------------------------------------------------
# Create PR curve data
all_pr_data <- all_test_preds %>%
  group_by(model) %>%
  pr_curve(truth = class, .pred_high)

logreg_lda_elastic_pr_data <- logreg_lda_elastic_test_preds %>%
  group_by(model) %>%
  pr_curve(truth = class, .pred_high)

rf_knn_svm_xgb_pr_data <- rf_knn_svm_xgb_test_preds %>%
  group_by(model) %>%
  pr_curve(truth = class, .pred_high)

# Plot PR curves
ggplot(all_pr_data, aes(x = recall, y = precision, color = model)) +
  geom_path(linewidth = 0.7) +
  labs(
    title = "PR Curves on Test Set:All models",
    x = "Recall",
    y = "Precision"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(logreg_lda_elastic_pr_data, aes(x = recall, y = precision, color = model)) +
  geom_path(linewidth = 0.7) +
  labs(
    title = "PR Curves on Test Set:LogReg, LDA, Elastic Net",
    x = "Recall",
    y = "Precision"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(rf_knn_svm_xgb_pr_data, aes(x = recall, y = precision, color = model)) +
  geom_path(linewidth = 0.7) +
  labs(
    title = "PR Curves on Test Set:RF, KNN, SVM, XGboost",
    x = "Recall",
    y = "Precision"
  ) +
  theme_minimal(base_family = "Arial")


## ---------------------------------------------------------------------------------------------------
all_gain_data <- all_test_preds %>%
  group_by(model) %>%
  gain_curve(truth = class, .pred_high, event_level = "first")

logreg_lda_elastic_gain_data <- logreg_lda_elastic_test_preds %>%
  group_by(model) %>%
  gain_curve(truth = class, .pred_high, event_level = "first")

rf_knn_svm_xgb_gain_data <- rf_knn_svm_xgb_test_preds %>%
  group_by(model) %>%
  gain_curve(truth = class, .pred_high, event_level = "first")

ggplot(all_gain_data, aes(x = .percent_tested, y = .percent_found, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  labs(
    title = "Cumulative Gain Curve:All models",
    x = "Proportion of Samples Tested (Top % by Score)",
    y = "Proportion of Positives Captured"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(logreg_lda_elastic_gain_data, aes(x = .percent_tested, y = .percent_found, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  labs(
    title = "Cumulative Gain Curve:LogReg,LDA,ELasitc Net",
    x = "Proportion of Samples Tested (Top % by Score)",
    y = "Proportion of Positives Captured"
  ) +
  theme_minimal(base_family = "Arial")

ggplot(rf_knn_svm_xgb_gain_data, aes(x = .percent_tested, y = .percent_found, color = model)) +
  geom_line(linewidth = 0.7) +
  geom_abline(lty = 2, color = "gray") +
  labs(
    title = "Cumulative Gain Curve:RF,KNN,SVM,XGboost",
    x = "Proportion of Samples Tested (Top % by Score)",
    y = "Proportion of Positives Captured"
  ) +
  theme_minimal(base_family = "Arial")


## ---------------------------------------------------------------------------------------------------
profit_regular <- -1000
profit_high <- 5000

#All models
all_gain_data2 <- all_test_preds %>%
  group_by(model) %>%
  arrange(-.pred_high)%>%
  mutate(
    profit = cumsum(ifelse(class == "high", profit_high, profit_regular)),
    number = seq(1, dim(test)[1]),
    percentage = (row_number() / n())*100
  )
ggplot(all_gain_data2, aes(x = percentage, y = profit, color = model)) +
  geom_line(linewidth = 0.7) + 
  labs(
    title = "Profit curves(high-risk:profit$5000, regular:cost$1000)",
    x = "Percentage of Samples Tested",
    y = "Profit (by Dollar)"
  ) +
  theme_minimal(base_family = "Arial")

#LogReg, LDA, ELastic Net
logreg_lda_elastic_gain_data2 <- logreg_lda_elastic_test_preds %>%
  group_by(model) %>%
  arrange(-.pred_high)%>%
  mutate(
    profit = cumsum(ifelse(class == "high", profit_high, profit_regular)),
    number = seq(1, dim(test)[1]),
    percentage = (row_number() / n())*100
  )
ggplot(logreg_lda_elastic_gain_data2, aes(x = percentage, y = profit, color = model)) +
  geom_line(linewidth = 0.7) + 
  labs(
    title = "Profit curves(high-risk:profit$5000, regular:cost$1000)",
    x = "Percentage of Samples Tested",
    y = "Profit (by Dollar)"
  ) +
  theme_minimal(base_family = "Arial")

# RF, KNN, SVM, XGboost
rf_knn_svm_xgb_gain_data2 <- rf_knn_svm_xgb_test_preds %>%
  group_by(model) %>%
  arrange(-.pred_high)%>%
  mutate(
    profit = cumsum(ifelse(class == "high", profit_high, profit_regular)),
    number = seq(1, dim(test)[1]),
    percentage = (row_number() / n())*100
  )
ggplot(rf_knn_svm_xgb_gain_data2, aes(x = percentage, y = profit, color = model)) +
  geom_line(linewidth = 0.7) + 
  labs(
    title = "Profit curves(high-risk:profit$5000, regular:cost$1000)",
    x = "Percentage of Samples Tested",
    y = "Profit (by Dollar)"
  ) +
  theme_minimal(base_family = "Arial")


## ---------------------------------------------------------------------------------------------------
logreg_lift <- lift_curve(
  data = logreg_test_preds,
  truth = class,
  .pred_high
)
ggplot(logreg_lift, aes(x = .percent_tested, y = .lift)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Lift Curve - Logistic Regression",
    x = "Percent of Samples Tested",
    y = "Lift (Gain Over Random)"
  ) +
  theme_minimal(base_family = "Arial")

#all lift curves
all_test_preds$class <- factor(all_test_preds$class, levels = c("high", "regular"))
# Calculate lift curves for each model
lift_all_models <- all_test_preds %>%
  group_by(model) %>%
  lift_curve(truth = class, .pred_high)

# Plot all lift curves
ggplot(lift_all_models, aes(x = .percent_tested, y = .lift, color = model)) +
  geom_line(size = 0.5) +
  geom_point(size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Lift Curves on Test Set (All Models)",
    x = "Percent of Samples Tested",
    y = "Lift (Gain Over Random)"
  ) +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "right")


## ---------------------------------------------------------------------------------------------------
stopCluster(cl)
registerDoSEQ()

