#' -----------------------------------------------------------------------------
#' Analysis for Helicopter Parenting and Social Competence among Single Malaysian Adults:
#' Filial Piety as a Moderator
#' by Jonathan Z. Ong
#' Last updated: 13 September 2024
#' -----------------------------------------------------------------------------
#' Description:
#' This script contains the full analytical workflow for the study titled 
#' "Helicopter Parenting and Social Competence among Single Malaysian Adults: 
#' Filial Piety as a Moderator." The purpose of this study is to explore how 
#' helicopter parenting influences social competence in single Malaysian adults, 
#' and to examine whether different dimensions of filial piety (Reciprocal and 
#' Authoritarian) moderate this relationship.
#'
#' The script includes the following sections:
#' 1. Data Preparation: Loading, cleaning, and subsetting the dataset.
#' 2. Handling Outliers: Identifying, removing, or winsorizing univariate and 
#'    multivariate outliers to ensure robust analysis.
#' 3. Assumption Checks: Evaluating normality and linearity assumptions for the 
#'    regression models.
#' 4. Correlation Analysis: Computing and visualizing correlation matrices to 
#'    understand relationships between variables.
#' 5. Dealing with Multivariate Outliers: Identifying and addressing multivariate
#'    outliers to improve the robustness of the analyses.
#' 6. Moderation Analysis: Conducting hierarchical regression analyses to test 
#'    for moderation effects of Reciprocal and Authoritarian Filial Piety.
#' 7. Sensitivity Analyses: Performing sensitivity checks to assess the robustness 
#'    of the findings without specific model steps.
#' 8. Simple Slopes Analysis: Examining simple slopes to interpret moderation effects 
#'    in more detail and visualize the interaction effects.
#'
#' This script is structured to be modular and transparent, allowing for easy 
#' replication and adaptation by other researchers. It uses the R statistical 
#' programming language and relies on packages such as dplyr, ggplot2, haven, 
#' and others. 
#'
#' To replicate the analyses, users should ensure they have the appropriate 
#' dataset ('hp_dataset_cleaned.csv') and install the required R packages 
#' listed in Section 1.
#'
#' Note: This script is provided under an open license to facilitate 
#' reproducibility and transparency in research. Contributions, feedback, and 
#' adaptations are welcome.
#' -----------------------------------------------------------------------------


rm(list = ls()) # clear all

# 1. Load Packages and Specify Directories =====================================

## 1.1 Load necessary libraries ----
library(dplyr)
library(ggplot2)
library(reshape2)
library(haven)  # Assuming the dataset is in .sav format
library(extrafont)
library(showtext)
library(MASS) 
library(broom)
library(tidyr)
library(interactions)
library(emmeans)


# Register the regular "Times New Roman" font for plotting later
font_add(family = "Times New Roman", regular = "C:/Windows/Fonts/times.ttf")

# Enable showtext for automatic font rendering
showtext_auto()

## 1.2 Set Working Directory and Create Folders ----

# Set the working directory
setwd(".")

# Create the main 'results' folder if it doesn't exist
if (!file.exists("results")) {
  dir.create("results")
}

# Generate the folder name using the current date and time
current_date <- format(Sys.time(), "%d-%m-%Y")
foldername <- file.path("results", paste("results", current_date, sep = "_"))

# Create the main results folder for the current date and time if it doesn't exist
if (!file.exists(foldername)) {
  dir.create(foldername)
}

# Create subfolders within the 'results' folder and assign to objects
correlations_dir <- file.path(foldername, "correlations_descriptives")
moderation_dir <- file.path(foldername, "moderation")
simple_slopes_dir <- file.path(foldername, "simple_slopes")

subfolders <- c(correlations_dir, moderation_dir, simple_slopes_dir)

for (subfolder in subfolders) {
  if (!file.exists(subfolder)) {
    dir.create(subfolder)
  }
}

## 1.3 Load Dataset ----

# Read the dataset
a <- read.csv("hp_dataset_cleaned.csv")
summary(a)

# Step 1: Subset the relevant columns
a2 <- a %>%
  dplyr::select(
    Hel_Avg, RecFP, AuthFP, Gender, Age, Ethnicity_Chinese, Rrel_exp4, SCavg, 
    Initiation, NegAssr, EmoSup, SelfDisc, ConfMan
  )

# Step 2: Rename the columns
b <- a2 %>%
  rename(
    Helicopter_parenting = Hel_Avg,
    Reciprocal_filial_piety = RecFP,
    Authoritarian_filial_piety = AuthFP,
    Relationship_experience = Rrel_exp4,    
    Social_competence = SCavg,
    Relationship_initiation = Initiation,
    Negative_assertion = NegAssr,
    Emotional_support = EmoSup,
    Self_disclosure = SelfDisc,
    Conflict_management = ConfMan
  )

# View the first few rows of the final dataset
head(b)

# 2. Deal with Outliers =======================================================

## 2.1 Identify Outliers Manually ----

# Select numeric variables from dataset 'b'
numeric_vars <- b %>% dplyr::select(where(is.numeric))

# Define the z-score threshold
threshold <- 3.29

# Calculate z-scores for each numeric column
z_scores <- as.data.frame(lapply(numeric_vars, function(column) {
  (column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE)
}))

# Identify outliers based on the z-score threshold
outliers_matrix <- as.data.frame(lapply(z_scores, function(column) {
  abs(column) > threshold
}))

## 2.2 Print Outliers Before Winsorization ----

# Get the indices of outliers
outlier_indices <- which(outliers_matrix == TRUE, arr.ind = TRUE)

# If there are any outliers, print them
if (nrow(outlier_indices) > 0) {
  cat("Outliers identified before winsorization:\n")
  for (i in 1:nrow(outlier_indices)) {
    row <- outlier_indices[i, "row"]
    col <- outlier_indices[i, "col"]
    var_name <- names(numeric_vars)[col]
    outlier_value <- numeric_vars[row, col]
    z_score_value <- z_scores[row, col]
    cat(sprintf("Row %d, Variable '%s': Value = %f, Z-Score = %f\n",
                row, var_name, outlier_value, z_score_value))
  }
} else {
  cat("No outliers identified before winsorization.\n")
}

## 2.3 Winsorize Outliers Based on Z-Score Threshold ----

# Create a copy of 'b' to store the winsorized data
c_cleaned <- b

# For each numeric variable, winsorize the outliers
for (var_name in names(numeric_vars)) {
  # Get the original column
  column <- c_cleaned[[var_name]]
  
  # Calculate mean and standard deviation
  mean_col <- mean(column, na.rm = TRUE)
  sd_col <- sd(column, na.rm = TRUE)
  
  # Calculate lower and upper bounds based on z-score threshold
  lower_bound <- mean_col - threshold * sd_col
  upper_bound <- mean_col + threshold * sd_col
  
  # Winsorize the data
  c_cleaned[[var_name]][column < lower_bound & !is.na(column)] <- lower_bound
  c_cleaned[[var_name]][column > upper_bound & !is.na(column)] <- upper_bound
}

## 2.4 Show What the Outliers Were Winsorized To ----

# For each outlier, show the original and winsorized values
if (nrow(outlier_indices) > 0) {
  cat("\nOutliers after winsorization:\n")
  for (i in 1:nrow(outlier_indices)) {
    row <- outlier_indices[i, "row"]
    col <- outlier_indices[i, "col"]
    var_name <- names(numeric_vars)[col]
    original_value <- b[row, var_name]
    winsorized_value <- c_cleaned[row, var_name]
    cat(sprintf("Row %d, Variable '%s': Original Value = %f, Winsorized Value = %f\n",
                row, var_name, original_value, winsorized_value))
  }
}

## 2.5 Print Summary Before and After Winsorization ----

cat("\nSummary of data before winsorization:\n")
print(summary(b))

cat("\nSummary of data after winsorization:\n")
print(summary(c_cleaned))


# 3. Check Assumptions ========================================================

## 3.1. Normality of Variable Distribution ----

# Assuming c_cleaned is already loaded
# Inspect the dataset
str(c_cleaned)

# Identify continuous and categorical variables
continuous_vars <- c_cleaned %>% dplyr::select(where(is.numeric)) %>% colnames()
categorical_vars <- c_cleaned %>% dplyr::select(where(is.factor) | where(is.character)) %>% colnames()

# Plot histogram and density plot together for continuous variables
for (var in continuous_vars) {
  p <- ggplot(c_cleaned, aes_string(x = var)) +
    geom_histogram(aes(y = ..density..), binwidth = 1, fill = "blue", alpha = 0.5) +
    geom_density(color = "red", size = 1) +
    ggtitle(paste("Histogram and Density Plot of", var)) +
    theme_minimal()
  
  print(p)
}

# Plot bar plots for categorical variables
for (var in categorical_vars) {
  p <- ggplot(c_cleaned, aes_string(x = var)) +
    geom_bar(fill = "orange", color = "black", alpha = 0.7) +
    ggtitle(paste("Bar Plot of", var)) +
    theme_minimal()
  
  print(p)
}

## 3.2 Residual Assumptions ---- 

# 3.2.1 Convert 'Gender' and 'Ethnicity_Chinese' to Factors ----

# Ensure 'Gender' and 'Ethnicity_Chinese' are factors
c_cleaned_factor <- c_cleaned %>%
  mutate(
    Gender = as.factor(Gender),
    Ethnicity_Chinese = as.factor(Ethnicity_Chinese)
  )

# 3.2.2 Define the Predictor Variables ----

# Define the predictor variables
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")
predictors <- c(base_predictors, moderators, covariates, interaction_terms)

# 3.2.3 Define the Outcome Variables ----

# Define the outcome variables
manifest_outcomes <- c(
  "Social_competence", 
  "Relationship_initiation", 
  "Negative_assertion", 
  "Emotional_support", 
  "Self_disclosure", 
  "Conflict_management"
)

# 3.2.4 Create and Plot for Each Outcome Variable ----

# Loop through each outcome variable to fit the model and plot residuals
for (outcome in manifest_outcomes) {
  
  # 3.2.4.1 Create the Formula for the Linear Model ----
  formula <- as.formula(
    paste(outcome, "~", paste(predictors, collapse = " + "))
  )
  
  # 3.2.4.2 Fit the Linear Model ----
  model <- lm(formula, data = c_cleaned_factor)
  
  # 3.2.4.3 Extract Residuals ----
  residuals <- model$residuals
  
  # 3.2.4.4 Residuals vs Fitted Values Plot (for Homoscedasticity) ----
  p1 <- ggplot(
    data = data.frame(fitted = model$fitted.values, residuals = residuals), 
    aes(x = fitted, y = residuals)
  ) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    ggtitle(paste("Residuals vs Fitted Values for", outcome)) +
    xlab("Fitted Values") +
    ylab("Residuals") +
    theme_minimal()
  
  # 3.2.4.5 Q-Q Plot (for Normality of Residuals) ----
  p2 <- ggplot(
    data = data.frame(sample = residuals), 
    aes(sample = sample)
  ) +
    stat_qq(color = "darkgreen") +
    stat_qq_line(color = "red") +
    ggtitle(paste("Q-Q Plot of Residuals for", outcome)) +
    theme_minimal()
  
  # 3.2.4.6 Print the Plots ----
  print(p1)
  print(p2)
}


# 4. Correlogram and Descriptives ==============================================

# Function to compute and save correlation matrix
compute_and_save_cor_matrix <- function(dataset, dataset_name, foldername, selected_vars) {
  # Function to format the correlation coefficient with significance levels
  format_cor <- function(cor_value, p_value) {
    signif_level <- ifelse(p_value < 0.001, "***", 
                           ifelse(p_value < 0.01, "**", 
                                  ifelse(p_value < 0.05, "*", "")))
    paste0(round(cor_value, 2), signif_level)
  }
  
  # Initialize an empty matrix to store formatted correlation results
  cor_matrix <- matrix("", nrow = length(selected_vars), ncol = length(selected_vars))
  rownames(cor_matrix) <- selected_vars
  colnames(cor_matrix) <- selected_vars
  
  # Initialize an empty matrix to store N values
  n_matrix <- matrix(NA, nrow = length(selected_vars), ncol = length(selected_vars))
  rownames(n_matrix) <- selected_vars
  colnames(n_matrix) <- selected_vars
  
  # Compute pairwise correlations and format the results
  for (i in 1:length(selected_vars)) {
    for (j in 1:length(selected_vars)) {
      if (i != j) {
        test_result <- cor.test(dataset[[selected_vars[i]]], dataset[[selected_vars[j]]], 
                                method = "pearson", use = "pairwise.complete.obs")
        cor_matrix[i, j] <- format_cor(test_result$estimate, test_result$p.value)
        n_matrix[i, j] <- test_result$parameter + 2  # parameter is df, so add 2 for sample size
      } else {
        cor_matrix[i, j] <- "1"
        n_matrix[i, j] <- sum(!is.na(dataset[[selected_vars[i]]]))
      }
    }
  }
  
  # Create extra rows for mean, standard deviation, lowest, and highest value
  summary_stats <- data.frame(
    Variable = selected_vars,
    M = round(sapply(selected_vars, function(var) mean(dataset[[var]], na.rm = TRUE)), 2),
    SD = round(sapply(selected_vars, function(var) sd(dataset[[var]], na.rm = TRUE)), 2),
    Lowest = sapply(selected_vars, function(var) round(min(dataset[[var]], na.rm = TRUE), 2)),
    Highest = sapply(selected_vars, function(var) round(max(dataset[[var]], na.rm = TRUE), 2)),
    N = sapply(selected_vars, function(var) sum(!is.na(dataset[[var]])))
  )
  
  # Print the formatted correlation matrix
  print(cor_matrix)
  
  # Define the output file paths
  cor_matrix_file <- file.path(foldername, paste0("formatted_correlation_matrix_", dataset_name, ".csv"))
  summary_stats_file <- file.path(foldername, paste0("summary_statistics_", dataset_name, ".csv"))
  n_matrix_file <- file.path(foldername, paste0("n_matrix_", dataset_name, ".csv"))
  
  # Save the formatted correlation matrix, summary statistics, and N matrix to CSV files
  write.csv(as.data.frame(cor_matrix), cor_matrix_file, row.names = TRUE)
  write.csv(summary_stats, summary_stats_file, row.names = FALSE)
  write.csv(as.data.frame(n_matrix), n_matrix_file, row.names = TRUE)
}

# Selected variables (renamed)
selected_vars <- c("Helicopter_parenting", "Reciprocal_filial_piety", "Authoritarian_filial_piety", 
                   "Gender", "Age", "Ethnicity_Chinese", "Relationship_experience", 
                    "Social_competence", "Relationship_initiation", 
                   "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")

# Convert categorical variables to numeric
c_cleaned$Gender <- ifelse(c_cleaned$Gender == "Female", 1, 0)  # Assuming "Female" is 1 and "Male" is 0
c_cleaned$Ethnicity_Chinese <- ifelse(c_cleaned$Ethnicity_Chinese == "Chinese", 1, 0)  # Assuming "Chinese" is 1 and "Non-Chinese" is 0

# Compute and save correlation matrix for the dataset
compute_and_save_cor_matrix(c_cleaned, "cleaned", correlations_dir, selected_vars)


# Load necessary libraries
library(dplyr)

# 5. Dealing with Multivariate Outliers =======================================

## 5.1 Version Including Covariates ----

### 5.1.1 Define continuous predictor variables including covariates ----
continuous_vars_with_cov <- c(
  "Helicopter_parenting", "Reciprocal_filial_piety",
  "Authoritarian_filial_piety", "Age", "Relationship_experience"
)

# Remove rows with NA in any of the continuous variables
c_cleaned_cont_with_cov <- c_cleaned[complete.cases(c_cleaned[continuous_vars_with_cov]), ]

### 5.1.2 Calculate Mahalanobis Distances and Identify Outliers ----
data_matrix_with_cov <- as.matrix(c_cleaned_cont_with_cov[, continuous_vars_with_cov])
cov_matrix_with_cov <- cov(data_matrix_with_cov, use = "complete.obs")
mean_vector_with_cov <- colMeans(data_matrix_with_cov, na.rm = TRUE)
mahal_distances_with_cov <- mahalanobis(data_matrix_with_cov, mean_vector_with_cov, cov_matrix_with_cov)

# Degrees of freedom and cutoff threshold
df_degrees_with_cov <- length(continuous_vars_with_cov)
threshold <- 0.001
cutoff_with_cov <- qchisq(1 - threshold, df = df_degrees_with_cov)

# Print the threshold for removal
cat("Including covariates:\n")
cat("Degrees of freedom (df):", df_degrees_with_cov, "\n")
cat("Threshold (cutoff) for Mahalanobis distance:", cutoff_with_cov, "\n")

# Tagging outliers
c_cleaned_cont_with_cov$multivariate_outlier <- mahal_distances_with_cov > cutoff_with_cov

### 5.1.3 Create datasets with and without multivariate outliers ----
d_with_outliers_with_cov <- c_cleaned_cont_with_cov  # Dataset including outliers

d_cleaned_with_cov <- c_cleaned_cont_with_cov %>%
  filter(multivariate_outlier == FALSE) %>%
  dplyr::select(-multivariate_outlier)

### 5.1.4 Print results ----
tagged_outliers_with_cov <- c_cleaned_cont_with_cov %>% filter(multivariate_outlier == TRUE)

if (nrow(tagged_outliers_with_cov) > 0) {
  cat("Values of tagged outliers (including covariates):\n")
  print(tagged_outliers_with_cov)
} else {
  cat("No outliers were tagged (including covariates).\n")
}

# Check the number of rows before and after removing outliers
cat("Number of rows before removing outliers (including covariates):", nrow(d_with_outliers_with_cov), "\n")
cat("Number of rows after removing outliers (including covariates):", nrow(d_cleaned_with_cov), "\n")

## 5.2 Version Excluding Covariates ----

### 5.2.1 Define continuous predictor variables excluding covariates ----
continuous_vars_no_cov <- c(
  "Helicopter_parenting", "Reciprocal_filial_piety",
  "Authoritarian_filial_piety"
)

# Remove rows with NA in any of the continuous variables
c_cleaned_cont_no_cov <- c_cleaned[complete.cases(c_cleaned[continuous_vars_no_cov]), ]

### 5.2.2 Calculate Mahalanobis Distances and Identify Outliers ----
data_matrix_no_cov <- as.matrix(c_cleaned_cont_no_cov[, continuous_vars_no_cov])
cov_matrix_no_cov <- cov(data_matrix_no_cov, use = "complete.obs")
mean_vector_no_cov <- colMeans(data_matrix_no_cov, na.rm = TRUE)
mahal_distances_no_cov <- mahalanobis(data_matrix_no_cov, mean_vector_no_cov, cov_matrix_no_cov)

# Degrees of freedom and cutoff threshold
df_degrees_no_cov <- length(continuous_vars_no_cov)
threshold <- 0.001
cutoff_no_cov <- qchisq(1 - threshold, df = df_degrees_no_cov)

# Print the threshold for removal
cat("\nExcluding covariates:\n")
cat("Degrees of freedom (df):", df_degrees_no_cov, "\n")
cat("Threshold (cutoff) for Mahalanobis distance:", cutoff_no_cov, "\n")

# Tagging outliers
c_cleaned_cont_no_cov$multivariate_outlier <- mahal_distances_no_cov > cutoff_no_cov

### 5.2.3 Create datasets with and without multivariate outliers ----
d_with_outliers_no_cov <- c_cleaned_cont_no_cov  # Dataset including outliers

d_cleaned_no_cov <- c_cleaned_cont_no_cov %>%
  filter(multivariate_outlier == FALSE) %>%
  dplyr::select(-multivariate_outlier)

### 5.2.4 Print results ----
tagged_outliers_no_cov <- c_cleaned_cont_no_cov %>% filter(multivariate_outlier == TRUE)

if (nrow(tagged_outliers_no_cov) > 0) {
  cat("Values of tagged outliers (excluding covariates):\n")
  print(tagged_outliers_no_cov)
} else {
  cat("No outliers were tagged (excluding covariates).\n")
}

# Check the number of rows before and after removing outliers
cat("Number of rows before removing outliers (excluding covariates):", nrow(d_with_outliers_no_cov), "\n")
cat("Number of rows after removing outliers (excluding covariates):", nrow(d_cleaned_no_cov), "\n")


# 6. Moderation ===============================================================

## 6.1 Define functions for moderation analysis ----

# Function to standardize variables (mean = 0, sd = 1), excluding specified variables
standardize <- function(df, variables, exclude_vars = NULL) {
  vars_to_standardize <- setdiff(variables, exclude_vars)
  df %>% mutate(across(all_of(vars_to_standardize), ~ scale(.) %>% as.vector()))
}

# Function to add asterisks based on p-values
add_asterisks <- function(p_value) {
  if (length(p_value) > 1) stop("The condition has length > 1")
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

## 6.2 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")

# Combine all variables for listwise deletion
all_vars <- c(base_predictors, moderators, covariates, manifest_outcomes)

## 6.3 Define function to process dataset and fit models ----

# Function to process dataset, perform listwise deletion, standardize variables, and fit hierarchical models
process_and_fit_models <- function(dataset, dataset_name) {
  # Perform listwise deletion
  dataset_listwise <- dataset %>% drop_na(all_of(all_vars))
  
  # Convert dummy variables to factors if they are not already
  dataset_listwise <- dataset_listwise %>%
    mutate(
      Gender = as.factor(Gender),
      Ethnicity_Chinese = as.factor(Ethnicity_Chinese)
    )
  
  # Add interaction terms to the dataset for Helicopter_parenting and each moderator
  dataset_interaction <- dataset_listwise
  for (moderator in moderators) {
    interaction_term <- paste("Helicopter_parenting", moderator, sep = "*")
    dataset_interaction <- dataset_interaction %>%
      mutate(!!interaction_term := .data[["Helicopter_parenting"]] * .data[[moderator]])
  }
  
  # Define variables to standardize (excluding dummy variables)
  vars_to_standardize <- setdiff(c(base_predictors, moderators, covariates, manifest_outcomes), c("Gender", "Ethnicity_Chinese"))
  
  # Standardize predictors and outcomes, excluding Gender and Ethnicity_Chinese
  dataset_standardized <- standardize(dataset_interaction, vars_to_standardize)
  
  # Ensure that Gender and Ethnicity_Chinese are included in the standardized dataset
  dataset_standardized <- dataset_standardized %>%
    mutate(
      Gender = dataset_interaction$Gender,
      Ethnicity_Chinese = dataset_interaction$Ethnicity_Chinese
    )
  
  # Function to fit and summarize a hierarchical linear regression model
  fit_hierarchical_model <- function(dataset, covariates, base_predictors, moderators, interaction_terms, outcome) {
    cat("\n\n##### Outcome:", outcome, " | Dataset:", dataset_name, "#####\n\n")
    
    # Step 1: Covariates only
    formula_step1 <- as.formula(paste(outcome, "~", paste(covariates, collapse = " + ")))
    model_step1 <- lm(formula_step1, data = dataset)
    
    # Print summary for Step 1
    cat("## Step 1: Covariates only\n")
    print(summary(model_step1))
    
    # Step 2: Base predictors and moderators
    formula_step2 <- as.formula(paste(outcome, "~", paste(c(covariates, base_predictors, moderators), collapse = " + ")))
    model_step2 <- lm(formula_step2, data = dataset)
    
    # Print summary for Step 2
    cat("## Step 2: Base predictors and moderators\n")
    print(summary(model_step2))
    
    # Step 3: Interaction terms
    formula_step3 <- as.formula(paste(outcome, "~", paste(c(covariates, base_predictors, moderators, interaction_terms), collapse = " + ")))
    model_step3 <- lm(formula_step3, data = dataset)
    
    # Print summary for Step 3
    cat("## Step 3: Interaction terms\n")
    print(summary(model_step3))
    
    # Summarize each step
    summary_step1 <- tidy(model_step1) %>% 
      mutate(significance = mapply(add_asterisks, p.value)) %>% 
      mutate(estimate = paste(round(estimate, 2), significance, sep = "")) %>% 
      mutate(step = "Step 1", outcome = outcome)
    summary_step2 <- tidy(model_step2) %>% 
      mutate(significance = mapply(add_asterisks, p.value)) %>% 
      mutate(estimate = paste(round(estimate, 2), significance, sep = "")) %>% 
      mutate(step = "Step 2", outcome = outcome)
    summary_step3 <- tidy(model_step3) %>% 
      mutate(significance = mapply(add_asterisks, p.value)) %>% 
      mutate(estimate = paste(round(estimate, 2), significance, sep = "")) %>% 
      mutate(step = "Step 3", outcome = outcome)
    
    # Extract p-values for R2 from summary
    r2_p_value_step1 <- pf(summary(model_step1)$fstatistic[1], summary(model_step1)$fstatistic[2], summary(model_step1)$fstatistic[3], lower.tail = FALSE)
    r2_p_value_step2 <- pf(summary(model_step2)$fstatistic[1], summary(model_step2)$fstatistic[2], summary(model_step2)$fstatistic[3], lower.tail = FALSE)
    r2_p_value_step3 <- pf(summary(model_step3)$fstatistic[1], summary(model_step3)$fstatistic[2], summary(model_step3)$fstatistic[3], lower.tail = FALSE)
    
    # Add R2, p-value for R2, and N to the summaries
    summary_step1 <- summary_step1 %>% 
      mutate(R2 = round(summary(model_step1)$r.squared, 2),
             R2_p_value = r2_p_value_step1,
             R2_annotated = paste0(round(summary(model_step1)$r.squared, 2), add_asterisks(r2_p_value_step1)),
             N = summary(model_step1)$df[2] + summary(model_step1)$df[1])
    summary_step2 <- summary_step2 %>% 
      mutate(R2 = round(summary(model_step2)$r.squared, 2),
             R2_p_value = r2_p_value_step2,
             R2_annotated = paste0(round(summary(model_step2)$r.squared, 2), add_asterisks(r2_p_value_step2)),
             N = summary(model_step2)$df[2] + summary(model_step2)$df[1])
    summary_step3 <- summary_step3 %>% 
      mutate(R2 = round(summary(model_step3)$r.squared, 2),
             R2_p_value = r2_p_value_step3,
             R2_annotated = paste0(round(summary(model_step3)$r.squared, 2), add_asterisks(r2_p_value_step3)),
             N = summary(model_step3)$df[2] + summary(model_step3)$df[1])
    
    # Compute R2 change and its significance
    r2_change_2 <- summary(model_step2)$r.squared - summary(model_step1)$r.squared
    r2_change_3 <- summary(model_step3)$r.squared - summary(model_step2)$r.squared
    f_change_2 <- anova(model_step1, model_step2)$`F`[2]
    f_change_3 <- anova(model_step2, model_step3)$`F`[2]
    p_value_change_2 <- anova(model_step1, model_step2)$`Pr(>F)`[2]
    p_value_change_3 <- anova(model_step2, model_step3)$`Pr(>F)`[2]
    
    r2_change_summary <- data.frame(
      step = c("Step 2 vs Step 1", "Step 3 vs Step 2"),
      R2_change = paste0(round(c(r2_change_2, r2_change_3), 3), mapply(add_asterisks, c(p_value_change_2, p_value_change_3))),
      F_change = round(c(f_change_2, f_change_3), 3),
      p_value = round(c(p_value_change_2, p_value_change_3), 3),
      significance = mapply(add_asterisks, c(p_value_change_2, p_value_change_3)),
      outcome = outcome
    )
    
    # Combine summaries into one data frame
    combined_summary <- bind_rows(summary_step1, summary_step2, summary_step3, r2_change_summary)
    
    return(combined_summary)
  }
  
  # Fit and summarize models for each outcome variable
  outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
  all_models <- bind_rows(lapply(outcomes, function(outcome) {
    fit_hierarchical_model(dataset_standardized, covariates, base_predictors, moderators, interaction_terms, outcome)
  }))
  
  all_models <- all_models %>% mutate(dataset = dataset_name)
  
  return(all_models)
}

## 6.4 Process datasets and combine results ----

# Process and fit models for datasets without and with multivariate outliers
models_d_cleaned <- process_and_fit_models(d_cleaned_with_cov, "d_cleaned_with_cov")
models_d_with_outliers <- process_and_fit_models(d_with_outliers_with_cov, "d_with_outliers_with_cov")

# Combine the models from both datasets
combined_models <- bind_rows(models_d_cleaned, models_d_with_outliers)

## 6.5 Compare significance levels and export results ----

# Compare significance levels and highlight differences
compare_significance <- function(df) {
  df <- df %>%
    group_by(term, step, outcome) %>%
    mutate(significance_d_cleaned = significance[dataset == "d_cleaned_with_cov"],
           significance_d_with_outliers = significance[dataset == "d_with_outliers_with_cov"],
           significance_difference = ifelse(significance_d_cleaned != significance_d_with_outliers, "Yes", "No")) %>%
    ungroup()
  return(df)
}

combined_models <- compare_significance(combined_models)

# Export combined summaries to a single CSV
write.csv(combined_models, file.path(moderation_dir, "hierarchical_models_combined_comparison.csv"), row.names = FALSE)

# Print the combined summary
print(combined_models)

# 7. Sensitivity Analyses Without Step 1 ======================================

## 7.1 Define functions for sensitivity analysis ----

# Function to standardize variables (mean = 0, sd = 1), excluding specified variables
standardize <- function(df, variables, exclude_vars = NULL) {
  vars_to_standardize <- setdiff(variables, exclude_vars)
  df %>% mutate(across(all_of(vars_to_standardize), ~ scale(.) %>% as.vector()))
}

# Function to add asterisks based on p-values
add_asterisks <- function(p_value) {
  if (length(p_value) > 1) stop("The condition has length > 1")
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

## 7.2 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")

# Combine all variables for listwise deletion
all_vars <- c(base_predictors, moderators, covariates, manifest_outcomes)

## 7.3 Define function to process dataset and fit models ----

# Function to process dataset, perform listwise deletion, standardize variables, and fit hierarchical models
process_and_fit_models <- function(dataset, dataset_name) {
  # Perform listwise deletion
  dataset_listwise <- dataset %>% drop_na(all_of(all_vars))
  
  # Convert dummy variables to factors if they are not already
  dataset_listwise <- dataset_listwise %>%
    mutate(
      Gender = as.factor(Gender),
      Ethnicity_Chinese = as.factor(Ethnicity_Chinese)
    )
  
  # Add interaction terms to the dataset for Helicopter_parenting and each moderator
  dataset_interaction <- dataset_listwise
  for (moderator in moderators) {
    interaction_term <- paste("Helicopter_parenting", moderator, sep = "*")
    dataset_interaction <- dataset_interaction %>%
      mutate(!!interaction_term := .data[["Helicopter_parenting"]] * .data[[moderator]])
  }
  
  # Define variables to standardize (excluding dummy variables)
  vars_to_standardize <- setdiff(c(base_predictors, moderators, covariates, manifest_outcomes), c("Gender", "Ethnicity_Chinese"))
  
  # Standardize predictors and outcomes, excluding Gender and Ethnicity_Chinese
  dataset_standardized <- standardize(dataset_interaction, vars_to_standardize)
  
  # Ensure that Gender and Ethnicity_Chinese are included in the standardized dataset
  dataset_standardized <- dataset_standardized %>%
    mutate(
      Gender = dataset_interaction$Gender,
      Ethnicity_Chinese = dataset_interaction$Ethnicity_Chinese
    )
  
  # Function to fit and summarize a hierarchical linear regression model
  fit_hierarchical_model <- function(dataset, base_predictors, moderators, interaction_terms, outcome) {
    cat("\n\n##### Outcome:", outcome, " | Dataset:", dataset_name, "#####\n\n")
    
    # Step 1: Base predictors and moderators
    formula_step1 <- as.formula(paste(outcome, "~", paste(c(base_predictors, moderators), collapse = " + ")))
    model_step1 <- lm(formula_step1, data = dataset)
    
    # Print summary for Step 1
    cat("## Step 1: Base predictors and moderators\n")
    print(summary(model_step1))
    
    # Step 2: Interaction terms
    formula_step2 <- as.formula(paste(outcome, "~", paste(c(base_predictors, moderators, interaction_terms), collapse = " + ")))
    model_step2 <- lm(formula_step2, data = dataset)
    
    # Print summary for Step 2
    cat("## Step 2: Interaction terms\n")
    print(summary(model_step2))
    
    # Summarize each step
    summary_step1 <- tidy(model_step1) %>% 
      mutate(significance = mapply(add_asterisks, p.value)) %>% 
      mutate(estimate = paste(round(estimate, 2), significance, sep = "")) %>% 
      mutate(step = "Step 1", outcome = outcome)
    summary_step2 <- tidy(model_step2) %>% 
      mutate(significance = mapply(add_asterisks, p.value)) %>% 
      mutate(estimate = paste(round(estimate, 2), significance, sep = "")) %>% 
      mutate(step = "Step 2", outcome = outcome)
    
    # Extract p-values for R2 from summary
    r2_p_value_step1 <- pf(summary(model_step1)$fstatistic[1], summary(model_step1)$fstatistic[2], summary(model_step1)$fstatistic[3], lower.tail = FALSE)
    r2_p_value_step2 <- pf(summary(model_step2)$fstatistic[1], summary(model_step2)$fstatistic[2], summary(model_step2)$fstatistic[3], lower.tail = FALSE)
    
    # Add R2, p-value for R2, and N to the summaries
    summary_step1 <- summary_step1 %>% 
      mutate(R2 = round(summary(model_step1)$r.squared, 2),
             R2_p_value = r2_p_value_step1,
             R2_annotated = paste0(round(summary(model_step1)$r.squared, 2), add_asterisks(r2_p_value_step1)),
             N = summary(model_step1)$df[2] + summary(model_step1)$df[1])
    summary_step2 <- summary_step2 %>% 
      mutate(R2 = round(summary(model_step2)$r.squared, 2),
             R2_p_value = r2_p_value_step2,
             R2_annotated = paste0(round(summary(model_step2)$r.squared, 2), add_asterisks(r2_p_value_step2)),
             N = summary(model_step2)$df[2] + summary(model_step2)$df[1])
    
    # Compute R2 change and its significance
    r2_change <- summary(model_step2)$r.squared - summary(model_step1)$r.squared
    f_change <- anova(model_step1, model_step2)$`F`[2]
    p_value_change <- anova(model_step1, model_step2)$`Pr(>F)`[2]
    
    r2_change_summary <- data.frame(
      step = "Step 2 vs Step 1",
      R2_change = paste0(round(r2_change, 3), add_asterisks(p_value_change)),
      F_change = round(f_change, 3),
      p_value = round(p_value_change, 3),
      significance = add_asterisks(p_value_change),
      outcome = outcome
    )
    
    # Combine summaries into one data frame
    combined_summary <- bind_rows(summary_step1, summary_step2, r2_change_summary)
    
    return(combined_summary)
  }
  
  # Fit and summarize models for each outcome variable
  outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
  all_models <- bind_rows(lapply(outcomes, function(outcome) {
    fit_hierarchical_model(dataset_standardized, base_predictors, moderators, interaction_terms, outcome)
  }))
  
  all_models <- all_models %>% mutate(dataset = dataset_name)
  
  return(all_models)
}

## 7.4 Process datasets and combine results ----

# Process and fit models for datasets without and with multivariate outliers
models_d_cleaned <- process_and_fit_models(d_cleaned_no_cov, "d_cleaned_no_cov")
models_d_with_outliers <- process_and_fit_models(d_with_outliers_no_cov, "d_with_outliers_no_cov")

# Combine the models from both datasets
combined_models <- bind_rows(models_d_cleaned, models_d_with_outliers)

## 7.5 Compare significance levels and export results ----

# Compare significance levels and highlight differences
compare_significance <- function(df) {
  df <- df %>%
    group_by(term, step, outcome) %>%
    mutate(significance_d_cleaned = significance[dataset == "d_cleaned_no_cov"],
           significance_d_with_outliers = significance[dataset == "d_with_outliers_no_cov"],
           significance_difference = ifelse(significance_d_cleaned != significance_d_with_outliers, "Yes", "No")) %>%
    ungroup()
  return(df)
}

combined_models <- compare_significance(combined_models)

# Export combined summaries to a single CSV
write.csv(combined_models, file.path(moderation_dir, "hierarchical_models_combined_comparison_nocov.csv"), row.names = FALSE)

# Print the combined summary
print(combined_models)

# 8. Simple Slopes Analysis ===================================================

## 8.1 Setup -------------------------------------------------------------------

### 8.1.1 Load Required Libraries ----

# Ensure the necessary packages are installed and loaded
required_packages <- c("dplyr", "ggplot2", "emmeans")
installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

### 8.1.2 Define Helper Functions ----

# Function to standardize variables, excluding specified variables
standardize <- function(df, variables, exclude_vars = NULL) {
  vars_to_standardize <- setdiff(variables, exclude_vars)
  df %>% mutate(across(all_of(vars_to_standardize), ~ scale(.) %>% as.vector()))
}

# Function to format p-values and omit the leading zero
format_p_value <- function(p_value) {
  if (is.na(p_value)) {
    formatted_p_value <- "NA"
  } else if (p_value < 0.001) {
    formatted_p_value <- "< .001"
  } else {
    formatted_p_value <- paste0("= ", sub("^0+", "0", format(round(p_value, 3), nsmall = 3, scientific = FALSE)))
  }
  return(formatted_p_value)
}

### 8.1.3 Define Variables ----

# Define predictor variables
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Relationship_initiation")

### 8.1.4 Prepare the Dataset ----

# Use your dataset (assuming it's named 'd_with_outliers_with_cov')
dataset <- d_with_outliers_with_cov

# Define all variables to standardize
all_vars_to_standardize <- c(base_predictors, moderators, covariates, manifest_outcomes)

# Exclude dummy variables from standardization
exclude_vars <- c("Gender", "Ethnicity_Chinese")

# Standardize predictors and outcomes
dataset_standardized <- standardize(dataset, all_vars_to_standardize, exclude_vars)

# Convert dummy variables to factors
dataset_standardized <- dataset_standardized %>%
  mutate(
    Gender = factor(Gender),
    Ethnicity_Chinese = factor(Ethnicity_Chinese)
  )

## 8.2 Modeling ---------------------------------------------------------------

### 8.2.1 Fit the Linear Model ----

# Fit the linear model
interaction_model <- lm(Relationship_initiation ~ Helicopter_parenting * Reciprocal_filial_piety + 
                          Gender + Age + Ethnicity_Chinese + Relationship_experience, 
                        data = dataset_standardized)

### 8.2.2 Specify Levels of the Moderator ----

# Specify levels of the moderator
moderator_values <- c(-1, 0, 1)  # -1 SD, Mean, +1 SD

### 8.2.3 Estimate Slopes Using emtrends() ----

# Estimate the slopes
emtrends_result <- emtrends(
  interaction_model, 
  specs = "Reciprocal_filial_piety", 
  var = "Helicopter_parenting",
  at = list(Reciprocal_filial_piety = moderator_values),
  cov.reduce = mean
)

# View the estimated slopes with p-values
slopes_summary <- summary(emtrends_result, infer = c(TRUE, TRUE))
print("Estimated Slopes using emtrends():")
print(slopes_summary)

### 8.2.4 Format P-Values ----

# Format p-values for individual slopes
formatted_p_values <- sapply(slopes_summary$p.value, format_p_value)

### 8.2.5 Perform Pairwise Comparisons of Slopes ----

# Compare the slopes between levels of the moderator with Tukey adjustment
contrast_result <- pairs(emtrends_result, adjust = "tukey")
contrast_summary <- summary(contrast_result)
print("Pairwise Comparisons of Slopes with Tukey Adjustment:")
print(contrast_summary)

# Format p-values for the contrasts
formatted_contrast_p_values <- sapply(contrast_summary$p.value, format_p_value)

## 8.3 Visualization -----------------------------------------------------------

### 8.3.1 Create the Interaction Plot ----

# Generate predicted values for the plot
plot_data <- expand.grid(
  Helicopter_parenting = seq(
    min(dataset_standardized$Helicopter_parenting, na.rm = TRUE), 
    max(dataset_standardized$Helicopter_parenting, na.rm = TRUE), 
    length.out = 100
  ),
  Reciprocal_filial_piety = moderator_values
)

# Set covariates to their mean or reference levels
plot_data$Gender <- factor(levels(dataset_standardized$Gender)[1], levels = levels(dataset_standardized$Gender))
plot_data$Ethnicity_Chinese <- factor(levels(dataset_standardized$Ethnicity_Chinese)[1], levels = levels(dataset_standardized$Ethnicity_Chinese))
plot_data$Age <- mean(dataset_standardized$Age, na.rm = TRUE)
plot_data$Relationship_experience <- mean(dataset_standardized$Relationship_experience, na.rm = TRUE)

# Predict the outcome
plot_data$Relationship_initiation <- predict(interaction_model, newdata = plot_data)

# Convert Reciprocal_filial_piety to factor for plotting
plot_data$Reciprocal_filial_piety <- factor(plot_data$Reciprocal_filial_piety, 
                                            levels = moderator_values,
                                            labels = c("Low (-1 SD)", "Mean", "High (+1 SD)"))

### 8.3.2 Add Annotations to the Plot ----

# Ensure Reciprocal_filial_piety is numeric in slopes_summary
slopes_summary$Reciprocal_filial_piety <- as.numeric(as.character(slopes_summary$Reciprocal_filial_piety))

# Extract beta and p-values using correct column names
beta_low <- round(slopes_summary$Helicopter_parenting.trend[slopes_summary$Reciprocal_filial_piety == -1], 2)
p_val_low <- formatted_p_values[slopes_summary$Reciprocal_filial_piety == -1]

beta_mean <- round(slopes_summary$Helicopter_parenting.trend[slopes_summary$Reciprocal_filial_piety == 0], 2)
p_val_mean <- formatted_p_values[slopes_summary$Reciprocal_filial_piety == 0]

beta_high <- round(slopes_summary$Helicopter_parenting.trend[slopes_summary$Reciprocal_filial_piety == 1], 2)
p_val_high <- formatted_p_values[slopes_summary$Reciprocal_filial_piety == 1]

# Create the interaction plot
interaction_plot <- ggplot(plot_data, aes(x = Helicopter_parenting, y = Relationship_initiation, linetype = Reciprocal_filial_piety)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted")
  ) +
  theme(
    text = element_text(size = 30, family = "Times New Roman"),
    axis.title = element_text(size = 32, family = "Times New Roman"),
    axis.text = element_text(size = 30, family = "Times New Roman"),
    legend.position = "top",
    legend.title = element_text(size = 26, family = "Times New Roman"),
    legend.text = element_text(size = 24, family = "Times New Roman"),
    legend.key.width = unit(2.5, "lines"),
    # legend.key.height = unit(0.2, "lines"),
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) +
  labs(
    x = "Helicopter Parenting",
    y = "Relationship Initiation",
    linetype = "Reciprocal Filial Piety (RFP)"
  )

# Add annotations to the plot with angles
interaction_plot2 <- interaction_plot +
  annotate("text", 
           x = max(plot_data$Helicopter_parenting) * 0.95, 
           y = 0.4, 
           label = paste0("RFP (+1 SD): β = ", beta_high, ", p ", p_val_high), 
           size = 11, 
           color = "black", 
           angle = 9,    # Slightly rotated to match the slope
           hjust = 1, 
           family = "Times New Roman") +
  
  annotate("text", 
           x = max(plot_data$Helicopter_parenting) * 0.95, 
           y = -0.1, 
           label = paste0("RFP (Mean): β = ", beta_mean, ", p ", p_val_mean), 
           size = 11, 
           color = "black", 
           angle = -13,  # Rotated to align with the mean slope
           hjust = 1, 
           family = "Times New Roman") +
  
  annotate("text", 
           x = max(plot_data$Helicopter_parenting) * 0.95, 
           y = -0.5, 
           label = paste0("RFP (-1 SD): β = ", beta_low, ", p ", p_val_low), 
           size = 11, 
           color = "black", 
           angle = -28.5, # More rotated to match the negative slope
           hjust = 1, 
           family = "Times New Roman")

## 8.4 Output ------------------------------------------------------------------

### 8.4.1 Display the Plot ----

# Print the interaction plot
print(interaction_plot2)

### 8.4.2 Save the Plot and Export Results ----

# Ensure the directory exists
if (!dir.exists(simple_slopes_dir)) {
  dir.create(simple_slopes_dir, recursive = TRUE)
}

# Example dimensions
plot_width <- 6  # in inches
plot_height <- 4  # in inches

# Save the plot
ggsave(filename = file.path(simple_slopes_dir, "interaction_plot_d_with_outliers_with_cov.png"), 
       plot = interaction_plot2, 
       width = plot_width, 
       height = plot_height,
       dpi = 300)  # Added dpi for higher resolution

# Calculate the sample size (N) used in the model
N <- nobs(interaction_model)

# Add the sample size to the slopes summary
slopes_summary$N <- N

# Add the sample size to the contrast summary
contrast_summary$N <- N

# Format the slopes summary with formatted p-values
slopes_summary_formatted <- slopes_summary %>%
  mutate(p.value = formatted_p_values)

# Format the contrast summary with formatted p-values
contrast_summary_formatted <- contrast_summary %>%
  mutate(p.value = formatted_contrast_p_values)

# Export slopes results to CSV with N included
write.csv(slopes_summary_formatted, 
          file.path(simple_slopes_dir, "emtrends_slopes_d_with_outliers_with_cov.csv"), 
          row.names = FALSE)

# Export contrast results to CSV with N included
write.csv(contrast_summary_formatted, 
          file.path(simple_slopes_dir, "emtrends_contrasts_d_with_outliers_with_cov.csv"), 
          row.names = FALSE)
