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

# 1.1 Load necessary libraries ----
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

# Register the regular "Times New Roman" font for plotting later
font_add(family = "Times New Roman", regular = "C:/Windows/Fonts/times.ttf")

# Enable showtext for automatic font rendering
showtext_auto()

# 1.2 Set Working Directory and Create Folders ----

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

# 1.3 Load Dataset ----

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

# 2.1 Define functions for outlier detection and handling ----

# Function to identify outliers using z-scores
identify_outliers <- function(dataset, threshold = 3.29) {
  z_scores <- as.data.frame(lapply(dataset, function(column) {
    if (is.numeric(column)) {
      return((column - mean(column, na.rm = TRUE)) / sd(column, na.rm = TRUE))
    } else {
      return(rep(NA, length(column)))
    }
  }))
  
  outliers <- as.data.frame(lapply(z_scores, function(column) {
    if (is.numeric(column)) {
      return(abs(column) > threshold)
    } else {
      return(rep(FALSE, length(column)))
    }
  }))
  
  return(list(outliers_matrix = outliers, z_scores = z_scores))
}

# Function to winsorize outliers and store the original and winsorized values
winsorize_outliers <- function(column) {
  if (is.numeric(column)) {
    lower_bound <- quantile(column, 0.05, na.rm = TRUE)
    upper_bound <- quantile(column, 0.95, na.rm = TRUE)
    column[column < lower_bound & !is.na(column)] <- lower_bound
    column[column > upper_bound & !is.na(column)] <- upper_bound
  }
  return(column)
}

# Function to handle outliers based on parameters
handle_outliers <- function(dataset, handle_outliers = "remove", threshold = 3.29, iterative = FALSE) {
  numeric_dataset <- dataset %>% dplyr::select(where(is.numeric))
  iteration <- 0
  all_outliers <- list()
  outlier_vars <- NULL
  
  repeat {
    iteration <- iteration + 1
    outliers_info <- identify_outliers(numeric_dataset, threshold)
    outliers <- outliers_info$outliers_matrix
    z_scores <- outliers_info$z_scores
    
    if (handle_outliers == "remove") {
      outlier_rows <- apply(outliers, 1, any, na.rm = TRUE)
      all_outliers[[iteration]] <- dataset[outlier_rows, , drop = FALSE]
      
      dataset <- dataset[!outlier_rows, ]
      numeric_dataset <- dataset %>% dplyr::select(where(is.numeric))
      
      if (!iterative || nrow(dataset) == 0 || sum(outlier_rows) == 0) {
        break
      }
    } else if (handle_outliers == "winsorise") {
      outlier_vars <- names(outliers)[apply(outliers, 2, any, na.rm = TRUE)]
      dataset <- dataset %>% mutate(across(all_of(outlier_vars), ~ winsorize_outliers(.)))
      break
    } else {
      break
    }
  }
  
  return(list(cleaned_data = dataset, outliers_info = outliers_info, iterations = iteration, all_outliers = all_outliers, outlier_vars = outlier_vars))
}

# 2.2 Apply outlier handling ----

# Handle outliers (set handle_outliers to "remove", "retain", or "winsorise"; 
# iterative = TRUE for iterative removal using z-score)
result <- handle_outliers(b, handle_outliers = "winsorise", iterative = TRUE)

# Extract cleaned data, outliers, and iteration info
c_cleaned <- result$cleaned_data
outliers_info <- result$outliers_info
outliers <- outliers_info$outliers_matrix
z_scores <- outliers_info$z_scores
iterations <- result$iterations
all_outliers <- result$all_outliers
outlier_vars <- result$outlier_vars

# 2.3 Print results of outlier handling ----

# Print the number of iterations
print(paste("Number of iterations:", iterations))

# Print the outliers identified in each iteration
for (i in seq_along(all_outliers)) {
  cat("Iteration", i, "outliers:\n")
  print(all_outliers[[i]])
}

# Print the names of variables with outliers in the final dataset
print("Variables with outliers in final dataset:")
print(outlier_vars)

# Print the outliers in the final dataset
print("Outliers in the final dataset:")
for (var in outlier_vars) {
  print(paste("Outliers in", var, ":"))
  outlier_indices <- which(outliers[[var]], arr.ind = TRUE)
  outlier_values <- c_cleaned[outlier_indices, var, drop = FALSE]
  print(outlier_values)
}

# Calculate and print the number of different values between b and c_cleaned for outlier_vars
for (var in outlier_vars) {
  diff_count <- sum(b[[var]] != c_cleaned[[var]], na.rm = TRUE)
  print(paste("Number of different values in", var, "between dataset b and cleaned dataset c:", diff_count))
}

# Print summary of the cleaned dataset
print("Summary of cleaned data:")
summary(c_cleaned)

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

# Define the predictor variables
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")
predictors <- c(base_predictors, moderators, covariates, interaction_terms)

# Define the outcome variables
manifest_outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")

# Create and plot for each outcome variable
for (outcome in manifest_outcomes) {
  # Create the formula for the linear model
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  
  # Fit the linear model
  model <- lm(formula, data = c_cleaned)
  
  # Extract residuals
  residuals <- model$residuals
  
  # Residuals vs Fitted Values Plot (for homoscedasticity)
  p1 <- ggplot(data = data.frame(fitted = model$fitted.values, residuals = residuals), aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_smooth(method = "loess", col = "red") +
    ggtitle(paste("Residuals vs Fitted Values for", outcome)) +
    theme_minimal()
  
  # Q-Q Plot (for normality of residuals)
  p2 <- ggplot(data = data.frame(sample = residuals), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot of Residuals for", outcome)) +
    theme_minimal()
  
  # Print the plots
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

# 5. Dealing with Multivariate Outliers =======================================

# 5.1 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")
predictors <- c(base_predictors, moderators, covariates, interaction_terms)
predictors_no_interaction <- c(base_predictors, moderators, covariates)

# Combine all relevant columns into one vector
relevant_columns <- c(base_predictors, moderators, covariates)

# Remove rows with NA in any of the relevant columns
c_cleaned <- c_cleaned[complete.cases(c_cleaned[relevant_columns]), ]
# 5.2 Define functions for Mahalanobis distance calculations ----

# Function to calculate the Mahalanobis distance
calculate_mahalanobis <- function(df, predictors) {
  cov_matrix <- cov(df[, predictors], use = "complete.obs")
  mean_vector <- colMeans(df[, predictors], na.rm = TRUE)
  mahalanobis_distances <- mahalanobis(df[, predictors], mean_vector, cov_matrix)
  return(mahalanobis_distances)
}

# Function to identify outliers based on a chosen significance level
identify_multivariate_outliers <- function(df, predictors, threshold = 0.001) {
  mahalanobis_distances <- calculate_mahalanobis(df, predictors)
  df <- length(predictors)
  cutoff <- qchisq(1 - threshold, df = df)
  outliers <- mahalanobis_distances > cutoff
  print(paste("Degrees of freedom (df):", df))
  print(paste("Threshold (cutoff) for Mahalanobis distance:", cutoff))
  return(outliers)
}

# Function to tag multivariate outliers in the original dataset
tag_multivariate_outliers <- function(original_df, predictors, threshold = 0.001) {
  outliers <- identify_multivariate_outliers(original_df, predictors, threshold)
  original_df$multivariate_outlier <- outliers
  return(original_df)
}

# 5.3 Apply outlier detection to dataset ----

# Add interaction terms to the dataset
c_cleaned2 <- c_cleaned %>%
  mutate(across(all_of(moderators), ~ . * Helicopter_parenting, .names = "Helicopter_parenting*{col}"))

# Tag multivariate outliers including interaction terms
df_tagged <- tag_multivariate_outliers(c_cleaned2, predictors)

# Create datasets with and without multivariate outliers including interaction terms
d_cleaned_step3 <- df_tagged %>%
  filter(is.na(multivariate_outlier) | multivariate_outlier == FALSE) %>%
  dplyr::select(-multivariate_outlier)
d_with_outliers <- df_tagged

# Create a new dataset without interaction terms and tag multivariate outliers
df_tagged_no_interaction <- tag_multivariate_outliers(c_cleaned, predictors_no_interaction)
d_cleaned_step2 <- df_tagged_no_interaction %>%
  filter(is.na(multivariate_outlier) | multivariate_outlier == FALSE) %>%
  dplyr::select(-multivariate_outlier)

# 5.4 Print results of multivariate outlier detection ----

# Print the values of the tagged outliers including interaction terms
tagged_outliers <- df_tagged %>% filter(multivariate_outlier == TRUE)
if (nrow(tagged_outliers) > 0) {
  cat("Values of tagged outliers (including interaction terms):\n")
  print(tagged_outliers)
} else {
  cat("No outliers were tagged (including interaction terms).\n")
}

# Check the number of rows before and after removing outliers including interaction terms
cat("Number of rows before removing outliers (including interaction terms):", nrow(c_cleaned2), "\n")
cat("Number of rows after removing outliers (including interaction terms):", nrow(d_cleaned_step3), "\n")

# Print the values of the tagged outliers without interaction terms
tagged_outliers_no_interaction <- df_tagged_no_interaction %>% filter(multivariate_outlier == TRUE)
if (nrow(tagged_outliers_no_interaction) > 0) {
  cat("Values of tagged outliers (without interaction terms):\n")
  print(tagged_outliers_no_interaction)
} else {
  cat("No outliers were tagged (without interaction terms).\n")
}

# Check the number of rows before and after removing outliers without interaction terms
cat("Number of rows before removing outliers (without interaction terms):", nrow(c_cleaned), "\n")
cat("Number of rows after removing outliers (without interaction terms):", nrow(d_cleaned_step2), "\n")

# 6. Moderation ===============================================================

# 6.1 Define functions for moderation analysis ----

# Function to standardize variables (mean = 0, sd = 1)
standardize <- function(df, variables) {
  df %>% mutate(across(all_of(variables), ~ scale(.) %>% as.vector()))
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

# 6.2 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")

# Combine all variables for listwise deletion
all_vars <- c(base_predictors, moderators, covariates, manifest_outcomes)

# 6.3 Define function to process dataset and fit models ----

# Function to process dataset, perform listwise deletion, standardize variables, and fit hierarchical models
process_and_fit_models <- function(dataset, dataset_name) {
  # Perform listwise deletion
  dataset_listwise <- dataset %>% drop_na(all_of(all_vars))
  
  # Add interaction terms to the dataset for Helicopter_parenting and each moderator
  dataset_interaction <- dataset_listwise
  for (moderator in moderators) {
    interaction_term <- paste("Helicopter_parenting", moderator, sep = "*")
    dataset_interaction <- dataset_interaction %>%
      mutate(!!interaction_term := .data[["Helicopter_parenting"]] * .data[[moderator]])
  }
  
  # Standardize predictors and outcomes
  all_vars_to_standardize <- c(base_predictors, moderators, covariates, manifest_outcomes)
  dataset_standardized <- standardize(dataset_interaction, all_vars_to_standardize)
  
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

# 6.4 Process datasets and combine results ----

# Process and fit models for datasets without and with multivariate outliers
models_d_cleaned <- process_and_fit_models(d_cleaned_step3, "d_cleaned")
models_d_with_outliers <- process_and_fit_models(d_with_outliers, "d_with_outliers")

# Combine the models from both datasets
combined_models <- bind_rows(models_d_cleaned, models_d_with_outliers)

# 6.5 Compare significance levels and export results ----

# Compare significance levels and highlight differences
compare_significance <- function(df) {
  df <- df %>%
    group_by(term, step, outcome) %>%
    mutate(significance_d_cleaned = significance[dataset == "d_cleaned"],
           significance_d_with_outliers = significance[dataset == "d_with_outliers"],
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

# 7.1 Define functions for sensitivity analysis ----

# Function to standardize variables (mean = 0, sd = 1)
standardize <- function(df, variables) {
  df %>% mutate(across(all_of(variables), ~ scale(.) %>% as.vector()))
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

# 7.2 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety", "Authoritarian_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Social_competence", "Relationship_initiation", "Negative_assertion", "Emotional_support", "Self_disclosure", "Conflict_management")
interaction_terms <- paste("Helicopter_parenting", moderators, sep = "*")

# Combine all variables for listwise deletion
all_vars <- c(base_predictors, moderators, covariates, manifest_outcomes)

# 7.3 Define function to process dataset and fit models ----

# Function to process dataset, perform listwise deletion, standardize variables, and fit hierarchical models
process_and_fit_models <- function(dataset, dataset_name) {
  # Perform listwise deletion
  dataset_listwise <- dataset %>% drop_na(all_of(all_vars))
  
  # Add interaction terms to the dataset for Helicopter_parenting and each moderator
  dataset_interaction <- dataset_listwise
  for (moderator in moderators) {
    interaction_term <- paste("Helicopter_parenting", moderator, sep = "*")
    dataset_interaction <- dataset_interaction %>%
      mutate(!!interaction_term := .data[["Helicopter_parenting"]] * .data[[moderator]])
  }
  
  # Standardize predictors and outcomes
  all_vars_to_standardize <- c(base_predictors, moderators, covariates, manifest_outcomes)
  dataset_standardized <- standardize(dataset_interaction, all_vars_to_standardize)
  
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

# 7.4 Process datasets and combine results ----

# Process and fit models for datasets without and with multivariate outliers
models_d_cleaned <- process_and_fit_models(d_cleaned_step3, "d_cleaned_step3")
models_d_with_outliers <- process_and_fit_models(d_with_outliers, "d_with_outliers")

# Combine the models from both datasets
combined_models <- bind_rows(models_d_cleaned, models_d_with_outliers)

# 7.5 Compare significance levels and export results ----

# Compare significance levels and highlight differences
compare_significance <- function(df) {
  df <- df %>%
    group_by(term, step, outcome) %>%
    mutate(significance_d_cleaned = significance[dataset == "d_cleaned_step3"],
           significance_d_with_outliers = significance[dataset == "d_with_outliers"],
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

# 8.1 Define functions for analysis ----

# Function to standardize variables
standardize <- function(df, variables) {
  df %>% mutate(across(all_of(variables), ~ scale(.) %>% as.vector()))
}

# Function to add asterisks based on p-values
add_asterisks <- function(p_value) {
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

# Function to format p-values and omit the leading zero
format_p_value <- function(p_value) {
  if (round(as.numeric(p_value), 3) < 0.001) {
    formatted_p_value <- "< .001"
  } else {
    formatted_p_value <- paste0("= ", sub("^0+", "", format(round(as.numeric(p_value), 3), nsmall = 3, scientific = FALSE)))
  }
  return(formatted_p_value)
}

# 8.2 Define predictor variables ----
base_predictors <- c("Helicopter_parenting")
moderators <- c("Reciprocal_filial_piety")
covariates <- c("Gender", "Age", "Ethnicity_Chinese", "Relationship_experience")
manifest_outcomes <- c("Relationship_initiation")

# 8.3 Define function to process dataset and fit model ----

# Function to process dataset and fit the interaction model
process_and_fit_model <- function(dataset, dataset_name, foldername) {
  # Create the interaction term as an additional covariate
  dataset <- dataset %>%
    mutate(`Helicopter_parenting * Authoritarian_filial_piety` = Helicopter_parenting * Authoritarian_filial_piety)
  
  # Define all variables including the interaction term
  all_vars_to_standardize <- c(base_predictors, moderators, covariates, manifest_outcomes, "Helicopter_parenting * Authoritarian_filial_piety")
  
  # Standardize predictors and outcomes
  dataset <- standardize(dataset, all_vars_to_standardize)
  
  # Fit the linear model
  interaction_model <- lm(Relationship_initiation ~ Helicopter_parenting * Reciprocal_filial_piety + 
                            Gender + Age + Ethnicity_Chinese + Relationship_experience + 
                            `Helicopter_parenting * Authoritarian_filial_piety`, 
                          data = dataset)
  
  # Conduct simple slopes analysis
  sim_slopes_result <- sim_slopes(interaction_model, pred = "Helicopter_parenting", modx = "Reciprocal_filial_piety")
  print(sim_slopes_result)
  
  # Extract simple slopes results
  simple_slopes <- as.data.frame(sim_slopes_result[["slopes"]])
  print("Simple Slopes Dataframe:")
  print(simple_slopes)
  
  # Check if simple_slopes_df is not empty
  if (nrow(simple_slopes) > 0) {
    # Extract and format p-values
    formatted_p_values <- sapply(simple_slopes$p, function(p) {
      formatted_p <- format_p_value(p)
      formatted_p
    })
    print("Formatted P-Values:")
    print(formatted_p_values)
    
    # Create the interaction plot using interact_plot
    interaction_plot <- interact_plot(
      interaction_model,
      pred = "Helicopter_parenting",
      modx = "Reciprocal_filial_piety",
      plot.points = FALSE,
      interval = FALSE,
      vary.lty = TRUE,
      colors = c("black", "black", "black"),
      legend.main = "Reciprocal Filial Piety"
    ) +
      theme(legend.position = "none")  # Disable the legend
    
    # Customize the plot for publication using ggplot2 with Times New Roman font
    interaction_plot <- interaction_plot +
      scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
      theme_minimal() +  # Minimal theme for a clean look
      theme(
        text = element_text(size = 28, family = "Times New Roman"),  # Adjust text size and font
        axis.title = element_text(size = 30, family = "Times New Roman"),  # Axis title size and font
        axis.text = element_text(size = 28, family = "Times New Roman"),  # Axis text size and font
        legend.position = "top",  # Legend at the top
        legend.title = element_text(size = 30, family = "Times New Roman"),  # Legend title size and font
        legend.text = element_text(size = 28, family = "Times New Roman"),  # Legend text size and font
        legend.key.width = unit(2, "lines"),  # Adjust key width
        legend.key.height = unit(0.5, "lines"),  # Adjust key height
        panel.background = element_rect(fill = "white", color = "white"),  # Panel background
        plot.background = element_rect(fill = "white", color = "white"),  # Plot background
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"),  # Add axis lines
        axis.ticks = element_line(color = "black")  # Add axis ticks
      ) +
      labs(
        x = "Helicopter Parenting",
        y = "Relationship Initiation",
        linetype = "Reciprocal Filial Piety"
      )   +
      annotate("text", x = 1.7, y = 0.2, label = paste0("RFP (+1 SD): β = ", round(simple_slopes$Est.[3], 2), ", p ", formatted_p_values[3]), size = 9, color = "black", angle = 2, family = "Times New Roman") +
      annotate("text", x = 1.7, y = -0.10, label = paste0("RFP (Mean): β = ", round(simple_slopes$Est.[2], 2), ", p ", formatted_p_values[2]), size = 9, color = "black", angle = -14, family = "Times New Roman") +
      annotate("text", x = 1.7, y = -0.4, label = paste0("RFP (-1 SD): β = ", round(simple_slopes$Est.[1], 2), ", p ", formatted_p_values[1]), size = 9, color = "black", angle = -28.5, family = "Times New Roman")
    
    # Print the interaction plot
    print(interaction_plot)
    
    # Save the plot with specified dimensions and dpi
    save_plot <- function(plot, filename, width, height, dpi = 300) {
      ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
    }
    
    # Example dimensions
    plot_width <- 6  # in inches
    plot_height <- 4  # in inches
    
    # Save the plot
    save_plot(interaction_plot, file.path(simple_slopes_dir, paste0("interaction_plot_", dataset_name, ".png")), plot_width, plot_height)
    
    # Export simple slopes results to CSV
    write.csv(simple_slopes, file.path(simple_slopes_dir, paste0("simple_slopes_", dataset_name, ".csv")), row.names = FALSE)
  } else {
    print(paste("No simple slopes results for", dataset_name))
  }
}

# 8.4 Process datasets and generate outputs ----

# Process datasets and generate outputs
#process_and_fit_model(d_cleaned_step3, "d_cleaned", foldername)
process_and_fit_model(d_with_outliers, "d_with_outliers", simple_slopes_dir)
