#' -----------------------------------------------------------------------------
#' Supplementary Analysis for Helicopter Parenting and Social Competence among Single Malaysian Adults:
#' Filial Piety as a Moderator
#' by Jonathan Z. Ong
#' Last updated: 13 September 2024
#' -----------------------------------------------------------------------------
#' Description:
#' This script provides supplementary analysis for the study titled 
#' "Helicopter Parenting and Social Competence among Single Malaysian Adults: 
#' Filial Piety as a Moderator." The analysis focuses on determining the factor 
#' structure of social competence items (ICQ1 to ICQ15) through exploratory factor 
#' analysis (EFA) to supplement the primary findings of the study. 
#'
#' Sections:
#' 1. Setup: Clears the environment, loads required libraries, and prepares the folder 
#'    structure for saving outputs.
#' 2. Data Preparation: Loads and subsets the data related to social competence.
#' 3. Parallel Analysis: Conducts parallel analysis to determine the number of factors to retain.
#' 4. Exploratory Factor Analysis (EFA): Performs EFA using the principal axis factoring 
#'    method with oblimin rotation based on the number of factors determined by parallel analysis.
#' 5. Visualization and Output: Saves the scree plot from parallel analysis and the factor 
#'    structure plot.
#' -----------------------------------------------------------------------------

# 1. Setup =====================================================================

# Clear all objects from the environment to ensure a clean workspace
rm(list = ls()) 

# Load necessary libraries
library(tidyverse)  # For data manipulation
library(psych)      # For factor analysis

# Create a folder called 'results' if it doesn't exist
if (!dir.exists("results")) {
  dir.create("results")
}

# Generate the folder name using the current date and time
current_date <- format(Sys.time(), "%d-%m-%Y")
foldername <- file.path("results", paste("results", current_date, sep = "_"))

# Create the folder for the current date and time if it doesn't exist
if (!file.exists(foldername)) {
  dir.create(foldername)
}

# Create the 'CronbachAlpha' subfolder and assign to an object
factor_analysis_dir <- file.path(foldername, "factor_analysis")

# Create the 'CronbachAlpha' subfolder if it doesn't exist
if (!file.exists(factor_analysis_dir)) {
  dir.create(factor_analysis_dir)
}

# 2. Data Preparation =========================================================

## 2.1 Load and subset data ---------------------------------------------------
# Read in the cleaned dataset
hp_dataset <- read.csv("hp_dataset_cleaned.csv")

# Select the items related to Social Competence
social_competence_items <- hp_dataset %>%
  dplyr::select(ICQ1, ICQ2, ICQ3, ICQ4, ICQ5, ICQ6, ICQ7, ICQ8, ICQ9, ICQ10, ICQ11, ICQ12, ICQ13, ICQ14, ICQ15)

# 3. Parallel Analysis ========================================================

## 3.1 Conduct parallel analysis to determine the number of factors ------------
parallel_result <- fa.parallel(social_competence_items, fm = "pa", fa = "fa")

# 4. Exploratory Factor Analysis (EFA) ========================================

## 4.1 Perform EFA based on parallel analysis results -------------------------
efa_result <- fa(social_competence_items, nfactors = parallel_result$nfact, rotate = "oblimin", fm = "pa")

# Print the factor analysis results
print(efa_result)

# 5. Visualization and Output =================================================

## 5.1 Save the scree plot from parallel analysis -----------------------------
png(file.path(factor_analysis_dir, "scree_plot.png"))
fa.parallel(social_competence_items, fm = "pa", fa = "fa")
dev.off()

## 5.2 Save the factor structure plot -----------------------------------------
png(file.path(factor_analysis_dir, "factor_structure.png"))
fa.diagram(efa_result)
dev.off()
