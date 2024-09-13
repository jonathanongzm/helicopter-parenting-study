#' -----------------------------------------------------------------------------
#' Data Cleaning for Helicopter Parenting and Social Competence among Single Malaysian Adults:
#' Filial Piety as a Moderator
#' by Jonathan Z. Ong
#' Last updated: 07 September 2024
#' -----------------------------------------------------------------------------
#' Description:
#' This script cleans and prepares the dataset for the study titled 
#' "Helicopter Parenting and Social Competence among Single Malaysian Adults: 
#' Filial Piety as a Moderator." The script includes data cleaning steps, such as 
#' converting text responses to numeric values, recoding variables, and computing 
#' reverse-scored items, followed by computing composite scores and calculating 
#' Cronbach's alpha for reliability analysis.
#'
#' Sections:
#' 1. Data Processing: Converts text responses to numeric values, recodes variables, 
#'    and computes reverse-scored items.
#' 2. Reliability Analysis: Calculates Cronbach's alpha for various scales and 
#'    saves the results to a CSV file for further review.
#' -----------------------------------------------------------------------------

# Clear all objects from the environment to ensure a clean workspace
rm(list = ls()) 

# Load necessary libraries
library(dplyr)
library(readr)
library(tidyr)
library(psych) # For reliability analysis

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
cronbach_alpha_dir <- file.path(foldername, "cronbach_alpha")

# Create the 'CronbachAlpha' subfolder if it doesn't exist
if (!file.exists(cronbach_alpha_dir)) {
  dir.create(cronbach_alpha_dir)
}

# 1. Data Processing ===========================================================

## 1.1 Load and clean raw data -------------------------------------------------

# Load the raw dataset from CSV file
raw_data <- read_csv("hp_dataset_raw.csv")

## 1.2 Convert text responses to numeric values and recode variables -----------

converted_data <- raw_data %>%
  # Convert text responses to numeric values
  mutate(across(c(Hel_Pr1:Hel_Pr7), 
                ~ recode(.,
                         "Strongly disagree" = 1,
                         "Disagree" = 2,
                         "Undecided" = 3,
                         "Agree" = 4,
                         "Strongly agree" = 5,
                         .default = NA_real_))) %>%
  mutate(across(c(ICQ1:ICQ15), 
                ~ recode(.,
                         "I'm always good at this" = 4,
                         "I'm OK at this" = 3,
                         "I'm only fair at this" = 2,
                         "I'm always poor at this" = 1,
                         .default = NA_real_))) %>%
  mutate(across(c(RecFP_1:RecFP_8, AuthFP_1:AuthFP_8), 
                ~ recode(.,
                         "Extremely unimportant" = 1,
                         "Quite unimportant" = 2,
                         "Slightly unimportant" = 3,
                         "Slightly important" = 4,
                         "Quite important" = 5,
                         "Extremely important" = 6,
                         .default = NA_real_))) %>%
  # Recode Rrel_exp4 to numeric and keep NA as NA
  mutate(Rrel_exp4 = recode(Rrel_exp4,
                            "1 - 6 months" = 1,
                            "7 - 12 months" = 2,
                            "more than 1 year but less than 3 years" = 3,
                            "more than 3 years but less than 5 years" = 4,
                            "more than 5 years but less than 10 years" = 5,
                            "10 years or more" = 6,
                            .missing = 0)) %>% # Not in a relationship
  # Reverse score Hel_Pr3 and Hel_Pr5
  mutate(Hel_Pr3 = if_else(is.na(Hel_Pr3), NA_real_, 6 - Hel_Pr3),
         Hel_Pr5 = if_else(is.na(Hel_Pr5), NA_real_, 6 - Hel_Pr5)) %>%
  # Recode Gender to factor
  mutate(Gender = recode(Gender,
                         "Female" = 1,
                         "Male" = 0,
                         .default = NA_real_)) %>%
  mutate(Gender = factor(Gender, levels = c(0, 1), labels = c("Male", "Female"))) %>%
  # Recode Ethnicity_Chinese to numeric where "Chinese" is 1, others are 0, and NA remains NA
  mutate(Ethnicity_Chinese = recode(Ethnicity, 
                                    "Chinese" = 1,
                                    .default = 0,
                                    .missing = NA_real_)) %>%
  # Convert to factor after recoding
  mutate(Ethnicity_Chinese = factor(Ethnicity_Chinese, levels = c(0, 1), labels = c("Not Chinese", "Chinese")))

## 1.3 Compute Composite Scores ------------------------------------------------

composite_scores <- converted_data %>%
  # Average score for Helicopter Parenting using reverse-scored items
  mutate(Hel_Avg = rowMeans(cbind(Hel_Pr1, Hel_Pr2, Hel_Pr3, Hel_Pr4, Hel_Pr5, Hel_Pr6, Hel_Pr7), na.rm = TRUE),
         
         # Average score for Social Competence
         SCavg = rowMeans(cbind(ICQ1, ICQ2, ICQ3, ICQ4, ICQ5, ICQ6, ICQ7, ICQ8, ICQ9, ICQ10, ICQ11, ICQ12, ICQ13, ICQ14, ICQ15), na.rm = TRUE),
         
         # Average scores for various subdomains
         Initiation = rowMeans(cbind(ICQ1, ICQ2, ICQ3), na.rm = TRUE),
         SelfDisc = rowMeans(cbind(ICQ10, ICQ11, ICQ12), na.rm = TRUE),
         NegAssr = rowMeans(cbind(ICQ4, ICQ5, ICQ6), na.rm = TRUE),
         EmoSup = rowMeans(cbind(ICQ7, ICQ8, ICQ9), na.rm = TRUE),
         ConfMan = rowMeans(cbind(ICQ13, ICQ14, ICQ15), na.rm = TRUE),
         
         # Total scores for Filial Piety dimensions
         RecFP = rowSums(cbind(RecFP_1, RecFP_2, RecFP_3, RecFP_4, RecFP_5, RecFP_6, RecFP_7, RecFP_8), na.rm = TRUE),
         AuthFP = rowSums(cbind(AuthFP_1, AuthFP_2, AuthFP_3, AuthFP_4, AuthFP_5, AuthFP_6, AuthFP_7, AuthFP_8), na.rm = TRUE))

# Save the composite scores to a CSV file in the current directory
write.csv(composite_scores, file = "hp_dataset_cleaned.csv", row.names = FALSE)

# Display the updated dataset with new composite scores
head(composite_scores)
summary(composite_scores)

# 2. Reliability Analysis ======================================================

# Compute Cronbach's alpha for each composite score
cronbach_results <- data.frame(
  Variable = c("Hel_Avg", "SCavg", "Initiation", "SelfDisc", "NegAssr", "EmoSup", "ConfMan", "RecFP", "AuthFP"),
  Full_Name = c(
    "Helicopter Parenting",
    "Social Competence",
    "Initiation Subdomain",
    "Self-Disclosure Subdomain",
    "Negative Assertion Subdomain",
    "Emotional Support Subdomain",
    "Conflict Management Subdomain",
    "Reciprocal Filial Piety",
    "Authoritarian Filial Piety"
  ),
  Cronbach_Alpha = c(
    alpha(cbind(converted_data$Hel_Pr1, converted_data$Hel_Pr2, converted_data$Hel_Pr3,
                converted_data$Hel_Pr4, converted_data$Hel_Pr5, converted_data$Hel_Pr6, 
                converted_data$Hel_Pr7))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ1, converted_data$ICQ2, converted_data$ICQ3,
                converted_data$ICQ4, converted_data$ICQ5, converted_data$ICQ6,
                converted_data$ICQ7, converted_data$ICQ8, converted_data$ICQ9,
                converted_data$ICQ10, converted_data$ICQ11, converted_data$ICQ12,
                converted_data$ICQ13, converted_data$ICQ14, converted_data$ICQ15))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ1, converted_data$ICQ2, converted_data$ICQ3))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ10, converted_data$ICQ11, converted_data$ICQ12))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ4, converted_data$ICQ5, converted_data$ICQ6))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ7, converted_data$ICQ8, converted_data$ICQ9))$total$raw_alpha,
    
    alpha(cbind(converted_data$ICQ13, converted_data$ICQ14, converted_data$ICQ15))$total$raw_alpha,
    
    alpha(cbind(converted_data$RecFP_1, converted_data$RecFP_2, converted_data$RecFP_3, 
                converted_data$RecFP_4, converted_data$RecFP_5, converted_data$RecFP_6, 
                converted_data$RecFP_7, converted_data$RecFP_8))$total$raw_alpha,
    
    alpha(cbind(converted_data$AuthFP_1, converted_data$AuthFP_2, converted_data$AuthFP_3, 
                converted_data$AuthFP_4, converted_data$AuthFP_5, converted_data$AuthFP_6, 
                converted_data$AuthFP_7, converted_data$AuthFP_8))$total$raw_alpha
  )
)

# Reorder columns to have Full_Name first
cronbach_results <- cronbach_results[, c("Full_Name", "Variable", "Cronbach_Alpha")]

# Save the Cronbach's alpha results to a CSV file in the dynamically generated folder
write.csv(cronbach_results, file = file.path(cronbach_alpha_dir, "cronbach_alpha_results.csv"), row.names = FALSE)

# Display message confirming the save
cat("Cronbach's alpha results have been saved")
