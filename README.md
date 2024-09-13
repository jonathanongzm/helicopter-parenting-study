# Helicopter Parenting Study

This repository contains the analysis, scripts, and data for the study titled **"Helicopter Parenting and Social Competence among Single Malaysian Adults: Filial Piety as a Moderator"** by Darlene M. Koh, Jonathan Z. Ong, and Goh Pei Hwa. The study explores how helicopter parenting influences social competence in single Malaysian adults and examines whether different dimensions of filial piety (Reciprocal and Authoritarian) moderate this relationship.

## Repository Structure

The repository is organized as follows:

- **hp_dataset_raw.csv**: The original dataset containing raw data collected for the study.
- **hp_dataset_cleaned.csv**: The cleaned dataset used for analysis after data processing and cleaning.
- **codebook.md**: Contains a detailed description of the dataset, including variable definitions and coding information.
- **results/**: Contains all output files generated from the analyses.
  - **correlations_descriptives/**: Includes correlation matrices and descriptive statistics.
  - **cronbach_alpha/**: Contains reliability analysis results (Cronbach's alpha) for the study scales.
  - **factor_analysis/**: Stores the results of the exploratory factor analysis (EFA).
  - **moderation/**: Holds outputs from moderation analysis.
  - **simple_slopes/**: Stores results of the simple slopes analysis for interpreting moderation effects.
- **scripts/**: Includes all R scripts used for data cleaning, analysis, and visualization.
  - **hp_datacleaning_cronbachalpha.R**: Performs data cleaning, recodes variables, computes reverse-scored items, and calculates Cronbach's alpha for reliability analysis.
  - **hp_primary_analysis.R**: Contains the full analytical workflow for the study, including data preparation, outlier handling, assumption checks, correlation analysis, moderation analysis, and simple slopes analysis.
  - **hp_factor_analysis.R**: Provides supplementary analysis to determine the factor structure of social competence items through exploratory factor analysis (EFA).
- **LICENSE**: The license for the project, specifying terms of use and distribution.
- **README.md**: This file, providing an overview of the repository, its contents, and usage instructions.

## Analysis Overview

The main analyses in this study include:

1. **Data Cleaning**: (`hp_datacleaning_cronbachalpha.R`) Prepares the dataset by converting text responses to numeric values, recoding variables, and computing composite scores. Also calculates Cronbach's alpha for various scales to assess reliability.
2. **Descriptive and Correlation Analysis**: Part of `hp_primary_analysis.R`, examines basic statistics and relationships between variables.
3. **Exploratory Factor Analysis (EFA)**: (`hp_factor_analysis.R`) Identifies the factor structure of social competence items (ICQ1 to ICQ15) to supplement the primary study findings.
4. **Moderation Analysis**: Part of `hp_primary_analysis.R`, conducts hierarchical regression analyses to test the moderating role of Reciprocal and Authoritarian Filial Piety.
5. **Simple Slopes Analysis**: Part of `hp_primary_analysis.R`, further explores moderation effects by examining simple slopes and interaction plots.
6. **Reliability Analysis**: (`hp_datacleaning_cronbachalpha.R`) Calculates Cronbach's alpha to assess the internal consistency of the scales used in the study.

## Getting Started

To replicate the analysis:

1. **Clone this repository** from GitHub.

2. **Install the required R packages**: Ensure that you have the necessary R packages installed (e.g., `dplyr`, `psych`, `tidyverse`, `ggplot2`, etc.).

3. **Run the scripts**:
   - Start with `scripts/hp_datacleaning_cronbachalpha.R` to clean the data and compute reliability metrics.
   - Proceed with `scripts/hp_primary_analysis.R` for the main analysis.
   - Use `scripts/hp_factor_analysis.R` for supplementary factor analysis.

## Prerequisites

- **R**: Version 4.0 or higher
- **RStudio** (optional but recommended)
- **Required R packages**: `dplyr`, `tidyverse`, `psych`, `ggplot2`, and others as specified in each script.

## Contributing

Contributions, feedback, and adaptations are welcome. Please fork the repository and use a pull request to propose changes.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.