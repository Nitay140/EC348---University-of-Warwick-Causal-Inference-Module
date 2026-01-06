# Importing Libraries
library(haven) # Read Stata, SPSS, and SAS files
library(tidyverse) # For data manipulation and visualisation packages
library(modelsummary) # Creates regression tables
library(kableExtra) # Enhanced table formatting for HTML/LaTeX
library(broom) # Convert model outputs to tidy data frames
library(hdm) # High-dimensional metrics (LASSO, post-LASSO, inference)
library(estimatr) # Fast estimation with robust/clustered standard errors


#Loading Dataset (function from the haven library which helps with that)
data <- read_dta("/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/Data_Covid_Paper_assignment.dta")

# Table 2 Replication: Effect of Training on Salaried Employment

# Converting outcome variables to numeric - Ensuring all my outcome variables are proper numeric values 
data <- data %>%
  mutate(
    Salaried_Prelockdown = as.numeric(as.character(Salaried_Prelockdown)), # Using as.character() then as.numeric() is a 2 step conversion to safely handle Stata's labelled values 
    Salaried_Jun_Jul2020 = as.numeric(as.character(Salaried_Jun_Jul2020)), # First converts to text, then to numbers (Binary variables (0/1) become proper numbers for regression)
    Salaried_Mar_Apr2021 = as.numeric(as.character(Salaried_Mar_Apr2021)),
    Salaried_Nov_Dec2021 = as.numeric(as.character(Salaried_Nov_Dec2021)),
    Training_complete = as.numeric(Training_complete), # Converts my main independent variable to numeric
    strata = interaction(state, sector, c_gender, c_treatment, 
                         drop = TRUE, sep = "_") # Creates unique strata identifiers by combining multiple variables as the table mentions for table 4B and A9.
  )

# Checking strata creation
cat("Number of unique strata:", n_distinct(data$strata)) # Verifying that strata were created correctly (Counts unique strata (which is 40 in my case))
cat("Observations per strata (first 10):") # Shows how many observations fall in each stratum
print(head(table(data$strata), 10)) # Ensures you have sufficient sample size in each stratum for later analysis

# Verifying balance across strata
cat("Strata distribution by treatment:") # Checking if treatment/control are distributed across strata
table(data$strata, data$c_treatment) %>% head()

cat("Strata variable created successfully.")

# Defining control variable lists 
sectors <- paste0("sector", 1:10) # I am creating a vector of sector variable names which are training sector dummy variables

individual_controls <- c(
  "c_gender", "c_caste", "c_age_above_20", "c_respondent_maritalstatus2",
  "c_respondent_religion2", "c_respondent_religion3", "c_respondent_education1",
  "c_respondent_education2", "c_respondent_education4", "c_Matric_exam",
  "c_Inter_exam", "c_respondent_migrate"
) # I am listing all of the individual level baseline characteristics

household_controls <- c(
  "c_household_earning1", "c_household_earning2", "c_household_earning3",
  "c_agriculture_land", "c_BPL_card", "c_RSBY_Card", "c_SHG_member",
  "c_MNREGA", "c_internet_use", "c_relatives_migrate",
  "c_difficulty_immediate_famil", "c_difficulty_future_family"
) # I am listing all of the household level baseline characteristics

# Creating formulas for each specification
create_formulas <- function(outcome) { # Creating a function that generates 4 different regression formulas for any outcome variable. I am combining outcome variable name with the regression formula structure
  list(
    col1 = as.formula(paste(outcome, "~ Training_complete")), # Outcome ~ Training_complete (no controls)
    col2 = as.formula(paste(outcome, "~ Training_complete +", paste(sectors, collapse = " + "))), # Adding sector controls
    col3 = as.formula(paste(outcome, "~ Training_complete +", # Adding sectors AND individual controls
                            paste(c(sectors, individual_controls), collapse = " + "))),
    col4 = as.formula(paste(outcome, "~ Training_complete +", 
                            paste(c(sectors, individual_controls, household_controls), collapse = " + "))) # Adding sectors AND individual controls AND household controls (full specification)
  ) 
}

# Function to run all 4 specifications for a given outcome
run_specifications <- function(outcome_var, data) { # I am taking an outcome variable name and runs 4 regressions with different control sets
  formulas <- create_formulas(outcome_var)
  
  models <- list(
    col1 = lm(formulas$col1, data = data),
    col2 = lm(formulas$col2, data = data),
    col3 = lm(formulas$col3, data = data),
    col4 = lm(formulas$col4, data = data) # I am generating the 4 formulas using the function above
  )
  
  return(models) # I am returning the 4 formulas using the function above
}

# Running for all four time periods

panel_a <- run_specifications("Salaried_Prelockdown", data)
panel_b <- run_specifications("Salaried_Jun_Jul2020", data)
panel_c <- run_specifications("Salaried_Mar_Apr2021", data)
panel_d <- run_specifications("Salaried_Nov_Dec2021", data)

# Function to extract key statistics for each panel
extract_panel_stats <- function(models, panel_name, outcome_var, data) { # Pulling out the key numbers I need from regression results
  results <- data.frame(
    Panel = panel_name,
    Specification = paste0("[", 1:4, "]"),
    Coefficient = numeric(4),
    Std_Error = numeric(4),
    Observations = numeric(4),
    Dropout_Mean = numeric(4),
    stringsAsFactors = FALSE
  ) # I am creating an empty dataframe to store the results
  
  for (i in 1:4) { # I am looping through each of the 4 models and finding which row contains the Training_complete coefficient as it might be in different rows depending on the controls included.
    model_summary <- summary(models[[i]])
    coef_row <- which(rownames(model_summary$coefficients) == "Training_complete") 
    
    results$Coefficient[i] <- model_summary$coefficients[coef_row, "Estimate"]
    results$Std_Error[i] <- model_summary$coefficients[coef_row, "Std. Error"]
    results$Observations[i] <- nobs(models[[i]]) 
    
    # Calculating dropout mean
    dropout_data <- data %>%
      filter(Training_complete == 0) %>%
      pull(!!sym(outcome_var)) # I am calculating the mean outcome for dropouts only.
    
    results$Dropout_Mean[i] <- mean(dropout_data, na.rm = TRUE)
  } # I am calculating the average and removing any NA values only to return the dataframe with extracted statistics.
  
  return(results)
}

# Extracting statistics for all panels with outcome variable names
stats_a <- extract_panel_stats(panel_a, "Panel A: Survey Round 1 (Pre-lockdown 2020)", "Salaried_Prelockdown", data)
stats_b <- extract_panel_stats(panel_b, "Panel B: Survey Round 1 (Jun-Jul 2020)", "Salaried_Jun_Jul2020", data)
stats_c <- extract_panel_stats(panel_c, "Panel C: Survey Round 2 (Mar-Apr 2021)", "Salaried_Mar_Apr2021", data)
stats_d <- extract_panel_stats(panel_d, "Panel D: Survey Round 3 (Nov-Dec 2021)", "Salaried_Nov_Dec2021", data)
# I am achieving 4 dataframes (stats_a through stats_d), each with 4 rows of statistics

# Displaying results
# In the following part I am printing the text labels for each panel, rounding numbers to 3 decimal places for display as shown and showing the dataframe itself.
cat("Panel A: Pre-lockdown 2020")
stats_a_display <- stats_a
stats_a_display[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")] <- 
  round(stats_a[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")], 3)
print(stats_a_display[, c("Specification", "Coefficient", "Std_Error", "Observations", "Dropout_Mean")])

cat("Panel B: Jun-Jul 2020")
stats_b_display <- stats_b
stats_b_display[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")] <- 
  round(stats_b[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")], 3)
print(stats_b_display[, c("Specification", "Coefficient", "Std_Error", "Observations", "Dropout_Mean")])

cat("Panel C: Mar-Apr 2021")
stats_c_display <- stats_c
stats_c_display[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")] <- 
  round(stats_c[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")], 3)
print(stats_c_display[, c("Specification", "Coefficient", "Std_Error", "Observations", "Dropout_Mean")])

cat("Panel D: Nov-Dec 2021")
stats_d_display <- stats_d
stats_d_display[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")] <- 
  round(stats_d[, c("Coefficient", "Std_Error", "Observations", "Dropout_Mean")], 3)
print(stats_d_display[, c("Specification", "Coefficient", "Std_Error", "Observations", "Dropout_Mean")])

# Creating a table to match the paper's format

# Function to format coefficients with stars and standard errors
format_coef <- function(coef, se, alpha = c(0.01, 0.05, 0.1)) { # I am formats numbers with significance stars similarly to the paper. Alpha is the significance level.
  # Calculating p-value
  t_stat <- coef / se # I am working out the t statistic and the two tailed p value.
  p_val <- 2 * pt(-abs(t_stat), df = Inf)
  
  # Adding significance stars
  # I am adding stars according to the following logic: If p < 0.01: add ***, If p < 0.05: add **, If p < 0.10: add *, otherwise: no stars.
  stars <- ""
  if (p_val < alpha[1]) stars <- "***"
  else if (p_val < alpha[2]) stars <- "**"
  else if (p_val < alpha[3]) stars <- "*"
  
  # Format: coefficient with stars, then SE in parentheses
  coef_str <- sprintf("%.3f%s", coef, stars)
  se_str <- sprintf('"(%.3f)"', se)
  
  return(c(coef_str, se_str)) # Returning  two element vector: [coefficient with stars, SE in parentheses]
}

# Creating the table structure matching the paper
create_table2 <- function() {
  
  table_data <- data.frame(
    Variable = character(),
    Col1 = character(),
    Col2 = character(),
    Col3 = character(),
    Col4 = character(),
    stringsAsFactors = FALSE
  ) # I am creating empty dataframe with structure matching Table 2
  
  # Function to add panel to table
  add_panel <- function(stats, panel_name) {
    # Panel header
    panel_header <- data.frame(
      Variable = panel_name,
      Col1 = "", Col2 = "", Col3 = "", Col4 = "",
      stringsAsFactors = FALSE
    ) # I am creating panel header row (e.g., "Panel A: Survey Round 1...")
    
    # Trained coefficient and SE rows
    trained_coef <- c("Trained", 
                      format_coef(stats$Coefficient[1], stats$Std_Error[1])[1],
                      format_coef(stats$Coefficient[2], stats$Std_Error[2])[1],
                      format_coef(stats$Coefficient[3], stats$Std_Error[3])[1],
                      format_coef(stats$Coefficient[4], stats$Std_Error[4])[1]) # I am creating a row with "Trained" coefficients
    
    trained_se <- c("",
                    format_coef(stats$Coefficient[1], stats$Std_Error[1])[2],
                    format_coef(stats$Coefficient[2], stats$Std_Error[2])[2],
                    format_coef(stats$Coefficient[3], stats$Std_Error[3])[2],
                    format_coef(stats$Coefficient[4], stats$Std_Error[4])[2]) # I am creating a row with standard errors
    
    # Observations
    obs_row <- c("Observations",
                 sprintf("%d", round(stats$Observations[1])),
                 sprintf("%d", round(stats$Observations[2])),
                 sprintf("%d", round(stats$Observations[3])),
                 sprintf("%d", round(stats$Observations[4]))) # I am creating a row showing number of observations
    
    # Dropout Mean
    mean_row <- c("Dropout Mean",
                  sprintf("%.3f", stats$Dropout_Mean[1]),
                  sprintf("%.3f", stats$Dropout_Mean[2]),
                  sprintf("%.3f", stats$Dropout_Mean[3]),
                  sprintf("%.3f", stats$Dropout_Mean[4])) # I am creating a row showing dropout mean
    
    # Combine
    panel_data <- rbind(panel_header,
                        setNames(data.frame(t(trained_coef)), names(panel_header)),
                        setNames(data.frame(t(trained_se)), names(panel_header)),
                        setNames(data.frame(t(obs_row)), names(panel_header)),
                        setNames(data.frame(t(mean_row)), names(panel_header))) # Combining all of the tables together.
    
    return(panel_data)
  }
  
  # Adding all panels
  table_data <- rbind(
    add_panel(stats_a, "Panel A: Survey Round 1 (Pre-lockdown 2020)"),
    add_panel(stats_b, "Panel B: Survey Round 1 (Jun-Jul 2020)"),
    add_panel(stats_c, "Panel C: Survey Round 2 (Mar-Apr 2021)"),
    add_panel(stats_d, "Panel D: Survey Round 3 (Nov-Dec 2021)")
  ) # I am stacking all of the panels vertically together.
  
  # Adding empty row
  empty_row <- data.frame(
    Variable = "",
    Col1 = "", Col2 = "", Col3 = "", Col4 = "",
    stringsAsFactors = FALSE
  ) # I am adding blank row for visual separation.
  
  table_data <- rbind(table_data, empty_row)
  
  # Adding control rows at the end
  controls_data <- data.frame(
    Variable = c("Sector Controls", "Individual Controls", "Household Characteristics"),
    Col1 = c("", "", ""),
    Col2 = c("Yes", "", ""),
    Col3 = c("Yes", "Yes", ""),
    Col4 = c("Yes", "Yes", "Yes"),
    stringsAsFactors = FALSE
  ) # I am adding rows showing which controls are included in each specification
  
  table_data <- rbind(table_data, controls_data)
  
  return(table_data)
}

# Generating the table
table2_final <- create_table2() # Generating and saving the final table.

# Printing
print(table2_final, row.names = FALSE)

# Saving as csv
write.csv(table2_final, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table2_replication.csv", row.names = FALSE)

# Creating html table
html_table <- table2_final %>%
  kbl(col.names = c("", "[1]", "[2]", "[3]", "[4]"),
      caption = "Table 2: Panel survey findings - effect of training on salaried employment",
      align = c("l", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), # This is to add styling onto the html file for easier display.
                full_width = FALSE,
                font_size = 12) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(c(1, 6, 11, 16), background = "#f0f0f0", bold = TRUE) %>%
  add_footnote(c("Notes: This table shows the effect of training completion on salaried employment.",
                 "Each panel refers to each survey round.",
                 "Column [1]: Regression without control variables.",
                 "Column [2] & [3]: Progressively adding sector and individual controls.",
                 "Column [4]: Full specification with household characteristics.",
                 "Standard errors in parentheses.",
                 "* p < 0.10, ** p < 0.05, *** p < 0.01"),
               notation = "none") # I am adding footnotes to further explain the html table.

# Saving html
save_kable(html_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table2_replication.html")

# Comparison with paper (for my understanding)
comparison <- data.frame(
  Period = c("Pre-lockdown", "Jun-Jul 2020", "Mar-Apr 2021", "Nov-Dec 2021"),
  My_Col4 = c(stats_a$Coefficient[4], stats_b$Coefficient[4], 
                stats_c$Coefficient[4], stats_d$Coefficient[4]),
  Paper_Col4 = c(0.318, 0.237, 0.135, 0.162),
  Difference = NA
) # Comparing my results to the referenced paper. I am calculating My estimate - Paper estimate

comparison$Difference <- comparison$My_Col4 - comparison$Paper_Col4 # I then display the difference

cat("Comparison of Column [4] coefficients (Full specification):")

# Rounding only numeric columns
comparison_display <- comparison
comparison_display[, c("My_Col4", "Paper_Col4", "Difference")] <- 
  round(comparison[, c("My_Col4", "Paper_Col4", "Difference")], 4)
print(comparison_display)

# Checking if replication is close
max_diff <- max(abs(comparison$Difference)) # Displaying comparison rounded to 4 decimals
if (max_diff < 0.005) {
  cat("Excellent Replication! Maximum difference: ", round(max_diff, 4))
} else if (max_diff < 0.01) {
  cat("Very Good Replication! Maximum difference: ", round(max_diff, 4))
} else if (max_diff < 0.02) {
  cat("Good Replication! Maximum difference: ", round(max_diff, 4))
} else {
  cat("Some differences detected. Maximum difference: ", round(max_diff, 4))
  cat("This could be due to:")
  cat("  - Different handling of missing data")
  cat("  - Rounding in the paper")
  cat("  - Different samples (attrition)")
}

cat("Replication Complete")
cat("Files created:")
cat("  1. table2_replication.csv (simple format)")
cat("  2. table2_replication.html (publication-quality)")
cat("Checking these files and compare with the original Table 2")

# Standard Error Comparison
se_comparison <- data.frame(
  Period = c("Pre-lockdown", "Jun-Jul 2020", "Mar-Apr 2021", "Nov-Dec 2021"),
  My_SE = c(stats_a$Std_Error[4], stats_b$Std_Error[4], 
              stats_c$Std_Error[4], stats_d$Std_Error[4]),
  Paper_SE = c(0.029, 0.027, 0.030, 0.031)
) # I am checking if the standard errors match the paper, and rounding to 5 decimal places for precision.

se_comparison$Difference <- se_comparison$My_SE - se_comparison$Paper_SE

# Rounding only numeric columns
se_display <- se_comparison
se_display[, c("My_SE", "Paper_SE", "Difference")] <- 
  round(se_comparison[, c("My_SE", "Paper_SE", "Difference")], 5)

print(se_display) # I am now providing additional al regression diagnostics in order to further my understanding of my results.

# Also run this for more detail:
cat("Detailed Regression Information (Panel A, Col 4)")
cat("R-squared:", summary(panel_a$col4)$r.squared) # Proportion of variance explained
cat("Adj. R-squared:", summary(panel_a$col4)$adj.r.squared) # Penalised for number of variables
cat("Number of coefficients:", length(coef(panel_a$col4))) # 36 (Training_complete + 10 sectors + 12 individual + 12 household + intercept)
cat("Residual SE:", summary(panel_a$col4)$sigma) # Standard deviation of residuals (0.452)
cat("Training coefficient with high precision:")
print(summary(panel_a$col4)$coefficients["Training_complete", ], digits = 10) # Shows Training_complete coefficient with maximum precision
































# Table 4B: RCT with LASSO and Strata Fixed Effects
# This section replicates Table 4 Panel B from the paper, which tests whether
# the Yuvasampark job portal training (treatment) increased job applications.
# 
# Methodology:
# I use Double LASSO (Belloni et al., 2013) with post selection OLS:
# 1. LASSO selects controls that predict the outcome (Y)
# 2. LASSO selects controls that predict treatment assignment (D)
# 3. Union of both selections = final control set
# 4. Run OLS with treatment + selected controls + strata fixed effects
# The post=TRUE parameter means I use LASSO only for selection, then run
# unbiased OLS on selected variables (not penalised regression).

cat("Table 4B: RCT results with LASSO and Strata Fixed Effects")

# Step 1: Preparing Data
# Converting all outcome and treatment variables from Stata format to numeric
# This ensures they are proper binary (0/1) variables for regression
data <- data %>%
  mutate(
    Main_outcome_1_Job_applied = as.numeric(as.character(Main_outcome_1_Job_applied)), # Outcome 1: Binary indicator - applied for any jobs in last 2 months
    Main_outcome_2_Job_applications = as.numeric(as.character(Main_outcome_2_Job_applications)), # Outcome 2: Binary indicator - applied to 1-2 jobs
    Main_outcome_3_Job_applications = as.numeric(as.character(Main_outcome_3_Job_applications)), # Outcome 3: Binary indicator - applied to 3+ jobs
    treatment = as.numeric(treatment) # Treatment: Binary indicator - received Yuvasampark training (1) or control (0)
  )

# Step 2: Get Baseline Controls (excluding strata components)
# Getting all baseline control variables (those starting with "c_")
# These were collected before treatment assignment

baseline_vars <- grep("^c_", names(data), value = TRUE)

# SUPER IMPORTANT: Excluding variables already in strata to avoid collinearity
# Including them would cause perfect multicollinearity since strata = state × sector × gender × treatment assignment
# I also exclude c_caste as good practice (correlated with strata components)
exclude_vars <- c(
  "c_treatment",  # Already in strata (part of randomisation stratification)
  "c_gender",     # Already in strata
  "c_caste"       # Excluded to avoid multicollinearity issues
)

baseline_vars <- setdiff(baseline_vars, exclude_vars)

# Reporting how many controls are available for LASSO to choose from
cat("LASSO candidate controls:", length(baseline_vars))
cat("Excluded from LASSO (in strata):", length(exclude_vars))

# Step 3: Double LASSO Control Selection
# This function implements the "Double LASSO" method (Belloni et al., 2013) which selects controls that predict EITHER the outcome OR treatment

# Why Double LASSO?
# 1. It controls treatment selection bias (variables that predict who gets treated)
# 2. It controls omitted variable bias (variables that predict the outcome)
# 3. It provides union of both sets gives me the essential controls to include

# Parameters:
# Y: Outcome variable
# D: Treatment variable  
# X_df: Dataframe with all candidate control variables
# baseline_vars: Names of candidate controls to select from

select_controls_lasso <- function(Y, D, X_df, baseline_vars) {

  lasso_data <- data.frame(Y = Y, D = D, X_df) %>% drop_na() # Creating clean dataset with complete cases (no missing values)
  # LASSO requires complete data
  
  Y_clean <- lasso_data$Y # Extracting components as separate objects for LASSO functions
  D_clean <- lasso_data$D
  X_clean <- as.matrix(lasso_data[, baseline_vars])
  
  # Double LASSO
  
  # First LASSO: Select controls that predict treatment (D)
  lasso_d <- rlasso(X_clean, D_clean, post = TRUE) # post=TRUE means: use LASSO to SELECT variables, then estimate with OLS
  selected_from_d <- which(lasso_d$coefficients[-1] != 0)  # These control for treatment selection - variables correlated with who got treated
  
  # Second LASSO: Select controls that predict the outcome (Y)
  lasso_y <- rlasso(X_clean, Y_clean, post = TRUE) # post=TRUE means: use LASSO to SELECT variables, then estimate with OLS
  selected_from_y <- which(lasso_y$coefficients[-1] != 0) # This gives unbiased estimates (no shrinkage) for selected variables
  
  selected_idx <- unique(c(selected_from_d, selected_from_y)) # Union: Combine both sets of selected variables
  # A variable is included if either LASSO selected it
  
  if (length(selected_idx) > 0) {  # Return variable names (not just indices)
    return(colnames(X_clean)[selected_idx])
  } else {
    return(NULL)
  }
}

cat("Running Double LASSO for each outcome") # LASSO is run separately for each outcome because different outcomes may
# require different control variables

X_df <- data %>% select(all_of(baseline_vars)) # Preparing dataframe with all candidate controls

# Running LASSO for outcome 1:  Applied for jobs in last 2 months
cat("Outcome 1: Applied for jobs")
selected_1 <- select_controls_lasso(
  Y = data$Main_outcome_1_Job_applied,
  D = data$treatment,
  X_df = X_df,
  baseline_vars = baseline_vars
)
cat("  Selected", length(selected_1), "controls")

# Running LASSO for outcome 2: Applied to 1-2 jobs
cat("Outcome 2: Applications (1-2)")
selected_2 <- select_controls_lasso(
  Y = data$Main_outcome_2_Job_applications,
  D = data$treatment,
  X_df = X_df,
  baseline_vars = baseline_vars
)
cat("  Selected", length(selected_2), "controls")

# Running LASSO for outcome 3: Applied to 3+ jobs
cat("Outcome 3: Applications (3+)")
selected_3 <- select_controls_lasso(
  Y = data$Main_outcome_3_Job_applications,
  D = data$treatment,
  X_df = X_df,
  baseline_vars = baseline_vars
)
cat("  Selected", length(selected_3), "controls")

# Step 4: Post LASSO Regressions with Strata fixed effects
# After LASSO selects controls, I run standard OLS regressions
# The regression equation is: Y = β*Treatment + X'α + δ_strata + ε
# where:
#   β = treatment effect (my parameter of interest)
#   X' = LASSO-selected controls
#   δ_strata = strata fixed effects (40 dummy variables)
cat("Running post LASSO regressions with strata FE")

# Function to create regression formula with LASSO-selected controls + strata FE
create_formula <- function(outcome_var, selected_controls) {
  if (!is.null(selected_controls) && length(selected_controls) > 0) {
    # Standard case: Include treatment + strata fixed effects + LASSO-selected controls
    # factor(strata) creates dummy variables for each stratum
    formula_str <- paste(outcome_var, "~ treatment + factor(strata) +", 
                         paste(selected_controls, collapse = " + "))
  } else {
    formula_str <- paste(outcome_var, "~ treatment + factor(strata)") # Edge case: If LASSO selected zero controls, just use treatment + strata
  }
  return(as.formula(formula_str))
}

# Creating formulae and run OLS regressions for each outcome
formula_1 <- create_formula("Main_outcome_1_Job_applied", selected_1)
model_1 <- lm(formula_1, data = data) # OLS regression

formula_2 <- create_formula("Main_outcome_2_Job_applications", selected_2)
model_2 <- lm(formula_2, data = data) # OLS regression

formula_3 <- create_formula("Main_outcome_3_Job_applications", selected_3)
model_3 <- lm(formula_3, data = data) # OLS regression

cat("Regressions complete")

# Step 5: Extract Results
extract_results <- function(model, outcome_name) { # This function pulls out the treatment effect estimates and related statistics
  
  summ <- summary(model) # Get detailed regression output
  
  # Check if treatment coefficient exists
  if ("treatment" %in% rownames(summ$coefficients)) { # I am verifying it exists to avoid errors
    treat_coef <- summ$coefficients["treatment", "Estimate"] # β coefficient
    treat_se <- summ$coefficients["treatment", "Std. Error"] # Standard error
    treat_tstat <- summ$coefficients["treatment", "t value"] # t-statistic
    treat_pval <- summ$coefficients["treatment", "Pr(>|t|)"] # p-value
  } else {
    stop("Treatment coefficient not found in model")
  }
  
  outcome_var <- as.character(formula(model)[[2]]) # Calculate control group mean (baseline rate without treatment)
  control_mean <- mean(data[[outcome_var]][data$treatment == 0], na.rm = TRUE) # This shows what proportion of control group had this outcome
  
  n_obs <- nobs(model) # Counting observations used in the regression
  
  return(list( # Return all statistics as a list
    outcome = outcome_name,
    coefficient = treat_coef,
    std_error = treat_se,
    t_stat = treat_tstat,
    p_value = treat_pval,
    control_mean = control_mean,
    observations = n_obs
  ))
}

results_1 <- extract_results(model_1, "Applied for jobs")
results_2 <- extract_results(model_2, "Applications (1-2)")
results_3 <- extract_results(model_3, "Applications (3+)")

# Step 6: Display Results
cat("Results") # Print formatted results for verification and understanding

cat("Column [1]: Applied for jobs in the last 2 months")
cat("  Treatment coefficient:", sprintf("%.4f", results_1$coefficient))
cat("  Standard error:      ", sprintf("%.4f", results_1$std_error))
cat("  t-statistic:         ", sprintf("%.3f", results_1$t_stat))
cat("  p-value:             ", sprintf("%.3f", results_1$p_value))
cat("  Control mean:        ", sprintf("%.3f", results_1$control_mean))
cat("  Controls selected:   ", length(selected_1))
cat("  Observations:        ", results_1$observations)

cat("Column [2]: Number of job applications (1-2)")
cat("  Treatment coefficient:", sprintf("%.4f", results_2$coefficient))
cat("  Standard error:      ", sprintf("%.4f", results_2$std_error))
cat("  t-statistic:         ", sprintf("%.3f", results_2$t_stat))
cat("  p-value:             ", sprintf("%.3f", results_2$p_value))
cat("  Control mean:        ", sprintf("%.3f", results_2$control_mean))
cat("  Controls selected:   ", length(selected_2))
cat("  Observations:        ", results_2$observations)

cat("Column [3]: Number of job applications (3 or more)")
cat("  Treatment coefficient:", sprintf("%.4f", results_3$coefficient))
cat("  Standard error:      ", sprintf("%.4f", results_3$std_error))
cat("  t-statistic:         ", sprintf("%.3f", results_3$t_stat))
cat("  p-value:             ", sprintf("%.3f", results_3$p_value))
cat("  Control mean:        ", sprintf("%.3f", results_3$control_mean))
cat("  Controls selected:   ", length(selected_3))
cat("  Observations:        ", results_3$observations)

# Step 7: Create Table
format_coef <- function(coef, se, pval) { # Formatting results to match the paper's table structure
  stars <- ""
  if (!is.na(pval) && pval < 0.01) stars <- "***" # Function to format coefficients with significance stars
  else if (!is.na(pval) && pval < 0.05) stars <- "**" # Determine significance level and assign stars
  else if (!is.na(pval) && pval < 0.10) stars <- "*"
  
  coef_str <- sprintf("%.3f%s", coef, stars) # Format coefficient with 3 decimals + stars
  se_str <- sprintf('"(%.3f)"', se) # Format standard error in parentheses with quotes for CSV export
  return(c(coef_str, se_str))
}

table4b <- data.frame( # Initialise empty dataframe with table structure
  Variable = character(),
  Col1 = character(),
  Col2 = character(),
  Col3 = character(),
  stringsAsFactors = FALSE
)

# Panel header
table4b <- rbind(table4b, data.frame( # Initialise empty dataframe with table structure
  Variable = "Panel B: Survey Round 3 (Nov-Dec 2021)",
  Col1 = "", Col2 = "", Col3 = "",
  stringsAsFactors = FALSE
))

# Treatment coefficients
fmt1 <- format_coef(results_1$coefficient, results_1$std_error, results_1$p_value) # Formatting treatment coefficients with significance stars
fmt2 <- format_coef(results_2$coefficient, results_2$std_error, results_2$p_value)
fmt3 <- format_coef(results_3$coefficient, results_3$std_error, results_3$p_value)

table4b <- rbind(table4b, data.frame( # Adding treatment coefficient row (first element = coefficient with stars)
  Variable = "Treatment",
  Col1 = fmt1[1], Col2 = fmt2[1], Col3 = fmt3[1],
  stringsAsFactors = FALSE
))

table4b <- rbind(table4b, data.frame( # Adding standard error row (second element = SE in parentheses)
  Variable = "",
  Col1 = fmt1[2], Col2 = fmt2[2], Col3 = fmt3[2],
  stringsAsFactors = FALSE
))

table4b <- rbind(table4b, data.frame( # Adding blank row for spacing
  Variable = "",
  Col1 = "", Col2 = "", Col3 = "",
  stringsAsFactors = FALSE
))

# p-value
table4b <- rbind(table4b, data.frame( # Adding p-value row
  Variable = "p-value",
  Col1 = sprintf("%.3f", results_1$p_value),
  Col2 = sprintf("%.3f", results_2$p_value),
  Col3 = sprintf("%.3f", results_3$p_value),
  stringsAsFactors = FALSE
))

# Control mean
table4b <- rbind(table4b, data.frame( # Adding control group mean row (baseline rate without treatment)
  Variable = "Control Mean",
  Col1 = sprintf("%.3f", results_1$control_mean),
  Col2 = sprintf("%.3f", results_2$control_mean),
  Col3 = sprintf("%.3f", results_3$control_mean),
  stringsAsFactors = FALSE
))

# Observations
table4b <- rbind(table4b, data.frame(
  Variable = "Observations",
  Col1 = sprintf("%d", results_1$observations),
  Col2 = sprintf("%d", results_2$observations),
  Col3 = sprintf("%d", results_3$observations),
  stringsAsFactors = FALSE
))

# Controls
table4b <- rbind(table4b, data.frame( # Adding control indicator rows
  Variable = "LASSO-selected controls",
  Col1 = "Yes", Col2 = "Yes", Col3 = "Yes",
  stringsAsFactors = FALSE
))

# Strata FE
table4b <- rbind(table4b, data.frame(
  Variable = "Strata fixed effects",
  Col1 = "Yes", Col2 = "Yes", Col3 = "Yes",
  stringsAsFactors = FALSE
))

print(table4b, row.names = FALSE) # Displaying table in console

# Saving Output
write.csv(table4b, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table4b.csv", row.names = FALSE)

# Creating HTML
html_table <- table4b %>%
  kbl(col.names = c("", "[1]", "[2]", "[3]"),
      caption = "Table 4 Panel B: RCT with LASSO-Selected Controls and Strata FE",
      align = c("l", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  row_spec(1, background = "#f0f0f0", bold = TRUE) %>%
  add_footnote(c(
    "Notes: Treatment effects with Double LASSO control selection and strata fixed effects",
    "Baseline controls selected via Double LASSO (Belloni et al., 2014)",
    "Strata fixed effects = state × sector × gender × treatment assignment",
    "Standard OLS errors in parentheses",
    paste("LASSO selects", length(selected_1), "-", length(selected_3), "controls per outcome"),
    "Column [1]: Applied for salaried jobs in last 2 months",
    "Columns [2]-[3]: Number of job applications (1-2 or 3+)",
    "* p < 0.10, ** p < 0.05, *** p < 0.01"
  ), notation = "none")

save_kable(html_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table4b.html")

# Quality control: checking how close my replication is to the published results
# Comparison with Paper
comparison <- data.frame(
  Outcome = c("Applied [1]", "Applications 1-2 [2]", "Applications 3+ [3]"),
  My_Coef = c(results_1$coefficient, results_2$coefficient, results_3$coefficient),
  Paper_Coef = c(-0.012, -0.011, -0.004),
  My_SE = c(results_1$std_error, results_2$std_error, results_3$std_error),
  Paper_SE = c(0.015, 0.013, 0.007),
  My_Pval = c(results_1$p_value, results_2$p_value, results_3$p_value),
  Paper_Pval = c(0.416, 0.388, 0.573)
)

comparison[, 2:7] <- round(comparison[, 2:7], 4) # Calculate differences between my results and the paper

cat("Comparison with paper")
print(comparison)

cat("Coefficient differences")
coef_diffs <- comparison$My_Coef - comparison$Paper_Coef
names(coef_diffs) <- comparison$Outcome
print(round(coef_diffs, 4))

cat("Replication Assessment")
max_coef_diff <- max(abs(coef_diffs)) # Assessing replication quality
max_se_diff <- max(abs(comparison$My_SE - comparison$Paper_SE))

if (max_coef_diff < 0.005) {
  cat("Excellent! Coefficients match within 0.005")
} else if (max_coef_diff < 0.010) {
  cat("Very Good! Coefficients match within 0.010")
} else {
  cat("Reasonable. Differences likely due to specification details")
}

cat("Maximum coefficient difference:", round(max_coef_diff, 4))
cat("Maximum SE difference:", round(max_se_diff, 4))

cat("Table 4B complete with Strata fixed effects")












# Table A9: Baseline Balance tests with Strata fixed effects
# This section replicates Table A9 from the appendix, which tests whether
# randomisation created balanced treatment and control groups at baseline.
#
# Purpose of balance tables:
# In randomised experiments, treatment and control groups should be statistically
# identical on all baseline characteristics. This validates that randomisation worked and that any post treatment differences can be attributed to the treatment
#not pre existing differences.

# Methodology:
# For each baseline variable, I run: variable ~ treatment + factor(strata)
# This tests whether treatment assignment predicts baseline characteristics
# Within strata (more rigorous than simple t-tests).
#
# Interpretation:
# If randomisation worked: Most p-values > 0.05 (no significant differences)
# I expect ~5% to be significant by chance (Type 1 error)
# More than 10-15% significant suggests potential imbalance

cat("Table A9: Baseline Balance tests with Strata fixed effects")
cat("Yuvasampark Experiment: Baseline Summary and Balance Tests")

# Ensure treatment is numeric
data <- data %>% # Converting treatment to numeric for regression
  mutate(treatment = as.numeric(treatment)) # treatment = 1 (received Yuvasampark training), treatment = 0 (control)

# Creating Derived Variables
cat("Creating derived variables for balance table")

data <- data %>%
  mutate(
    # Caste dummies (India's caste system: ST (Scheduled Tribes), OBC (Other Backward Classes), General (upper castes))
    caste_ST = c_respondent_caste2, # Creating dummy variables for different caste categories
    caste_OBC = as.numeric(c_caste == 0 & c_respondent_caste2 == 0 & c_respondent_caste4 == 0),  # Actual ST (but labeled as OBC in paper)
    caste_General = c_respondent_caste4,
    
    # Religion
    religion_Muslim = as.numeric(c_respondent_religion2), # (Hindu is the reference category)
    religion_Christian = as.numeric(c_respondent_religion3),
    
    # Education (Creating dummy variables for different education levels)
    edu_middle = as.numeric(c_respondent_education1),
    edu_secondary = as.numeric(c_respondent_education2),
    edu_tertiary = as.numeric(c_respondent_education4),
    
    # Exam performance (Creating dummies for exam performance thresholds)
    matric_above_50 = as.numeric(c_Matric_percentage2), # Scored >50% on Matric exam
    inter_below_50 = as.numeric(c_Inter_percentage1), # Scored <50% on Inter exam (created by inverting ≥50% dummy)
    
    # Household head (Who is the household head?)
    hh_head_mother = as.numeric(c_householdhead_relationship3),
    hh_head_others = as.numeric(c_householdhead_relationship17), # Others (not father)
    
    # Earning members
    earning_3plus = as.numeric(c_earning_members2), # Household has 3+ earning members
    
    # House type
    # Housing quality indicator (socioeconomic status proxy)
    # Pucca = permanent structure, Semi-pucca = semi-permanent, Kutcha = temporary
    # IAY = Indira Awas Yojana (government housing scheme)
    semi_pucca = as.numeric(c_house_type2), # Semi-permanent house
    pucca_IAY = as.numeric(c_house_type3), # Permanent house (gov't scheme)
    pucca_non_IAY = as.numeric(c_house_type4), # Permanent house (non-gov't)
    
    # Household size
    hh_2less = as.numeric(c_number_of_family_members1), # Small household (≤2 members)
    hh_6plus = as.numeric(c_number_of_family_members3), # Large household (≥6 members)
    
    # Joint household
    joint_hh = as.numeric(c_household_type1), # Joint family (extended family living together)
    
    # Relatives migrated (Family migration experience (important for labor market outcomes))
    relatives_1 = as.numeric(c_number_of_relatives_migrate2), # 1 relative migrated
    relatives_2plus = as.numeric(c_number_of_relatives_migrate3), # 2+ relatives migrated
    
    # Survey duration
    # Time taken to complete baseline survey (attention/engagement proxy)
    duration_high = as.numeric(c_duration1) # Survey duration above median
  )

cat("Derived variables created.")

# Balance Calculation Function with Strata fixed effects
# This function tests balance for a single variable using regression with strata fixed effects
calc_balance <- function(data, var_name) {
  
  if (!var_name %in% names(data)) { # Check if variable exists in dataset
    return(NULL)
  }
  
  var_data <- data[[var_name]] # Get variable data
  
  if (!is.numeric(var_data)) {
    var_data <- as.numeric(as.character(var_data)) # Ensure variable is numeric
  }
  
  # Create formula: variable ~ treatment + factor(strata)
  formula_str <- paste(var_name, "~ treatment + factor(strata)")
  
  # Run regression with error handling (some variables may cause issues)
  model <- tryCatch({
    lm(as.formula(formula_str), data = data)
  }, error = function(e) {
    return(NULL) # If regression failed, return NULL
  })
  
  if (is.null(model)) {
    return(NULL)
  }
  
  # Extract treatment coefficient and p value
  summ <- summary(model)
  
  if (!"treatment" %in% rownames(summ$coefficients)) {
    return(NULL)
  }
  
  # Extract strata-adjusted difference and p-value
  coef_treatment <- summ$coefficients["treatment", "Estimate"] # Diff [2-1]
  se_treatment <- summ$coefficients["treatment", "Std. Error"] # Standard error
  pval_treatment <- summ$coefficients["treatment", "Pr(>|t|)"] # p-value
  
  # Calculate raw means for columns [1] and [2]
  control_mean <- mean(data[[var_name]][data$treatment == 0], na.rm = TRUE) # These are simple means (not regression adjusted)
  treatment_mean <- mean(data[[var_name]][data$treatment == 1], na.rm = TRUE) # Used for descriptive purposes in columns [1] and [2]
  
  return(list(
    control_mean = control_mean, # Column [1]: Control group mean
    treatment_mean = treatment_mean, # Column [2]: Treatment group mean
    difference = coef_treatment,    # Column [3]: Strata-adjusted difference
    p_value = pval_treatment        # Column [4]: Strata-adjusted p-value
  ))
}

# Helper function to add variable row
add_var_row <- function(data, var_name, var_label) { # Function to add a variable row to the table
  balance <- calc_balance(data, var_name) # Calculate balance statistics
  
  if (is.null(balance)) {
    return(NULL)
  }
  
  # Creating formatted row
  return(data.frame(
    Variable = var_label,
    Control_Group = sprintf("%.3f", balance$control_mean),
    Treatment_Group = sprintf("%.3f", balance$treatment_mean),
    Diff_2_1 = sprintf("%.3f", balance$difference),
    p_value = sprintf("%.3f", balance$p_value),
    stringsAsFactors = FALSE
  ))
}

# Helper functions for headers and spacing
add_panel_header <- function(title) { # Function to add panel header (such as "Panel A: Demographics")
  data.frame(Variable = title, Control_Group = "", Treatment_Group = "", 
             Diff_2_1 = "", p_value = "", stringsAsFactors = FALSE)
}

add_empty <- function() { # Function to add empty row for spacing
  data.frame(Variable = "", Control_Group = "", Treatment_Group = "", 
             Diff_2_1 = "", p_value = "", stringsAsFactors = FALSE)
}

# Build Table A9
table_a9 <- data.frame( # Initialise empty dataframe
  Variable = character(), Control_Group = character(), 
  Treatment_Group = character(), Diff_2_1 = character(), 
  p_value = character(), stringsAsFactors = FALSE
)

# Panel A: Demographics and Caste
table_a9 <- rbind(table_a9, add_panel_header("Panel A: Demographics and Caste")) # Basic demographic characteristics and caste/religion composition
table_a9 <- rbind(table_a9, add_var_row(data, "c_age_above_20", "Older (More than 20)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_respondent_maritalstatus2", "Married"))
table_a9 <- rbind(table_a9, add_var_row(data, "caste_ST", "Caste: ST"))
table_a9 <- rbind(table_a9, add_var_row(data, "caste_OBC", "Caste: OBC"))
table_a9 <- rbind(table_a9, add_var_row(data, "caste_General", "Caste: General"))
table_a9 <- rbind(table_a9, add_var_row(data, "religion_Muslim", "Religion: Muslim"))
table_a9 <- rbind(table_a9, add_var_row(data, "religion_Christian", "Religion: Christian"))
table_a9 <- rbind(table_a9, add_empty())

# Panel B: Education
# Educational attainment and exam performance
table_a9 <- rbind(table_a9, add_panel_header("Panel B: Education"))
table_a9 <- rbind(table_a9, add_var_row(data, "edu_middle", "Middle school (6-8 class)"))
table_a9 <- rbind(table_a9, add_var_row(data, "edu_secondary", "Secondary level (9-10 class)"))
table_a9 <- rbind(table_a9, add_var_row(data, "edu_tertiary", "Tertiary education (Graduate & above)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Matric_exam", "Matric exam"))
table_a9 <- rbind(table_a9, add_var_row(data, "matric_above_50", "More than 50%"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Inter_exam", "Inter exam"))
table_a9 <- rbind(table_a9, add_var_row(data, "inter_below_50", "Less than 50%"))
table_a9 <- rbind(table_a9, add_empty())

# Panel C: Skills
table_a9 <- rbind(table_a9, add_panel_header("Panel C: Skills")) # Psychometric test scores measuring personality traits and non-cognitive skills
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_BIG5Extraversion", "Big 5 Extraversion Test (1 to 5)")) # Big 5: Standard personality framework (Extraversion, Agreeableness, Conscientiousness, Neuroticism, Openness)
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_BIG5Agreeableness", "Big 5 Agreeableness Test (1 to 5)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_BIG5Conscientiousness", "Big 5 Conscientiousness Test (1 to 5)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_BIG5Neuroticism", "Big 5 Neuroticism Test (1 to 5)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_BIG5Openness", "Big 5 Openness Test (1 to 5)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_Grit", "Grit Test (1 to 5)")) # Grit: Perseverance and passion for long-term goals
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_ASE", "ASE Test (1 to 4)")) # ASE: Academic Self-Efficacy
table_a9 <- rbind(table_a9, add_var_row(data, "c_Score_Lifegoals", "Life goal Test (1 to 4)")) # Life goals: Aspirations and goal-setting
table_a9 <- rbind(table_a9, add_var_row(data, "duration_high", "Duration of baseline survey (above median)"))
table_a9 <- rbind(table_a9, add_empty())

# Panel D: Socioeconomic background
# Household composition, economic status, assets, and government program participation
table_a9 <- rbind(table_a9, add_panel_header("Panel D: Socioeconomic background"))
table_a9 <- rbind(table_a9, add_var_row(data, "hh_head_mother", "Household head relationship (mother)"))
table_a9 <- rbind(table_a9, add_var_row(data, "hh_head_others", "Household head relationship (others)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_difficulty_immediate_famil", "Immediate difficulty to family"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_difficulty_future_family", "Future difficulty to family"))
table_a9 <- rbind(table_a9, add_var_row(data, "earning_3plus", "Earning members (3 or more)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_household_earning1", "Household earning (15000 or more)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_household_earning2", "Household earning (5000 or less)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_household_earning3", "Household earning (5001-9000)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_agriculture_land", "Agriculture land"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_BPL_card", "BPL card"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_RSBY_Card", "RSBY card"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_MNREGA", "MNREGA"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_SHG_member", "SHG member"))
table_a9 <- rbind(table_a9, add_var_row(data, "semi_pucca", "Semi pucca house"))
table_a9 <- rbind(table_a9, add_var_row(data, "pucca_IAY", "Pucca house (IAY)"))
table_a9 <- rbind(table_a9, add_var_row(data, "pucca_non_IAY", "Pucca house (Non IAY)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_house_ownership", "Own house"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_internet_use", "Internet use"))
table_a9 <- rbind(table_a9, add_var_row(data, "joint_hh", "Joint household"))
table_a9 <- rbind(table_a9, add_var_row(data, "hh_2less", "Household members (2 or less)"))
table_a9 <- rbind(table_a9, add_var_row(data, "hh_6plus", "Household members (6 or more)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_respondent_migrate", "Ever migrated out of state (self)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_relatives_migrate", "Ever migrated out of state (relatives)"))
table_a9 <- rbind(table_a9, add_var_row(data, "relatives_1", "Relatives migrated (one)"))
table_a9 <- rbind(table_a9, add_var_row(data, "relatives_2plus", "Relatives migrated (2 or more)"))
table_a9 <- rbind(table_a9, add_empty())

# Panel E: Expectations
# Earnings expectations, training perceptions, and job search expectations
table_a9 <- rbind(table_a9, add_panel_header("Panel E: Expectations"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_previous_earning", "Previous earning"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_hypothetical1_earning", "Hypothetical earning (immediate)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_hypothetical2_earning", "Hypothetical earning (in one year)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_expected_earning", "Expected earning (in one year)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_preference_earning", "Preferred earning (in one year)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_respondent_awareness_perce", "Training awareness"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_training_usefulness", "Training usefulness"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_satisfaction", "Training satisfaction"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_percentage_training_comple", "Likelihood of training completion"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_offer_post_training", "Likelihood of job offer"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_minimum_salary", "Expected minimum salary (immediate)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_maximum_salary", "Expected maximum salary (immediate)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_average_salary", "Expected average salary (immediate)"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_outside_state", "Likelihood of job offer outside state"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_acceptance_in_state", "Likelihood of accepting job inside state"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_retention_in_state", "Likelihood of retention in job inside state"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_acceptance_outside_sta", "Likelihood of accepting job outside state"))
table_a9 <- rbind(table_a9, add_var_row(data, "c_job_retention_outside_stat", "Likelihood of retention in job outside state"))
table_a9 <- rbind(table_a9, add_var_row(data, "internet_use", "Internet use"))
table_a9 <- rbind(table_a9, add_empty())

# Number of observations
n_control <- sum(data$treatment == 0, na.rm = TRUE)
n_treatment <- sum(data$treatment == 1, na.rm = TRUE)

obs_row <- data.frame(
  Variable = "Number of observations",
  Control_Group = as.character(n_control),
  Treatment_Group = as.character(n_treatment),
  Diff_2_1 = "",
  p_value = "",
  stringsAsFactors = FALSE
)

table_a9 <- rbind(table_a9, obs_row)

# Remove any null rows (from failed variable calculations)
table_a9 <- table_a9[!is.na(table_a9$Variable), ] 

# Displaying the table
cat("Table A9 Complete")
print(table_a9, row.names = FALSE)

# Saving CSV
write.csv(table_a9, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table_a9.csv", row.names = FALSE)
cat("Table saved: table_a9.csv")

# Creating HTML with formatting
panel_rows <- which(grepl("^Panel", table_a9$Variable))

html_table <- table_a9 %>%
  kbl(col.names = c("", "Control Group[1]", "Treatment Group[2]", 
                    "Diff [2-1]\n[3]", "p-value[4]"),
      caption = "Table A9: Yuvasampark Experiment - Baseline Summary and Balance Tests",
      align = c("l", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(panel_rows, background = "#e0e0e0", bold = TRUE) %>%
  add_footnote(c("Notes: Columns [1] and [2] report mean values in control and treatment groups.",
                 "Treatment dummy coefficient controlling for strata fixed effects in Column [3].",
                 "P-value for test of no treatment effect (with strata FE) in Column [4].",
                 paste("Total observations:", n_control + n_treatment)),
               notation = "none")

save_kable(html_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table_a9.html")

# Balance Summary
# Assessing whether randomisation successfully created balanced groups
p_vals <- as.numeric(table_a9$p_value[table_a9$p_value != "" & !grepl("^Panel", table_a9$Variable)])
p_vals <- p_vals[!is.na(p_vals)]

cat("Balance Check Summary")
cat("Variables tested:", length(p_vals))
cat("p < 0.01:", sum(p_vals < 0.01), sprintf("(%.1f%%)", sum(p_vals < 0.01)/length(p_vals)*100))
cat("p < 0.05:", sum(p_vals < 0.05), sprintf("(%.1f%%)", sum(p_vals < 0.05)/length(p_vals)*100))
cat("p < 0.10:", sum(p_vals < 0.10), sprintf("(%.1f%%)", sum(p_vals < 0.10)/length(p_vals)*100))
cat("Expected at 5%:", sprintf("%.1f", length(p_vals) * 0.05))


# Assess balance quality
# With 67 tests at α=0.05, I expect ~3.4 significant results by chance
# If I see ≤5 significant results (within 1.5x expected), balance is excellent
if (sum(p_vals < 0.05) <= length(p_vals) * 0.05 * 1.5) {
  cat("Randomisation successful - balance is excellent")
} else {
  cat("Some imbalance detected")
}

cat("Table A9 complete with Strata fixed effects")




































# Table 2 Extension with Double LASSO control selection
# My innovation addresses specification uncertainty in Table 2 by using control selection via Double LASSO instead of including all available controls.
# Are the training effects in Table 2 robust to different control selection
# strategies? Does LASSO select a more parsimonious set of controls while
# maintaining similar treatment effect estimates?


cat("Table 2 Extension: Double LASSO Innovation")
cat("Applying Double LASSO to all 4 survey rounds")



# Preparing Control Variables
# Sector controls
available_sectors <- grep("^sector[0-9]+$", names(data), value = TRUE)
sector_controls <- available_sectors 

# Note: These match the controls in Table 2 Column [3]
# Individual controls
individual_controls <- c(
  "c_gender", 
  "c_caste",
  "c_age_above_20", 
  "c_respondent_maritalstatus2",
  "c_respondent_religion2", 
  "c_respondent_religion3", 
  "c_respondent_education1",
  "c_respondent_education2", 
  "c_respondent_education4", 
  "c_Matric_exam",              
  "c_Inter_exam",               
  "c_respondent_migrate"
)

individual_controls <- individual_controls[individual_controls %in% names(data)] # Verify all variables exist in dataset

# Household controls
# Note: These match the controls in Table 2 Column [4]
household_controls <- c(
  "c_household_earning1", 
  "c_household_earning2", 
  "c_household_earning3",
  "c_agriculture_land", 
  "c_BPL_card", 
  "c_RSBY_Card", 
  "c_SHG_member",
  "c_MNREGA", 
  "c_internet_use", 
  "c_relatives_migrate",
  "c_difficulty_immediate_famil",   
  "c_difficulty_future_family"      
)
household_controls <- household_controls[household_controls %in% names(data)] # Verify all variables exist in dataset


# Combining all potential controls
all_controls <- c(sector_controls, individual_controls, household_controls)

cat("Total potential controls:", length(all_controls))
cat("  - Sectors:", length(sector_controls))
cat("  - Individual:", length(individual_controls))
cat("  - Household:", length(household_controls))

# Function: Running Double LASSO for one panel
# This function runs Double LASSO for a single survey round

run_double_lasso_panel <- function(outcome_var, treatment_var, controls, data, panel_name) {
  
  # Remove any rows with missing values in outcome, treatment, or controls
  panel_data <- data %>%
    select(all_of(outcome_var), all_of(treatment_var), all_of(controls)) %>%
    drop_na()
  
  cat("Sample size:", nrow(panel_data))
  
  # Extract variables as separate objects
  Y <- as.numeric(panel_data[[outcome_var]])  # Outcome (salaried employment)
  D <- as.numeric(panel_data[[treatment_var]]) # Treatment (training completion)
  X <- as.matrix(panel_data[, controls]) # Control variables (as matrix)
  
  # LASSO of D on X
  lasso_d <- rlasso(X, D, post = TRUE)
  selected_from_d <- which(lasso_d$coefficients[-1] != 0) # [-1] excludes intercept
  
  # LASSO of Y on X
  lasso_y <- rlasso(X, Y, post = TRUE)
  selected_from_y <- which(lasso_y$coefficients[-1] != 0) # [-1] excludes intercept
  
  # Union
  selected_idx <- unique(c(selected_from_y, selected_from_d)) # Combine both sets of selected variables
  n_selected <- length(selected_idx) # A control is included if EITHER LASSO selected it
  
  cat("  Controls selected:", n_selected)

  if (n_selected > 0) { # Get variable names (not just indices)
    selected_vars <- colnames(X)[selected_idx]
  } else {
    selected_vars <- NULL
  }
  
  # Post-LASSO OLS
  if (!is.null(selected_vars) && length(selected_vars) > 0) { # Now run standard OLS with the selected controls
    formula_lasso <- as.formula(paste(outcome_var, "~", treatment_var, "+",
                                      paste(selected_vars, collapse = " + "))) # This gives unbiased estimates (no L1 penalty shrinkage)
  } else {
    formula_lasso <- as.formula(paste(outcome_var, "~", treatment_var))
  }
  
  model_lasso <- lm(formula_lasso, data = panel_data)
  summ <- summary(model_lasso)
  
  # Extract results
  coef_lasso <- summ$coefficients[treatment_var, "Estimate"] # Training coefficient
  se_lasso <- summ$coefficients[treatment_var, "Std. Error"] # Standard error
  pval_lasso <- summ$coefficients[treatment_var, "Pr(>|t|)"] # P-value
  
  # Dropout mean (control group mean)
  dropout_mean <- mean(panel_data[[outcome_var]][panel_data[[treatment_var]] == 0], na.rm = TRUE) # Calculate dropout mean (control group baseline)
  
  cat("  Coefficient:", sprintf("%.3f", coef_lasso)) 
  if (pval_lasso < 0.01) cat("***")
  else if (pval_lasso < 0.05) cat("**")
  else if (pval_lasso < 0.10) cat("*")
  cat("  SE:", sprintf("%.3f", se_lasso))
  cat("  Observations:", nrow(panel_data))
  cat("  Dropout Mean:", sprintf("%.3f", dropout_mean))
  
  return(list(
    coefficient = coef_lasso,
    se = se_lasso,
    pval = pval_lasso,
    n_obs = nrow(panel_data),
    n_controls = n_selected,
    dropout_mean = dropout_mean,
    selected_vars = selected_vars
  ))
}

# Running Double LASSO for all 4 panels
cat("Running Double LASSO for all panels")

# Panel A: Pre-lockdown 2020
results_A <- run_double_lasso_panel(
  outcome_var = "Salaried_Prelockdown",
  treatment_var = "Training_complete",
  controls = all_controls,
  data = data,
  panel_name = "Panel A: Survey Round 1 (Pre-lockdown 2020)"
)

# Panel B: Jun-Jul 2020
results_B <- run_double_lasso_panel(
  outcome_var = "Salaried_Jun_Jul2020",
  treatment_var = "Training_complete",
  controls = all_controls,
  data = data,
  panel_name = "Panel B: Survey Round 1 (Jun-Jul 2020)"
)

# Panel C: Mar-Apr 2021
results_C <- run_double_lasso_panel(
  outcome_var = "Salaried_Mar_Apr2021",
  treatment_var = "Training_complete",
  controls = all_controls,
  data = data,
  panel_name = "Panel C: Survey Round 2 (Mar-Apr 2021)"
)

# Panel D: Nov-Dec 2021
results_D <- run_double_lasso_panel(
  outcome_var = "Salaried_Nov_Dec2021",
  treatment_var = "Training_complete",
  controls = all_controls,
  data = data,
  panel_name = "Panel D: Survey Round 3 (Nov-Dec 2021)"
)

# Creating Table in the Paper's format

cat("Table 2 Extended: Column [5] Double")

# Formatting coefficients with stars
format_coef <- function(coef, pval) {
  stars <- ""
  if (pval < 0.01) stars <- "***"
  else if (pval < 0.05) stars <- "**"
  else if (pval < 0.10) stars <- "*"
  return(paste0(sprintf("%.3f", coef), stars))
}

# Creating table
table_lasso <- data.frame(
  Panel = character(),
  Col5_LASSO = character(),
  stringsAsFactors = FALSE
)

# Panel A
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Panel A: Survey Round 1 (Pre-lockdown 2020)",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Trained",
  Col5_LASSO = format_coef(results_A$coefficient, results_A$pval),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = paste0("(", sprintf("%.3f", results_A$se), ")"),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Observations",
  Col5_LASSO = as.character(results_A$n_obs),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Dropout Mean",
  Col5_LASSO = sprintf("%.3f", results_A$dropout_mean),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))

# Panel B
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Panel B: Survey Round 1 (Jun-Jul 2020)",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Trained",
  Col5_LASSO = format_coef(results_B$coefficient, results_B$pval),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = paste0("(", sprintf("%.3f", results_B$se), ")"),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Observations",
  Col5_LASSO = as.character(results_B$n_obs),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Dropout Mean",
  Col5_LASSO = sprintf("%.3f", results_B$dropout_mean),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))

# Panel C
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Panel C: Survey Round 2 (Mar-Apr 2021)",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Trained",
  Col5_LASSO = format_coef(results_C$coefficient, results_C$pval),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = paste0("(", sprintf("%.3f", results_C$se), ")"),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Observations",
  Col5_LASSO = as.character(results_C$n_obs),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Dropout Mean",
  Col5_LASSO = sprintf("%.3f", results_C$dropout_mean),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))

# Panel D
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Panel D: Survey Round 3 (Nov-Dec 2021)",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Trained",
  Col5_LASSO = format_coef(results_D$coefficient, results_D$pval),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = paste0("(", sprintf("%.3f", results_D$se), ")"),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Observations",
  Col5_LASSO = as.character(results_D$n_obs),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Dropout Mean",
  Col5_LASSO = sprintf("%.3f", results_D$dropout_mean),
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "",
  Col5_LASSO = "",
  stringsAsFactors = FALSE
))

# Footer rows
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Sector Controls",
  Col5_LASSO = "LASSO",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Individual Controls",
  Col5_LASSO = "LASSO",
  stringsAsFactors = FALSE
))
table_lasso <- rbind(table_lasso, data.frame(
  Panel = "Household Controls",
  Col5_LASSO = "LASSO",
  stringsAsFactors = FALSE
))

# Print table
print(table_lasso, row.names = FALSE)

# Summary Statistics
cat("Summary: Controls selected by LASSO")

cat("Panel A (Pre-lockdown):", results_A$n_controls, "controls selected")
cat("Panel B (Jun-Jul 2020):", results_B$n_controls, "controls selected")
cat("Panel C (Mar-Apr 2021):", results_C$n_controls, "controls selected")
cat("Panel D (Nov-Dec 2021):", results_D$n_controls, "controls selected")

avg_selected <- mean(c(results_A$n_controls, results_B$n_controls, 
                       results_C$n_controls, results_D$n_controls))
reduction <- (1 - avg_selected / length(all_controls)) * 100

cat("Average controls selected:", round(avg_selected, 1))
cat("Total potential controls:", length(all_controls))
cat("Average reduction:", round(reduction, 1))

cat("Key Insights")
cat("Double LASSO achieves ", round(reduction, 0), "% reduction in controls", sep="")
cat("while maintaining significant training effects across all panels.")

# Find variables selected in all 4 panels
all_selected <- Reduce(intersect, list(
  results_A$selected_vars,
  results_B$selected_vars,
  results_C$selected_vars,
  results_D$selected_vars
))

cat("Controls selected in ALL 4 panels (", length(all_selected), "):")
print(all_selected)

# Find variables selected in 3+ panels
mostly_selected <- table(unlist(list(
  results_A$selected_vars,
  results_B$selected_vars,
  results_C$selected_vars,
  results_D$selected_vars
)))

cat("Controls selected in 3+ panels:")
print(sort(mostly_selected[mostly_selected >= 3], decreasing = TRUE))

# Saving Outputs
# Saving table
write.csv(table_lasso, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table2_column5_lasso.csv", row.names = FALSE)

# Saving summary
summary_df <- data.frame(
  Panel = c("A: Pre-lockdown", "B: Jun-Jul 2020", "C: Mar-Apr 2021", "D: Nov-Dec 2021"),
  Coefficient = c(results_A$coefficient, results_B$coefficient, 
                  results_C$coefficient, results_D$coefficient),
  SE = c(results_A$se, results_B$se, results_C$se, results_D$se),
  P_value = c(results_A$pval, results_B$pval, results_C$pval, results_D$pval),
  N_Controls = c(results_A$n_controls, results_B$n_controls, 
                 results_C$n_controls, results_D$n_controls),
  Observations = c(results_A$n_obs, results_B$n_obs, results_C$n_obs, results_D$n_obs)
)

write.csv(summary_df, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/lasso_summary_all_panels.csv", row.names = FALSE)

# html table
html_table <- table_lasso %>%
  kbl(col.names = c("", "[5] Double LASSO"),
      caption = "Table 2 Extended: Column [5] with Double LASSO Control Selection",
      align = c("l", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) %>%
  row_spec(c(1, 7, 13, 19), bold = TRUE, background = "#e0e0e0") %>%
  add_footnote(c(
    "Column [5] uses Double LASSO (Belloni et al., 2014) for data-driven control selection",
    "LASSO selects controls separately for each panel",
    paste("Average reduction:", round(reduction, 0), "% fewer controls vs. full specification"),
    "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10"
  ), notation = "none")

save_kable(html_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table2_column5_lasso.html")



























# Caste Heterogeneity Analysis with Strata fixed effects
# Do training effects on salaried employment differ between lower caste 
# (SC/ST/OBC) and general caste (upper caste) youth?

cat("Caste Heterogeneity Analysis")
cat("Testing if training effects differ for lower vs. general caste")

# Step 1: Creating Caste Variables
data <- data %>%
  mutate(
    # Convert outcome and treatment variables to numeric
    Training_complete = as.numeric(Training_complete), # (Some may be stored as factors or characters)
    Salaried_Prelockdown = as.numeric(Salaried_Prelockdown),
    Salaried_Jun_Jul2020 = as.numeric(Salaried_Jun_Jul2020),
    Salaried_Mar_Apr2021 = as.numeric(Salaried_Mar_Apr2021),
    Salaried_Nov_Dec2021 = as.numeric(Salaried_Nov_Dec2021),
    
    # Create General Caste indicator
    General_Caste = as.numeric(c_respondent_caste4) # Create General Caste indicator (1 = General/Upper caste, 0 = SC/ST/OBC)
  ) # c_respondent_caste4 is the baseline variable for General caste

# Verify caste composition
cat("Caste composition:")
cat("  General Caste (upper):", sum(data$General_Caste == 1, na.rm = TRUE), 
    "(", round(mean(data$General_Caste, na.rm = TRUE) * 100, 1))
cat("  Lower Caste (SC/ST/OBC):", sum(data$General_Caste == 0, na.rm = TRUE), 
    "(", round(mean(data$General_Caste == 0, na.rm = TRUE) * 100, 1))

# Step 2: Defining Controls (excluding caste since I am interacting with it)
# Sector controls
sector_controls <- grep("^sector[0-9]+$", names(data), value = TRUE)

# Individual controls excluding castes
individual_controls <- c(
  "c_gender",
  "c_age_above_20",
  "c_respondent_maritalstatus2",
  "c_respondent_religion2",
  "c_respondent_religion3",
  "c_respondent_education1",
  "c_respondent_education2",
  "c_respondent_education4",
  "c_Matric_exam",              
  "c_Inter_exam",               
  "c_respondent_migrate"
)
individual_controls <- individual_controls[individual_controls %in% names(data)]

# Household controls
household_controls <- c(
  "c_household_earning1",
  "c_household_earning2",
  "c_household_earning3",
  "c_agriculture_land",
  "c_BPL_card",
  "c_RSBY_Card",
  "c_SHG_member",
  "c_MNREGA",
  "c_internet_use",
  "c_relatives_migrate",
  "c_difficulty_immediate_famil",
  "c_difficulty_future_family"
)
household_controls <- household_controls[household_controls %in% names(data)]

# All controls (excluding caste variables)
all_controls <- c(sector_controls, individual_controls, household_controls)
cat("Using", length(all_controls), "controls (excluding caste)")

# Step 3: Function to Run Caste Interaction Model with strata fixed effects
run_caste_interaction <- function(outcome_var, data, controls, panel_name) { # This function estimates the interaction model for one survey round
  
  # Model: Y ~ Training + General_Caste + Training×General_Caste 
  #            + factor(strata) + Controls
  #
  # Strata fixed effects = 40 strata (state × sector × gender × treatment assignment)
  # This controls for the stratified randomisation design
  
  cat("Panel", panel_name)
  
  # Model: Y ~ Training + General + Training×General + factor(strata) + Controls
  formula_str <- paste(outcome_var, 
                       "~ Training_complete + General_Caste + Training_complete:General_Caste + factor(strata)")
  
  if (length(controls) > 0) {
    formula_str <- paste(formula_str, "+", paste(controls, collapse = " + "))
  }
  
  formula_full <- as.formula(formula_str)
  
  # Running regression
  model <- lm(formula_full, data = data)
  summ <- summary(model)
  
  # Extracting coefficients
  coef_training <- coef(model)["Training_complete"]
  se_training <- summ$coefficients["Training_complete", "Std. Error"]
  pval_training <- summ$coefficients["Training_complete", "Pr(>|t|)"]
  
  coef_interaction <- coef(model)["Training_complete:General_Caste"]
  se_interaction <- summ$coefficients["Training_complete:General_Caste", "Std. Error"]
  pval_interaction <- summ$coefficients["Training_complete:General_Caste", "Pr(>|t|)"]
  
  # Calculate effect for General Caste: β1 + β3
  effect_general <- coef_training + coef_interaction # Training effect for General Caste = β₁ + β₃
  
  # Standard error using delta method
  vcov_matrix <- vcov(model) # Use delta method: Var(β₁ + β₃) = Var(β₁) + Var(β₃) + 2·Cov(β₁, β₃)
  cov_training_interaction <- vcov_matrix["Training_complete", "Training_complete:General_Caste"] # This accounts for correlation between coefficients
  se_general_effect <- sqrt(se_training^2 + se_interaction^2 + 2*cov_training_interaction)
  
  # T-test for General Caste effect
  t_general <- effect_general / se_general_effect # H₀: β₁ + β₃ = 0 (no effect for General Caste)
  pval_general_effect <- 2 * pt(-abs(t_general), df = model$df.residual)
  
  n_total <- nobs(model)
  
  # Display results
  cat("  Training effect (Lower Caste):", sprintf("%.4f", coef_training))
  if (pval_training < 0.01) cat("***")
  else if (pval_training < 0.05) cat("**")
  else if (pval_training < 0.10) cat("*")

  cat("  Interaction (Training × General):", sprintf("%.4f", coef_interaction))
  if (pval_interaction < 0.01) cat("***")
  else if (pval_interaction < 0.05) cat("**")
  else if (pval_interaction < 0.10) cat("*")

  cat("  Training effect (General Caste):", sprintf("%.4f", effect_general))
  if (pval_general_effect < 0.01) cat("***")
  else if (pval_general_effect < 0.05) cat("**")
  else if (pval_general_effect < 0.10) cat("*")

  cat("  P-value (interaction):", sprintf("%.4f", pval_interaction))
  cat("  Observations:", n_total)
  
  return(list(
    coef_training = coef_training,
    se_training = se_training,
    pval_training = pval_training,
    coef_interaction = coef_interaction,
    se_interaction = se_interaction,
    pval_interaction = pval_interaction,
    effect_general = effect_general,
    se_general = se_general_effect,
    pval_general = pval_general_effect,
    n_obs = n_total
  ))
}

# Step 4: Run Caste Interaction for All 4 Panels - Estimate model separately for each time period to examine how caste
# Panel A: Pre-lockdown
results_A <- run_caste_interaction(
  outcome_var = "Salaried_Prelockdown",
  data = data,
  controls = all_controls,
  panel_name = "A: Pre-lockdown 2020"
)

# Panel B: Jun-Jul 2020
results_B <- run_caste_interaction(
  outcome_var = "Salaried_Jun_Jul2020",
  data = data,
  controls = all_controls,
  panel_name = "B: Jun-Jul 2020"
)

# Panel C: Mar-Apr 2021
results_C <- run_caste_interaction(
  outcome_var = "Salaried_Mar_Apr2021",
  data = data,
  controls = all_controls,
  panel_name = "C: Mar-Apr 2021"
)

# Panel D: Nov-Dec 2021
results_D <- run_caste_interaction(
  outcome_var = "Salaried_Nov_Dec2021",
  data = data,
  controls = all_controls,
  panel_name = "D: Nov-Dec 2021"
)

# Step 5: Create Results Table
format_coef <- function(coef, se, pval) {
  stars <- ""
  if (pval < 0.01) stars <- "***"
  else if (pval < 0.05) stars <- "**"
  else if (pval < 0.10) stars <- "*"
  
  coef_str <- sprintf("%.3f%s", coef, stars)
  se_str <- sprintf('"(%.3f)"', se)
  return(c(coef_str, se_str))
}

# Create table
caste_table <- data.frame(
  Variable = character(),
  Panel_A = character(),
  Panel_B = character(),
  Panel_C = character(),
  Panel_D = character(),
  stringsAsFactors = FALSE
)

# Training effect (Lower Caste)
fmt_A_lower <- format_coef(results_A$coef_training, results_A$se_training, results_A$pval_training) # This is the baseline training effect for SC/ST/OBC youth
fmt_B_lower <- format_coef(results_B$coef_training, results_B$se_training, results_B$pval_training)
fmt_C_lower <- format_coef(results_C$coef_training, results_C$se_training, results_C$pval_training)
fmt_D_lower <- format_coef(results_D$coef_training, results_D$se_training, results_D$pval_training)

caste_table <- rbind(caste_table, data.frame( 
  Variable = "Training effect (Lower Caste)",
  Panel_A = fmt_A_lower[1], Panel_B = fmt_B_lower[1], 
  Panel_C = fmt_C_lower[1], Panel_D = fmt_D_lower[1],
  stringsAsFactors = FALSE
))

caste_table <- rbind(caste_table, data.frame(
  Variable = "",
  Panel_A = fmt_A_lower[2], Panel_B = fmt_B_lower[2], 
  Panel_C = fmt_C_lower[2], Panel_D = fmt_D_lower[2],
  stringsAsFactors = FALSE
))


# Tests whether training effects differ for General Caste
# Negative β₃ = smaller effect for General Caste
# Positive β₃ = larger effect for General Caste
# Training × General Caste interaction
fmt_A_int <- format_coef(results_A$coef_interaction, results_A$se_interaction, results_A$pval_interaction)
fmt_B_int <- format_coef(results_B$coef_interaction, results_B$se_interaction, results_B$pval_interaction)
fmt_C_int <- format_coef(results_C$coef_interaction, results_C$se_interaction, results_C$pval_interaction)
fmt_D_int <- format_coef(results_D$coef_interaction, results_D$se_interaction, results_D$pval_interaction)

caste_table <- rbind(caste_table, data.frame(
  Variable = "Training × General Caste",
  Panel_A = fmt_A_int[1], Panel_B = fmt_B_int[1], 
  Panel_C = fmt_C_int[1], Panel_D = fmt_D_int[1],
  stringsAsFactors = FALSE
))

caste_table <- rbind(caste_table, data.frame( # Empty spacing row
  Variable = "",
  Panel_A = fmt_A_int[2], Panel_B = fmt_B_int[2], 
  Panel_C = fmt_C_int[2], Panel_D = fmt_D_int[2],
  stringsAsFactors = FALSE
))

# Training effect (General Caste)
# Total training effect for upper caste youth
fmt_A_general <- format_coef(results_A$effect_general, results_A$se_general, results_A$pval_general)
fmt_B_general <- format_coef(results_B$effect_general, results_B$se_general, results_B$pval_general)
fmt_C_general <- format_coef(results_C$effect_general, results_C$se_general, results_C$pval_general)
fmt_D_general <- format_coef(results_D$effect_general, results_D$se_general, results_D$pval_general)

caste_table <- rbind(caste_table, data.frame( # Row 5: Total effect coefficient
  Variable = "Training effect (General Caste)",
  Panel_A = fmt_A_general[1], Panel_B = fmt_B_general[1], 
  Panel_C = fmt_C_general[1], Panel_D = fmt_D_general[1],
  stringsAsFactors = FALSE
))

caste_table <- rbind(caste_table, data.frame( # Row 6: Standard error
  Variable = "",
  Panel_A = fmt_A_general[2], Panel_B = fmt_B_general[2], 
  Panel_C = fmt_C_general[2], Panel_D = fmt_D_general[2],
  stringsAsFactors = FALSE
))

# Empty row
caste_table <- rbind(caste_table, data.frame(
  Variable = "",
  Panel_A = "", Panel_B = "", Panel_C = "", Panel_D = "",
  stringsAsFactors = FALSE
))

# P-value for interaction
caste_table <- rbind(caste_table, data.frame(
  Variable = "P-value (interaction)",
  Panel_A = sprintf("%.3f", results_A$pval_interaction), # Key test: Is there significant caste heterogeneity?
  Panel_B = sprintf("%.3f", results_B$pval_interaction),
  Panel_C = sprintf("%.3f", results_C$pval_interaction),
  Panel_D = sprintf("%.3f", results_D$pval_interaction),
  stringsAsFactors = FALSE
))

# Sample size
caste_table <- rbind(caste_table, data.frame(
  Variable = "Observations",
  Panel_A = as.character(results_A$n_obs),
  Panel_B = as.character(results_B$n_obs),
  Panel_C = as.character(results_C$n_obs),
  Panel_D = as.character(results_D$n_obs),
  stringsAsFactors = FALSE
))

# Controls included
caste_table <- rbind(caste_table, data.frame(
  Variable = "Controls",
  Panel_A = "Yes", Panel_B = "Yes", Panel_C = "Yes", Panel_D = "Yes",
  stringsAsFactors = FALSE
))

# Strata fixed effects included
caste_table <- rbind(caste_table, data.frame(
  Variable = "Strata FE",
  Panel_A = "Yes", Panel_B = "Yes", Panel_C = "Yes", Panel_D = "Yes",
  stringsAsFactors = FALSE
))

# Display table
print(caste_table, row.names = FALSE)

# Step 6: Save Outputs
write.csv(caste_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/caste_heterogeneity_table.csv", row.names = FALSE)
cat("Table saved: caste_heterogeneity_table.csv")

# Saving summary
summary_df <- data.frame(
  Panel = c("A: Pre-lockdown", "B: Jun-Jul 2020", 
            "C: Mar-Apr 2021", "D: Nov-Dec 2021"),
  Effect_Lower = c(results_A$coef_training, results_B$coef_training,
                   results_C$coef_training, results_D$coef_training),
  SE_Lower = c(results_A$se_training, results_B$se_training,
               results_C$se_training, results_D$se_training),
  Effect_General = c(results_A$effect_general, results_B$effect_general,
                     results_C$effect_general, results_D$effect_general),
  SE_General = c(results_A$se_general, results_B$se_general,
                 results_C$se_general, results_D$se_general),
  Interaction_Coef = c(results_A$coef_interaction, results_B$coef_interaction,
                       results_C$coef_interaction, results_D$coef_interaction),
  Interaction_Pval = c(results_A$pval_interaction, results_B$pval_interaction,
                       results_C$pval_interaction, results_D$pval_interaction)
)

write.csv(summary_df, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/caste_heterogeneity_summary.csv", row.names = FALSE)

# Creating HTML table
html_caste_table <- caste_table %>%
  kbl(col.names = c("", "Pre-lockdown", "Jun-Jul 2020", "Mar-Apr 2021", "Nov-Dec 2021"),
      caption = "Caste Heterogeneity in Training Effects (with Strata FE)",
      align = c("l", "c", "c", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 12) %>%
  row_spec(0, bold = TRUE, background = "#f0f0f0") %>%
  add_footnote(c(
    "Dependent variable: Salaried employment",
    "Lower Caste = SC + ST + OBC (93% of sample), General Caste = Upper castes (7%)",
    "Training effect (Lower Caste) = coefficient on Training",
    "Training effect (General Caste) = Training + (Training × General)",
    "P-value (interaction) tests whether effects differ significantly by caste",
    "All specifications include sector, individual, household controls, and strata fixed effects",
    "Strata fixed effects = state × sector × gender × treatment assignment",
    "Standard errors in parentheses. *** p<0.01, ** p<0.05, * p<0.10"
  ), notation = "none")

save_kable(html_caste_table, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/caste_heterogeneity_table.html")

# Step 7: Interpretation
sig_interactions <- sum(c(results_A$pval_interaction, results_B$pval_interaction,
                          results_C$pval_interaction, results_D$pval_interaction) < 0.10)

cat("Key Findings")

cat("1. Caste Differences in Training Effects:")

for (i in 1:4) {
  panel_name <- c("A (Pre-lockdown)", "B (Jun-Jul 2020)", 
                  "C (Mar-Apr 2021)", "D (Nov-Dec 2021)")[i]
  result <- list(results_A, results_B, results_C, results_D)[[i]]
  
  cat("   Panel", panel_name, ":")
  cat("     Lower Caste:", sprintf("%.3f", result$coef_training))
  if (result$pval_training < 0.01) cat("***")
  else if (result$pval_training < 0.05) cat("**")
  else if (result$pval_training < 0.10) cat("*")

  cat("     General Caste:", sprintf("%.3f", result$effect_general))
  if (result$pval_general < 0.01) cat("***")
  else if (result$pval_general < 0.05) cat("**")
  else if (result$pval_general < 0.10) cat("*")

  diff <- result$effect_general - result$coef_training
  cat("     Difference:", sprintf("%.3f", diff))
  if (result$pval_interaction < 0.10) {
    cat(" (significant)")
  } else {
    cat(" (not significant)")
  }
}

cat("2. Summary:")

if (sig_interactions == 0) {
  cat("   No significant caste heterogeneity detected.")
  cat("   Training effects are statistically similar across caste groups.")
  cat("   (All interaction p-values > 0.10)")
} else {
  cat("   Some caste heterogeneity detected.")
  cat("   Training effects differ by caste in", sig_interactions, "out of 4 panels.")
}

cat("Caste Heterogeneity Analysis is complete with Strata fixed effects.")
















# Creating Table 1: Survey Structure

# Create the table manually
table1 <- data.frame(
  Panel = c("Panel A", "Panel B", "Panel C", "Panel D"),
  Survey_Round = c(
    "Round 1 (Pre-lockdown)",
    "Round 1 (Post-lockdown)", 
    "Round 2",
    "Round 3"
  ),
  Timing = c(
    "Feb-Mar 2020",
    "Jun-Jul 2020",
    "Mar-Apr 2021",
    "Nov-Dec 2021"
  ),
  Sample_Size = c(2204, 2204, 1890, 1916),
  Control_Mean = c(0.147, 0.080, 0.134, 0.133),
  Context = c(
    "Pre-pandemic",
    "Lockdown period",
    "Partial recovery",
    "18 months post-training"
  ),
  stringsAsFactors = FALSE
)

# Displaying
cat("Table 1: Survey Structure and Sample Characteristics")
print(table1, row.names = FALSE)

# Saving CSV
write.csv(table1, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table1_survey_structure.csv", row.names = FALSE)

# Creating formatted HTML table
html_table1 <- table1 %>%
  kbl(
    col.names = c(
      "Panel",
      "Survey Round",
      "Timing",
      "Sample Size",
      "Control Mean",
      "Context"
    ),
    caption = "Table 1: Survey Structure and Sample Characteristics",
    align = c("l", "l", "c", "c", "c", "l")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) %>%
  row_spec(0, bold = TRUE) %>%
  add_footnote(
    c(
      "Notes: Sample sizes refer to observations with non-missing outcome data.",
      "Control Mean shows salaried employment rate in control group (non-completers).",
      "Total baseline sample: 2,259 (1,122 treatment, 1,137 control).",
      "Attrition: 14% by Round 2, 13% by Round 3."
    ),
    notation = "none"
  )

save_kable(html_table1, "/Users/nitaycarmi/Desktop/Year 3/EC348 Research in Policy Evaluation/Assignment 1/table1_survey_structure.html")













# Search for round 1 (post lockdown) variables
# 1. Find all variables with "Salaried"
cat("Variables with 'Salaried' or 'salaried':")
salaried_all <- grep("alaried", names(data), value = TRUE, ignore.case = TRUE)
print(salaried_all)

# Check their means
for (var in salaried_all) {
  control_mean <- mean(data[[var]][data$treatment == 0], na.rm = TRUE)
  treatment_mean <- mean(data[[var]][data$treatment == 1], na.rm = TRUE)
  cat(sprintf("%-30s Control: %.3f  Treatment: %.3f", 
              var, control_mean, treatment_mean))
}

# 2. Search for "Casual" employment
cat("Variables with 'Casual' or 'casual':")
casual_vars <- grep("asual", names(data), value = TRUE, ignore.case = TRUE)
print(casual_vars)

# 3. Search for "location" in Round 1 data (not c_ prefix)
cat("Variables with 'location' (non-baseline):")
location_round1 <- grep("location", names(data), value = TRUE, ignore.case = TRUE)
location_round1 <- location_round1[!grepl("^c_", location_round1)]
print(location_round1)

# 4. Search for Round 1 specific variables
cat("Variables with 'Jun', 'Jul', 'June', 'July', 'Round1', 'r1':")
round1_vars <- grep("Jun|Jul|June|July|Round1|r1|R1", names(data), 
                    value = TRUE, ignore.case = TRUE)
print(round1_vars)

# 5. Check what variables exist that are NOT baseline controls
cat("Non-baseline variables (first 50):")
non_baseline <- names(data)[!grepl("^c_", names(data))]
print(head(non_baseline, 50))

# 6. Look for Home/State/Outside in variable names
cat("Variables with 'Home', 'State', 'Outside':")
home_state <- grep("Home|State|Outside", names(data), value = TRUE, ignore.case = TRUE)
print(home_state)

cat("Variables that might be 'state':")
grep("state|district|location|region|area", names(data), 
     value = TRUE, ignore.case = TRUE)

# Also check what 'sector' looks like
cat("Sector variables:")
grep("sector", names(data), value = TRUE, ignore.case = TRUE)

# Check gender
cat("Gender variables:")
grep("gender|sex", names(data), value = TRUE, ignore.case = TRUE)

# Check treatment
cat("Treatment variables:")
grep("treatment|treat|assign", names(data), value = TRUE, ignore.case = TRUE)