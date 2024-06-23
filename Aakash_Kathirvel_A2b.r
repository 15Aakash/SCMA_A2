# Set the working directory and verify it
setwd('C:/Users/Aakash/Desktop/SCMA')

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    if (!require(package, character.only = TRUE)) {
      stop(paste("Package", package, "could not be installed. Please check and try again."))
    }
  }
  library(package, character.only = TRUE)
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "stringdist")
lapply(libraries, install_and_load)

# Load the datasets
df_ipl <- read_csv("IPL_ball_by_ball_updated till 2024.csv")
salary <- read_excel("IPL SALARIES 2024.xlsx")

# Group and aggregate the performance metrics for runs scored
total_runs_each_year <- df_ipl %>%
  group_by(Season, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE)) %>%
  ungroup()

# Function to match names
match_names <- function(name, names_list) {
  match <- amatch(name, names_list, maxDist = 0.2)
  if (!is.na(match)) {
    return(names_list[match])
  } else {
    return(NA)
  }
}

# Matching names and merging with salary data
df_salary_runs <- salary
df_salary_runs$Matched_Player <- sapply(df_salary_runs$Player, function(x) match_names(x, total_runs_each_year$Striker))
df_merged_runs <- merge(df_salary_runs, total_runs_each_year, by.x = "Matched_Player", by.y = "Striker")

# Subset data for the last three years
df_merged_runs <- df_merged_runs %>% filter(Season %in% c("2021", "2022", "2023"))

# Perform regression analysis for runs scored
X_runs <- df_merged_runs %>% dplyr::select(runs_scored)
y_runs <- df_merged_runs$Rs

# Split the data into training and test sets (80% for training, 20% for testing)
set.seed(42)
trainIndex_runs <- sample(seq_len(nrow(X_runs)), size = 0.8 * nrow(X_runs))
X_train_runs <- X_runs[trainIndex_runs, , drop = FALSE]
X_test_runs <- X_runs[-trainIndex_runs, , drop = FALSE]
y_train_runs <- y_runs[trainIndex_runs]
y_test_runs <- y_runs[-trainIndex_runs]

# Create a linear regression model for runs scored
model_runs <- lm(y_train_runs ~ runs_scored, data = data.frame(runs_scored = X_train_runs$runs_scored, y_train_runs))
summary_runs <- summary(model_runs)
print(summary_runs)

# Evaluate the model for runs scored
y_pred_runs <- predict(model_runs, newdata = data.frame(runs_scored = X_test_runs$runs_scored))
r2_runs <- cor(y_test_runs, y_pred_runs)^2
print(paste("R-squared for runs scored: ", r2_runs))

# Group and aggregate the performance metrics for wickets taken
total_wicket_each_year <- df_ipl %>%
  group_by(Season, Bowler) %>%
  summarise(wickets_taken = sum(wicket_confirmation, na.rm = TRUE)) %>%
  ungroup()

# Matching names and merging with salary data
df_salary_wickets <- salary
df_salary_wickets$Matched_Player <- sapply(df_salary_wickets$Player, function(x) match_names(x, total_wicket_each_year$Bowler))
df_merged_wickets <- merge(df_salary_wickets, total_wicket_each_year, by.x = "Matched_Player", by.y = "Bowler")

# Subset data for the last three years
df_merged_wickets <- df_merged_wickets %>% filter(Season %in% c("2021", "2022", "2023"))

# Perform regression analysis for wickets taken
X_wickets <- df_merged_wickets %>% dplyr::select(wickets_taken)
y_wickets <- df_merged_wickets$Rs

# Split the data into training and test sets (80% for training, 20% for testing)
set.seed(42)
trainIndex_wickets <- sample(seq_len(nrow(X_wickets)), size = 0.8 * nrow(X_wickets))
X_train_wickets <- X_wickets[trainIndex_wickets, , drop = FALSE]
X_test_wickets <- X_wickets[-trainIndex_wickets, , drop = FALSE]
y_train_wickets <- y_wickets[trainIndex_wickets]
y_test_wickets <- y_wickets[-trainIndex_wickets]

# Create a linear regression model for wickets taken
model_wickets <- lm(y_train_wickets ~ wickets_taken, data = data.frame(wickets_taken = X_train_wickets$wickets_taken, y_train_wickets))
summary_wickets <- summary(model_wickets)
print(summary_wickets)

# Evaluate the model for wickets taken
y_pred_wickets <- predict(model_wickets, newdata = data.frame(wickets_taken = X_test_wickets$wickets_taken))
r2_wickets <- cor(y_test_wickets, y_pred_wickets)^2
print(paste("R-squared for wickets taken: ", r2_wickets))
