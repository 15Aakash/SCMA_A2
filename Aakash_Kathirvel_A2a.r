# Set the working directory and verify it
setwd('C:/Users/Aakash/Desktop/SCMA')
getwd()

# Reading the file into R
data <- read.csv("C:/Users/Aakash/Desktop/SCMA/NSSO68.csv")
# Display the first few rows of the data
head(data)

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "ggplot2", "car")
lapply(libraries, install_and_load)

# Subset data for Meghalaya
subset_data <- data %>%
  filter(state_1 == 'MEG') %>% 
  select(foodtotal_q, MPCE_MRP, ricepds_q, riceos_q, Meals_At_Home, Possess_ration_card, milkprott_q, No_of_Meals_per_day)
print(subset_data)

sum(is.na(subset_data$MPCE_MRP))
sum(is.na(subset_data$ricepds_q))
sum(is.na(subset_data$Meals_At_Home))
sum(is.na(subset_data$Possess_ration_card))
sum(is.na(data$milkprott_q))

impute_with_mean <- function(data, columns) {
  data %>%
    mutate(across(all_of(columns), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
}

# Columns to impute
columns_to_impute <- c("milkprott_q")

# Impute missing values with mean
data <- impute_with_mean(data, columns_to_impute)

sum(is.na(data$milkprott_q))

# Fit the regression model
model <- lm(foodtotal_q~ MPCE_MRP+ricepds_q+riceos_q+Meals_At_Home+Possess_ration_card+milkprott_q, data = subset_data)

# Print the regression results
print(summary(model))


# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(model) # VIF Value more than 8 its problematic

# Extract the coefficients from the model
coefficients <- coef(model)

# Construct the equation
equation <- paste0("y = ", round(coefficients[1], 2))
for (i in 2:length(coefficients)) {
  equation <- paste0(equation, " + ", round(coefficients[i], 6), "*x", i-1)
}
# Print the equation
print(equation)




head(subset_data$MPCE_MRP,1)
head(subset_data$ricepds_q,1)
head(subset_data$riceos_q,1) 
head(subset_data$Meals_At_Home,1)
head(subset_data$Possess_ration_card,1) 
head(subset_data$milkprott_q,1)
head(subset_data$foodtotal_q,1)






