library(tidyverse)
library(arrow)
library(caret)

fname <- file.choose()

full_data <- read_csv(fname)

trainIndex <- createDataPartition(full_data$energy_per_sqft, p = .8, list = FALSE)
train_set <- full_data[trainIndex, ]
test_set <- full_data[-trainIndex, ]

# Create a model for prediction using `train_set`, and evaluate your model's performance using `test_set`.
# You can try several models and pick the best one.

# Assume the overall temperature increases by 5 degrees in the next 10 years. We need to make predictions about energy usage.

new_data <- test_set
new_data$temp <- new_data$temp + 5
new_data$energy_per_sqft <- NULL

# Now, you have a new dataset created by modifying the temperature in the test data.
# Tip: Even though your target variable is `energy_per_sqft`, you ultimately want to know the whole energy assumption (by day? by hour?).
# Calculate the total energy consumption by multiplying `energy_per_sqft` with `sqft` to determine the peak energy demand.