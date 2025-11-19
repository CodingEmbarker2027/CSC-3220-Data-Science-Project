# Load Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(pROC)

# Load Data
train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

################EXPLORATORY DATA ANALYSIS###################
# checkign column names
names(train)
names(test)

str(train) # Basic Structure of dataeset

summary(train) # Summary (Haha can't go without it)

colSums(is.na(train)) # Check Missing Values

# Introvert vs Extrovert
table(train$Personality)

ggplot(train, aes(x = Personality)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Personality Types",
       x = "Personality (0 = Introvert, 1 = Extrovert)",
       y = "Count") +
  theme_minimal()

# Boxplots for numeric variables (detecting outliers)
# There are outliers in Time_Spent_Alone
numeric_vars <- c("Time_spent_Alone",
                  "Social_event_attendance",
                  "Going_outside",
                  "Friends_circle_size",
                  "Post_frequency")

for (var in numeric_vars) {
  print(
    ggplot(train, aes_string(y = var)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = paste("Boxplot of", var),
           y = var) +
      theme_minimal()
  )
}

###################CLEANING########################################
# Convert Yes/No Variables to 1/0
binary_variables <- c("Stage_fear", "Drained_after_socializing")

convert_binary <- function(df){
  df %>% mutate(across(all_of(binary_variables),
                       ~ ifelse(. == "Yes", 1, 
                                ifelse(. == "No", 0, NA))))
}

train <- convert_binary(train)
test <- convert_binary(test)

# Convert Personality to Binary. Just training set. 
# Set Extrovert = 1, Introvert = 0
train <- train %>%
  mutate(Personality = ifelse(Personality == "Extrovert", 1, 0))

# Defining numeric variables
numeric_variables <- c("Time_spent_Alone", 
                       "Social_event_attendance",
                       "Going_outside",
                       "Friends_circle_size",
                       "Post_frequency")

# Median Imputation for Numeric Variables
for (var in numeric_variables){
  med <- median(train[[var]], na.rm = TRUE) # computing median
  train[[var]][is.na(train[[var]])] <- med
  test[[var]][is.na(test[[var]])] <- med
}

# Mode imputation for Binary Variables

mode_value <- function(x){
  ux <- unique(x[!is.na(x)]) # Remove Missing Values and get the unique remaining values
  counts <- tabulate(match(x, ux)) # Count how many time each unique value appears
  mode_index <- which.max(counts) # Find which value has the maximum count
  return(ux[mode_index]) # Return either 0 or 1
}

for (var in binary_variables){
  m <- mode_value(train[[var]]) # compute mode from Train
  train[[var]][is.na(train[[var]])] <- m
  test[[var]][is.na(test[[var]])] <- m
}
##########################MODELING##############################
# Logistics Regression Model
model <- glm(
  Personality ~ Time_spent_Alone +
                Social_event_attendance +
                Going_outside +
                Friends_circle_size +
                Post_frequency +
                Stage_fear +
                Drained_after_socializing,
  data = train,
  family = binomial
)


# How I interpret the Logistic Model Summary
# Remember that Extrovert is set to 1 and Introvert is set to 0
# Look at the Estimate: Positive pushed towards Extrovert
# While Negative pushes toward Introvert
# The Std Error is small which means more confidence in estimate
# Drained after Socializing and Stage_fear are strongest indicators for introversion
# More time Spent Alone is moderate indicator for introversion

summary(model)

########### A COOL VISUALIZATION TO SEE THE LOGISTICS REGRESSION MODEL###################
############UNCOMMENT THE LINES BELOW TO SEE#########################
# # Convert model output into a tidy dataframe
 tidy_model <- tidy(model)
# 
# # Remove intercept for clarity
 tidy_model <- tidy_model[tidy_model$term != "(Intercept)", ]
# 
# # Coefficient Plot
ggplot(tidy_model, aes(x = estimate, y = term)) +
geom_point(size = 3, color = "blue") +
geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
labs(title = "Coefficient Plot of Logistic Regression",
x = "Estimate (Effect Size)",
        y = "Predictor") +
   theme_minimal()

train$pred_prob <- predict(model, type = "response")
train$pred_class <- ifelse(train$pred_prob >= 0.5, 1, 0)
table(Predicted = train$pred_class, Actual = train$Personality)

accuracy <- mean(train$pred_class == train$Personality)
accuracy

roc_obj <- roc(train$Personality, train$pred_prob)
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve for Logistic Regression")
auc(roc_obj)

