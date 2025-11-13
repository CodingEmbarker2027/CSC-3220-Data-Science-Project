library(tidyr)
library(dplyr)
library(mice)

# Read in the tables
train <- read.table(
  'introextro_train.csv',
  sep = ",",
  na.strings = c("", " ", "NA"),
  stringsAsFactors = T, header = T
)

test <- read.table(
  'introextro_test.csv',
  sep = ",",
  na.strings = c("", " ", "NA"),
  stringsAsFactors = T, header = T
)

# Checking it
summary(train)
summary(test)

# Changing Yes/No into 1/0
train <- train %>%
  mutate(Stage_fear = ifelse(Stage_fear == "Yes", 1, 0))
train <- train %>%
  mutate(Drained_after_socializing = ifelse(Drained_after_socializing == "Yes", 1, 0))
test <- test %>%
  mutate(Stage_fear = ifelse(Stage_fear == "Yes", 1, 0))
test <- test %>%
  mutate(Drained_after_socializing = ifelse(Drained_after_socializing == "Yes", 1, 0))

# Checking it again
summary(train)
summary(test)

# Mice imputationing it (test)

mice_imputation <- data.frame(
  original = train$Time_spent_Alone,
  imp_pmm = complete(mice(train, method = "pmm"))$Time_spent_Alone,
  imp_cart = complete(mice(train, method = "cart"))$Time_spent_Alone,
  imp_lasso = complete(mice(train, method = "lasso.norm"))$Time_spent_Alone
)

mice_imp_test <- data.frame(
  original = train,
  imp_cart = complete(mice(train, method = "cart"))
)

colSums(is.na(mice_imp_test))



# Logistic Regression stuff
y <- "Personality"
x <- c("Time_spentAlone",
       "Stage_fear",
       "Social-event-attendance",
       "Going_outside",
       "Drained_after_socializing",
       "Friends_circle_size",
       "Post_frequency")
fmla