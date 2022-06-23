library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(dtplyr)
library(tidyr)
library(MASS)
library(randomForest)
library(purrr)
library(caret)

#Read Data
train_df <- read.csv("train_rest.csv")
test_df <- read.csv("test_rest.csv")

#Data summary
head(train_df)
head(test_df)
summary(train_df)
str(train_df)

#Missing values 
missing_values <- train_df  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing_data") %>%
  count(variables, missing_data) 

ggplot(missing_values, aes(y = variables, x = n, fill = missing_data)) + 
  geom_col() + ggtitle("Train Data")

sprintf("We have left %1.0f missing values", sum(missing_values$missing_data))

#Missing values test data
missing_values_test <- test_df  %>%
  summarise_all(list(~is.na(.))) %>%
  pivot_longer(everything(),
               names_to = "variables", values_to="missing_data") %>%
  count(variables, missing_data) 

ggplot(missing_values_test, aes(y = variables, x = n, fill = missing_data)) + 
  geom_col() + ggtitle("Test Data")

sprintf("We have left %1.0f missing values", sum(missing_values_test$missing_data))

#Join data
test_id <- test_df$Id
test_df <- test_df %>%
  mutate(revenue = NA)
all_data <- rbind(train_df, test_df)

#Open Date variable
all_data$Open.Date <- as.Date(all_data$Open.Date, format = "%m/%d/%Y")

str(all_data$Open.Date)

all_data <- all_data %>%
  mutate(days_since_open = as.numeric(Sys.Date()-all_data$Open.Date))

str(all_data$days_since_open)


#Categorical variables
length(unique(all_data$City))
length(unique(all_data$City.Group))
length(unique(all_data$Type))

#City has too many different values

#Dummify categorical variables 
all_data$City.Group <- as.factor(all_data$City.Group)
all_data$Type <- as.factor(all_data$Type)
levels(all_data$Type) <- c("Drive Thru", "Food Court", "Inline", "Mobile")
str(all_data$City.Group)
str(all_data$Type)


str(all_data)

#Separate Data
train_df_final <- all_data[1:137,]
test_df_final <- all_data[138:length(all_data$Id),]

train_df_final <- train_df_final[-c(1,2,3)]
test_df_final <- test_df_final[-c(1,2,3,43)]

#Graphs and correlation

ggplot(train_df_final, aes(days_since_open, revenue)) + 
  geom_point() +
  ggtitle("Train Data") +
  labs(y = "Revenue", x = "Days since open")

ggplot(train_df_final, aes(Type, revenue)) + 
  geom_point() +
  ggtitle("Train Data") +
  labs(y = "Revenue", x = "Restaurant type")

ggplot(train_df_final, aes(City.Group, revenue)) + 
  geom_point() +
  ggtitle("Train Data") +
  labs(y = "Revenue", x = "City Type")


#Scaling Data
for(i in c(3:39,41)){
  train_df_final[i] <- scale(train_df_final[i],center=TRUE,scale =TRUE)
}
for(i in c(3:40)){
  test_df_final[i] <- scale(test_df_final[i],center=TRUE,scale =TRUE)
}

cor_matrix <- data.frame(cor(train_df_final[,3:41],train_df_final$revenue))
cor_matrix

#Regression model

model_1 <- glm(revenue ~., train_df_final, family = "gaussian")
summary(model_1)

model_2 <- lm(revenue ~., train_df_final)
summary(model_2)


#Random Forest model

randomforest_model <- randomForest(revenue ~., train_df_final)
summary(randomforest_model)
print(randomforest_model)

#Predictions
prediction_model_1 <- predict(randomforest_model,test_df_final)
head(prediction_model_1)

submission_df <- data.frame(test_id, prediction_model_1)

head(submission_df)

write.csv(submission_df,"Revenue Prediction", row.names = FALSE)
