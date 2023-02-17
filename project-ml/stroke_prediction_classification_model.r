##Stroke Prediction by R

library(dplyr)
library(caret)
library(tidyverse)
library(mlbench)
library(ggplot2)
library(tidyr)
library(viridis)
library(superml)
library(ROSE)

##load data
##Stroke data set from kaggle
data_stroke <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQQ-f2Cvykee-B0awziuZSLRwShwSe2zW5Exv-cPwa83odAaByskayp1zIvYTbkYACbnASiU5I3UDs4/pub?gid=252135330&single=true&output=csv")
View(data_stroke)
glimpse(data_stroke)

##remove row that show "N/A"
mean(complete.cases(data_stroke))

data_stroke <- data_stroke %>% 
  filter(bmi != "N/A") 

##convert stroke to factor
data_stroke$stroke <- factor(data_stroke$stroke,
                      level = c(0,1),
                      labels = c("0", "1"))

data_stroke$age <- as.numeric(data_stroke$age)
data_stroke$avg_glucose_level <- as.numeric(data_stroke$avg_glucose_level)
data_stroke$bmi <- as.numeric(data_stroke$bmi)


##Exploring numerical data
data_stroke %>% 
  group_by(stroke) %>% 
  summarise(count = n()) %>% 
  mutate(percent = ((count / sum(count))*100))

data_stroke %>% 
  ggplot( aes(stroke)) +
  geom_bar(width = 0.5, fill = "darkorange") +
  theme_minimal()

##The class "0" or is the majority class, and the smaller in size "Yes" class is the minority class.
##The classes are imbalanced data.

##Average glucose level and Stroke
data_stroke %>% 
  ggplot( aes(avg_glucose_level , fill=stroke))+
  geom_histogram(alpha=0.6, color="#e9ecef", position = 'identity') +
  scale_fill_manual(values=c("#66b2b2", "#004c4c"))+
  theme_minimal() +
  labs(fill="")

##BMI and Stroke
data_stroke %>% 
  ggplot( aes(bmi , fill=stroke))+
  geom_histogram(alpha=0.6, color="#e9ecef", position = 'identity') +
  scale_fill_manual(values=c("#66b2b2", "#004c4c"))+
  theme_minimal() +
  labs(fill="")

##Age and Stroke
data_stroke %>% 
  ggplot( aes(age , fill=stroke))+
  geom_histogram(alpha=0.6, color="#e9ecef", position = 'identity') +
  scale_fill_manual(values=c("#66b2b2", "#004c4c"))+
  theme_minimal() +
  labs(fill="")

## drop column 'id'
data_stroke <- data_stroke[, -1]

##feature scaling 
##Normalization the data 
##the mean is 0 and standard deviation is 1
data.pre <-preProcess(data_stroke, method=c("center", "scale"))
data_stroke <- predict(data.pre, data_stroke)

## One-Hot Encoding
dummy <- dummyVars("~ gender + ever_married + work_type + Residence_type + smoking_status", 
                   data = data_stroke,
                   fullRank = T) 

data_dummy <- data.frame(predict(dummy, newdata = data_stroke))

data_stroke <- data_stroke %>% 
  select(-c(gender , ever_married, work_type, Residence_type, smoking_status)) %>% 
  mutate(data_dummy)


##split data
train_test_split <- function(data, train_size = 0.7){
  set.seed(22)
  n <- nrow(data)
  id <- sample(1:n, size=n*train_size)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  
  return(list(train_data, test_data))
}

split_data <- train_test_split(data_stroke, 0.7)
train_data <- split_data[[1]]
test_data <- split_data[[2]]

nrow(train_data); nrow(test_data)


##The classes are imbalanced data 
##so perform over-sampling and under-sampling only for train data

ovun_data <- ovun.sample(stroke ~., 
                         data = train_data, 
                         method = "over",
                         seed = 28)$data

##Classification Model 
##Model : Random Forest 
crtl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 3,
                     verboseIter = T,
                     search = "grid")

tunegrid <- expand.grid(.mtry = c(1:15))

##Train
set.seed(28)
rf_model <- train( stroke ~ .,
                    data = ovun_data,
                    method = "rf",
                    metric = "Accuracy",
                    tuneGrid = tunegrid,
                    trControl = crtl)

p1 <- predict(rf_model, newdata = test_data)
mean(p1 == test_data$stroke)

##variable importance
varImp(rf_model)

##Model : Random Forest
##adjust variables 
##Train model2
set.seed(28)
rf_model2 <- train( stroke ~ avg_glucose_level + age + bmi,
                   data = ovun_data,
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = tunegrid,
                   trControl = crtl)

p2 <- predict(rf_model2, newdata = test_data)
mean(p2 == test_data$stroke)

##Evaluate models
confusionMatrix(p2, 
                test_data$stroke,
                mode = "everything",
                positive = "1")


##Model : KNN
crtl2 <- trainControl(method = "repeatedcv",
                      number = 10,
                      repeats = 10,
                      verboseIter = T)

set.seed(22)
knn_model <- train( stroke ~ avg_glucose_level + age + bmi,
                    data = ovun_data,
                    method = "knn",
                    metric = "Accuracy",
                    trControl = crtl2,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

knn_p <- predict(knn_model, newdata = test_data)
mean(knn_p == test_data$stroke)


##Evaluate models
confusionMatrix(knn_p, 
                test_data$stroke,
                mode = "everything",
                positive = "1")


##Model : glm
glm_model <- train( stroke ~ avg_glucose_level + age + bmi,
                    data = ovun_data,
                    method = "glm",
                    metric = "Accuracy",
                    trControl = crtl2,
                    tuneLength = 10)

glm_p <- predict(glm_model, newdata = test_data)
mean(glm_p == test_data$stroke)

##Evaluate models
confusionMatrix(glm_p, 
                test_data$stroke,
                mode = "everything",
                positive = "1")


#Models : glm lasso, ridge
grid <- expand.grid(alpha = c(0,1),
                    lambda = seq(0,1, by = 0.03))

glmnet_model <- train( stroke ~ avg_glucose_level + age + bmi,
                       data = ovun_data,
                       method = "glmnet",
                       metric = "Accuracy",
                       trControl = crtl2,
                       tuneGrid = grid)

glmnet_p <- predict(glmnet_model, newdata = test_data)
mean(glmnet_p == test_data$stroke)

##Evaluate models
confusionMatrix(glmnet_p, 
                test_data$stroke,
                mode = "everything",
                positive = "1")

##Model : Decision tree 
tree_model <- train( stroke ~ avg_glucose_level + age + bmi,
                    data = ovun_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = crtl)

tree_p <- predict(tree_model, newdata = test_data)
mean(tree_p == test_data$stroke)

##Evaluate models
confusionMatrix(tree_p, 
                test_data$stroke,
                mode = "everything",
                positive = "1")


