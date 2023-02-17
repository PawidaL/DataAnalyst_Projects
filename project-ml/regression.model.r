## Car Price Prediction by R

library(dplyr)
library(caret)
library(mlbench)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)
library(viridis)
library(RColorBrewer)
library(superml)
library(reshape2)
library(heatmaply)

## loading dataset and exploring
## Car Prices Dataset from Kaggle
carprice_data <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ0O0GYJqNy7EVd4kCvIus6nsAQHjZE-9_RRkBHJmGVyOkYRlnLjDjMjxfIkm0SR3WDPSrcB738EgVi/pub?output=csv")
View(carprice_data)
glimpse(carprice_data)

## check whether there is missing values or not
mean(complete.cases(carprice_data))

## drop column 'id'
carprice_data <- carprice_data[, -1]

## Data Visualization
## visualizing Price(dependent variable)
## Boxplot to detect Outlier
carprice_data %>% 
  ggplot(aes( price))+
  geom_boxplot(fill="darkslategray3")+
  xlab("Car Price")

## Histogram for Distribution
carprice_data %>% 
  ggplot(aes( price))+
  geom_density(fill="#52bf90",color="#52bf90",alpha=0.8)

print(summary(carprice_data$price))
## the price is skewed distribution, the most prices in the data set are below 15,000
## the 85% of the prices are below 18,500, whereas the remaining 15% are between 18,500 and 45,400


## visualizing numerical data
## the plot show the relationship between price and numerical independent variables
carprice_data %>% 
  keep(is.numeric) %>% 
  gather(-price, key = "key", value = "value") %>% 
  ggplot(aes(value, price)) +
  geom_point(color = "#005582")+
  geom_smooth(method=lm, color="#FF7F50")+
  facet_wrap(~key, scales = "free", ncol=3) +
  theme_minimal()

## Calculate the correlation coefficient 
## Compute the correlation matrix
col_num <- carprice_data %>% 
  keep(is.numeric)

cormat <- round(cor(col_num),2)

## correlation by heatmap
melted_cormat <- melt(cormat)
head(melted_cormat)

melted_cormat %>% 
  ggplot( aes(Var2, Var1, fill=value)) +
  geom_tile()+
  scale_fill_gradient2(low = "#e7eff6", high = "#4b86b4", mid = "#adcbe3", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  geom_text(aes(Var2, Var1, label = value),
            color = "black", size = 4)+
  theme_minimal()

## wheelbase, enginesize, boreratio and horesepower show the positive correlation with price.
## citympg and highwaympg show the negative correlation with price.
## Highly correlated variables to price over 0.8 is curbweight, enginesize and horsepower

## Visualizing categorical variable
glimpse(carprice_data)

## The impact of categorical variables on price
## Bar Chart
carprice_data %>% 
  select(c(fueltype,aspiration, doornumber, carbody, drivewheel, enginelocation,
           enginetype,cylindernumber, fuelsystem)) %>% 
  gather(key = "key_factor", value = "value") %>% 
  ggplot( aes(value)) +
  geom_bar(fill="#739d89", width = 0.3) +
  scale_color_viridis(discrete = TRUE)+
  facet_wrap(~key_factor, scale = "free", ncol = 2) + 
  theme_minimal()

## Boxplot
carprice_data %>% 
  select(c(fueltype,aspiration, doornumber, carbody, drivewheel, enginelocation,
           enginetype,cylindernumber, fuelsystem, price)) %>% 
  gather(-price, key = "key_factor", value = "value") %>% 
  ggplot( aes(value, price, fill=value )) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE)+
  facet_wrap(~key_factor, scale = "free", ncol = 2) + 
  theme_minimal()+
  theme(legend.position ="none")

## Observation based on the above plot
## The number of gas fueled car are more than diesel and the average price of diesel car is higher than that of gas fueled cars.
## The average price of cars with turbo aspiration is higher than standard aspiration.
## The car with four and two doors are almost equal in number and on average price so the price is not affected by this variable.
## The engine location is clearly impact on price. The rear cars are more expensive than the front engine.
## Price are highest for hardtop but sedan is the most popular.
## The highest average price of fuel system is mpfi and it is the most favored type.
## The car with eight cylinder is the most expensive, the common number of cylinders are four.
## FWD is the most common type of drive wheel followed by RWD.


## Separate 'CarName' column into 'CompanyName' and 'Model'
carprice_data <- extract(carprice_data, 
                         col = CarName, 
                         into = c("CompanyName", "Model"), 
                         regex = "^(\\w+)\\s?(.*)$")

unique(carprice_data$CompanyName)

## Replace misspelled company name
carprice_data$CompanyName[carprice_data$CompanyName == 'maxda'] <- 'mazda'
carprice_data$CompanyName[carprice_data$CompanyName == 'porcshce'] <- 'porsche'
carprice_data$CompanyName[carprice_data$CompanyName == 'toyouta'] <- 'toyota'
carprice_data$CompanyName[carprice_data$CompanyName == 'vokswagen'] <- 'volkswagen'
carprice_data$CompanyName[carprice_data$CompanyName == 'vw'] <- 'volkswagen'
carprice_data$CompanyName[carprice_data$CompanyName == 'Nissan'] <- 'nissan'

## The impact of company on price 
carprice_data %>% 
  select(c(price,CompanyName)) %>% 
  group_by(CompanyName) %>% 
  ggplot( aes(reorder(CompanyName, -price), price, fill=CompanyName)) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE)

## Observation : Company Name vs Price
## The highest price is Jaguar followed by Buick and Porsche
price_mean <- carprice_data %>% 
  group_by(CompanyName) %>% 
  select(c(CompanyName, price)) %>% 
  summarise(mean(price))

carprice_data <- left_join(carprice_data, price_mean)

carprice_data <- carprice_data %>% 
                 rename("MeanPrice" = 28)

carprice_data <- carprice_data %>% 
  mutate(carrange = case_when(
    MeanPrice < 10000 ~ "Budget",
    MeanPrice > 10000 & MeanPrice < 20000 ~ "Medium",
    TRUE ~ "Highend"
  )) %>% 
  mutate(
    carrange = factor(
      carrange,
      labels =c("Budget", "Medium", "Highend"),
      levels =c("Budget", "Medium", "Highend"),
      ordered = TRUE
    )
  )

## Label encoding
lbl <- LabelEncoder$new()

carprice_data$fueltype <- lbl$fit_transform(carprice_data$fueltype)
carprice_data$aspiration <- lbl$fit_transform(carprice_data$aspiration)
carprice_data$enginelocation <- lbl$fit_transform(carprice_data$enginelocation)
carprice_data$carbody <- lbl$fit_transform(carprice_data$carbody)
carprice_data$drivewheel <- lbl$fit_transform(carprice_data$drivewheel)
carprice_data$enginetype <- lbl$fit_transform(carprice_data$enginetype)
carprice_data$cylindernumber <- lbl$fit_transform(carprice_data$cylindernumber)
carprice_data$fuelsystem <- lbl$fit_transform(carprice_data$fuelsystem)
carprice_data$carrange <- lbl$fit_transform(carprice_data$carrange)

carprice_data_select <- carprice_data %>% 
  select(c(curbweight, enginesize, horsepower, 
           fueltype, aspiration, enginelocation,
           carbody, drivewheel, enginetype, cylindernumber, 
           fuelsystem, carrange, price))


## Split data
train_test_split <- function(data, train_size = 0.7){
  set.seed(22)
  n <- nrow(data)
  id <- sample(1:n, size=n*train_size)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  
  return(list(train_data, test_data))
}

split_data <- train_test_split(carprice_data_select)
train_data <- split_data[[1]]
test_data <- split_data[[2]]

nrow(train_data)
nrow(test_data)

# 5 folds repeat 3 times
set.seed(22)
crtl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats=3,
                     verboseIter = T)

## Train model
## Random Forest

## Model 1 
set.seed(22)
model_1 <- train( 
  price ~ .,
      data = train_data,
      method = "rf",
      trControl = ctrl
      )

# Feature importance
varImp(model_1)

## Final model
set.seed(22)
model_2 <- train( 
  price ~ enginesize + curbweight + carrange + horsepower,
  data = train_data,
  method = "rf",
  trControl = ctrl)

## Score and evaluation
## Score / prediction
p_2 <- predict(model_2, newdata = split_data[[2]])

## Evaluation
## RMSE from model should close to predict
RMSE(p_2, test_data$price)

#final_model
#R2 is 90.11%
#train RMSE = 2347.098
#test RMSE = 2427.435
