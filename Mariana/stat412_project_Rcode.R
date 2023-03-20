####################
#packages required
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MASS)
library(glmnet)
library(corrplot)
library(car)

#load and clean the data
food_data <- read_csv("foods_dataset.csv")

dataMinus <- food_data[c(5,15, 16, 17, 18, 20, 21, 22, 23, 26)]
dataMinus <- na.omit(dataMinus)

#need to reshape zip_region variable to numeric categories
#create a new factor variable with the desired levels
dataMinus$zip_cat <- factor(dataMinus$zip_region, levels = c("northeast", "south", "midwest", "west"))

#convert the factor levels into numeric values
dataMinus$zip_region <- as.numeric(dataMinus$zip_cat) - 1 #this will ensure count starts at 0 

dataMinus <- dataMinus[,-11] #dropped zip_cat, don't need it anymore

#Histograms of Outcome Variable
fruit_paid <- ggplot(food_data, aes(x=food_data$fruit_paid))+
  geom_histogram(color="darkblue", fill="lightblue") + ggtitle("Original")

ln_fruit_paid <- ggplot(dataMinus, aes(x=dataMinus$ln_fruit_paid))+
  geom_histogram(color="darkblue", fill="lightblue") + ggtitle("Log Transformation")

ggarrange(fruit_paid, ln_fruit_paid, 
          ncol = 2, nrow = 2)

#Tables of Categorical Variables
dataMinus %>% count(dataMinus$children)
dataMinus %>% count(dataMinus$marital_status)
dataMinus %>% count(dataMinus$employed_new)
dataMinus %>% count(dataMinus$urban)
dataMinus %>% count(dataMinus$hh_size)
dataMinus %>% count(dataMinus$edu)
dataMinus %>% count(dataMinus$race)
dataMinus %>% count(dataMinus$income)
dataMinus %>% count(dataMinus$zip_region)

#Correlation Plot
M<-cor(dataMinus)
corrplot(M, type="upper")

#Linear Model
linear_mod <- lm(ln_fruit_paid ~ children + marital_status + employed_new + urban + hh_size + edu + race + income + zip_region, data = dataMinus)
summary(linear_mod)
plot(linear_mod)

#checking out the outliers
plot(linear_mod, 4, id.n=10)

threshold <- 4/22448
cooksD <- cooks.distance(linear_mod)

outlier <- cooksD[(cooksD > threshold)]
#outlier

outlierTest(linear_mod)

#Linear Model with Influential Observations Excluded
dataNoOutliers <- dataMinus[-c(1095, 864, 15928, 20807, 19591, 1441, 8523, 18820, 17320, 6939),]

mod_no_outliers <- lm(ln_fruit_paid ~ children + marital_status + employed_new + urban + edu + race + income + zip_region, data = dataNoOutliers)
summary(mod_no_outliers)
plot(mod_no_outliers)

#Robust Regression Model
robust_mod <- rlm(ln_fruit_paid ~ children + marital_status + employed_new + urban + hh_size + edu + race + income + zip_region, method = "M", data = dataMinus)
summary(robust_mod)
plot(robust_mod)

#weights of observations
hweights <- data.frame(resid = robust_mod$resid, weight = robust_mod$w)
hweights2 <- hweights[order(robust_mod$w), ]
hweights2[1:10, ]

#Ridge Regression Model
#define x and y 
y = dataMinus$ln_fruit_paid
x = data.matrix(dataMinus[-c(1)])

ridge_mod <- glmnet(x,y, alpha=0)
#alpha = 0 is Ridge, alpha = 1 is LASSO
#perform k-fold cross-validation to find optimal lambda value
cv_mod <- cv.glmnet(x,y, alpha=0)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_mod$lambda.min
best_lambda

#produce plot of test MSE by lambda value
plot(cv_mod)

#find coefficients of best model 
best_mod <- glmnet(x, y, alpha=0, lambda=best_lambda) #now we can add the optimal lambda value in the model
coef(best_mod)

#use fitted best model to make predictions
y_predicted <- predict(ridge_mod, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

#produce Ridge trace plot
plot(ridge_mod, xvar = "lambda")

