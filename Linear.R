library(dplyr)
library(dummies)
library(caTools)
library(glmnet)
library(DAAG)
library(ggplot2)
servo_df <-
  read.csv("C:\\Users\\Sharang\\Documents\\Assignment Sem 2\\Random Forrest\\servo.data", header = FALSE)
head(servo_df)

#Renaming the colnames
colnames(servo_df) <- c('motor', 'screw', 'pgain', 'vgain', 'class')
head(servo_df)
#Removing the output variable columns(Y)
servo_df_1 <- servo_df[-5]
head(servo_df_1)
head(servo_df)
#-------------Graphical-------------
hist(servo_df$class, probability = TRUE)
lines(density(servo_df$class), lwd = 2, col = "red")
ggplot(servo_df, aes(pgain)) + geom_histogram(aes(fill = class),
                                              color = 'Black',
                                              bins = 50,
                                              alpha = 0.5) + theme_bw()
ggplot(servo_df, aes(vgain)) + geom_histogram(aes(fill = class),
                                              color = 'red',
                                              bins = 50,
                                              alpha = 0.5) + theme_bw()

#-------------#correlation------------------
cor(servo_df[c('pgain', 'vgain', 'class')])
pairs(servo_df[c('pgain', 'vgain', 'class')])
#________Creating dummies varibale-----------
encoded_df <- dummy.data.frame(servo_df, sep = ".")
head(encoded_df)
#----------Spliting the data into training and testing set----------------
set.seed(101)
sample <- sample.split(encoded_df, SplitRatio = .70)
train <- subset(encoded_df, sample == T)
test <- subset(encoded_df, sample == F)


# Linear Model
#----------Creating model----------------
linear_model <- lm(class ~ ., data = encoded_df)

linear_model
# summary of the model tells about the R2 Value and Adj.R2
summary(linear_model)
attributes(linear_model)

# Stats
## model coefficients
coefficients(linear_model) 
## CIs for model parameters
confint(linear_model, level = 0.95) 
# predicted values
fitted(linear_model) 
## residuals
residuals(linear_model) 
## anova table
anova(linear_model) 
## covariance matrix for model parameters
vcov(linear_model) 
## regression diagnostics
influence(linear_model) 
## optional 4 graphs/page
layout(matrix(c(1, 2, 3, 4), 2, 2)) 
plot(linear_model)
# Predicting model
rmse_test <- predict(linear_model, data = test)
summary(rmse_test)

rmse_train <- predict(linear_model, data = train)
summary(rmse_train)
# Calulating R2 scored value-

tss <- mean((rmse_test - test$class) ^ 2) / 10
tss
adj_r <- summary(linear_model)$adj.r.squared
adj_r
#--------linearmodel2, Crearing Model2 using Sigficant Features------------------
linear_model_1 <-
  lm(class ~ screw.A + motor.A + pgain + vgain, data = encoded_df)
summary(linear_model_1)


#-----------Stepwise for model 1--------------
step_wise <- step(linear_model)
for_df <- step(linear_model, direction = "forward")
#AIC for Forward Direction is 34.1
bac_df <- step(linear_model, direction = "backward")
#AIC for Forward Direction is 30.1
#--------Stepwise for model 2------------------
step_wise_1 <- step(linear_model_1)
for_df_1 <- step(linear_model_1, direction = "forward")
#AIC for Forward Direction is 47.7
bac_df_1 <- step(linear_model_1, direction = "backward")
#AIC for Forward Direction is 47.7
layout(matrix(c(1, 2, 3, 4), 2, 2)) # optional 4 graphs/page
plot(linear_model_1)

#cross-validation on model 1 and 2 upto 5 folds#####
## Cross Validation for Model 1
cv.lm(encoded_df, linear_model, m = 5)
## Cross Validation for model 2
cv.lm(encoded_df, linear_model_1, m = 5)
#from here we can see that Mean Square of Model_1 is 1.26.
#Mean Square of Model_2 is 1.36 hence from here we can see that Model_1 has the better accuracy




#According to AIC value the Linear_Model is better than the Linear_Model2 by comparing the adj_squared values also Linear Model has 54% and Linear_model2
#Has 47%.
