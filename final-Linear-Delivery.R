#Q1 : Build a simple linear regression model by performing EDA and do necessary transformations and 
# select the best model using R or Python 

# Objective :  To Predict delivery time using sorting time

# Loading the data
delivery_time<-read.csv("D:\\Data\\IN102385\\Downloads\\delivery_time.csv")
View(delivery_time)
str(delivery_time)
summary(delivery_time)
install.packages("psych")
library(psych)
describe(delivery_time)
hist(delivery_time$Delivery.Time)
hist(delivery_time$Sorting.Time)
# For Delivery Time - Skewness is 0.3 and mean is close to median and histogram shows that it is 
# normally distributed
# For Sorting time- Although skewness is low and mean is close to median, 
# but hIstogram does not look to be not normally distributed
install.packages("lattice")
library(lattice)
attach(delivery_time)
dotplot(Delivery.Time)
qqPlot(Delivery.Time)
dotplot(Sorting.Time)
qqPlot(Sorting.Time)
# Dotplot indicates that Delivery time is concentrated in the middle whereas sorting time is 
# Both Delivery time and Sorting Time are normally distributed as evident from 
# Q-Q Plot
# equally distributed throughout 
boxplot(Delivery.Time, col="Blue")
boxplot(Sorting.Time, col="red")
# Box Plot indicates that there are no outliers in both sorting and Delivery time
plot(Sorting.Time, Delivery.Time)
# Scatter plot indicates that as there is a linear increasing trend.
# As sorting time increases, Delivery time also increases
cor(Sorting.Time,Delivery.Time)
# Correlation coefficient value is 0.82 which shows medium positive correlation

# Model Building
model<-lm(Delivery.Time~ Sorting.Time,data = delivery_time)
summary(model)
# R^2 value is 0.68 which shows that model needs further improvement
plot(model)
# Normal QQ Plot indicates that there is a deviation from normal
# RMSE
sqrt(sum(model$residuals^2)/nrow(delivery_time))
# Applying sqrt in Sorting time
model_sqrt_sort<-lm(Delivery.Time~ sqrt(Sorting.Time),data = delivery_time)
summary(model_sqrt_sort)
#RMSE
sqrt(sum(model_sqrt_sort$residuals^2)/nrow(delivery_time))
# R^2 value has improved to 0.69
# Applying transformation of sqrt in Delivery time
model_sqrt_del<-lm(sqrt(Delivery.Time)~ Sorting.Time,data = delivery_time)
summary(model_sqrt_del)
#RMSE
sqrt(sum(model_sqrt_del$residuals^2)/nrow(delivery_time))
# R^2 has improved to 0.70
# Applying transformation on square of sorting time
model_sq_sort<-lm(Delivery.Time~ (Sorting.Time^2),data = delivery_time)
summary(model_sq_sort)
#RMSE
sqrt(sum(model_sq_sort$residuals^2)/nrow(delivery_time))
# R^2 value reduced to 0.68
# Checking with sqaure of delivery time
model_sq_del<-lm((Delivery.Time^2)~ Sorting.Time,data = delivery_time)
summary(model_sq_del)
sqrt(sum(model_sq_del$residuals^2)/nrow(delivery_time))
# R^2 value got further reduced to 0.6
# Applying Transformations of Log(Sorting Time)
model_log<-lm(Delivery.Time~log(Sorting.Time),data = delivery_time)
summary(model_log)
# There is very slight improvement in R^2 value 
plot(model_log)
# There is improvement in residual plot
# RMSE
sqrt((sum(model_log$residuals^2))/nrow(delivery_time))
# Applying transformation by taking log of delivery time
model_exp<-lm(log(Delivery.Time)~Sorting.Time,data=delivery_time)
summary(model_exp)
# There is very slight improvement in R^2 value
# RMSE 
sqrt(sum(model_exp$residuals^2)/nrow(delivery_time))
plot(model_exp)
# Applying square of the sorting time
attach(delivery_time)
model_quad<-lm(Delivery.Time~ (Sorting.Time)+ I(Sorting.Time^2))
summary(model_quad)
# RMSE 
sqrt(sum(model_quad$residuals^2)/nrow(delivery_time))
# Poly model
model_poly<-lm(Delivery.Time~ (Sorting.Time)+ I(Sorting.Time^2)+I(Sorting.Time^3))
summary(model_poly)
# RMSE 
sqrt((sum(model_poly$residuals^2))/nrow(delivery_time))
# No improvement in R^2 value
# Applying transformation by taking log of delivery time and log of sorting time
model_log_exp<-lm(log(Delivery.Time)~log(Sorting.Time),data=delivery_time)
summary(model_log_exp)
# There is an improvement of R^2 value up to 0.77
# RMSE
sqrt(sum(model_log_exp$residuals^2)/nrow(delivery_time))
plot(model_log_exp)
# There could be many other influencing variables than sorting time.
# To improve the delivery time, We can check the influencing data points
# Identified data  points are 5,9 and 21
# Further improvement by eliminating influencing data points
install.packages("car")
library(car)
influenceIndexPlot(model_log_exp)
delivery_time1<-delivery_time[-c(5,9,21),]
View(delivery_time1)
qqPlot(delivery_time1$Delivery.Time)
qqPlot(delivery_time1$Sorting.Time)
model_iip<-lm(Delivery.Time~Sorting.Time,data = delivery_time1)
summary(model_iip)
# There is an improvement in R^2 value from 0.77 to 0.83
plot(model_iip)
confint(model_iip,level=0.95)
predict(model_iip,interval="predict")
# RMSE 
sqrt(sum(model_iip$residuals^2)/nrow(delivery_time1))
# Summerization of all models
model_R_Squared_values <- list(model=NULL,R_squared=NULL,RMSE=NULL)
model_R_Squared_values[["model"]] <- c("model","model_sqrt_sort","model_sqrt_del","model_sq_sort","model_sq_del","model_log","model_exp","model_quad","model_poly","model_log_exp","model_iip")
model_R_Squared_values[["R_squared"]] <- c(0.68,0.69,0.70,0.68, 0.60,0.69,0.71,0.69,0.70,0.77,0.83)
model_R_Squared_values[["RMSE"]]<-c(2.79,2.73,0.33,2.79,111.9,2.73,0.16,2.74,2.69,0.15,1.73)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]],model_R_Squared_values[["RMSE"]])
View(model_R_Squared_values)
View(Final)
# Final Model is as given below
Final_model<- lm(Delivery.Time~Sorting.Time, data=delivery_time[-c(5,9,21),])
summary(Final_model)
# Conclusion : There were 3 influencing data by removing which the model becomes strong.
