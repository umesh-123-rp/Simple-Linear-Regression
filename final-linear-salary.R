# Q2 : Build a simple linear regression model by performing EDA and do necessary transformations
# select the best model using R or Python
# Objective : To build a prediction model for salary hike

# Loading the data 
salary_Data<-read.csv("D:\\Data\\IN102385\\Downloads\\Salary_Data.csv")
View(salary_Data)
summary(salary_Data)
# Mean > Median in both the cases, looks to have positively skewed distribution.
# To be analysed furtherExp<-salary_Data$YearsExperience
describe(salary_Data)
attach(salary_Data)
hist(Salary)
plot(YearsExperience,Salary)
cor(YearsExperience,Salary)
# Correlation Coefficient is 0.97. It indicates strong relation.
install.packages("lattice")
library(lattice)
dotplot(YearsExperience)
dotplot(Salary)
boxplot(YearsExperience,col="blue")
boxplot(Salary,col="yellow")
# There is no outliers for both the variables
library(car)
qqPlot(Salary)
qqPlot(YearsExperience)
hist(Salary)
hist(YearsExperience)
# Plot indicates that both Experience and Salary follow normal distribution

# We can now go for model building as there is strong relation between Salary and Experience
# Model Building
model<-lm(Salary~ YearsExperience, data = salary_Data)
summary(model)
plot(model)
#RMSE
sqrt(sum(model$residuals^2)/nrow(salary_Data))
# R-Square value is 96% which is an excellent model

# Check for further improvement by using transformation in log of Experience
model_log<-lm(Salary~log(YearsExperience), data = salary_Data)
summary(model_log)
#RMSE
sqrt(sum(model_log$residuals^2)/nrow(salary_Data))

# R^2 value reduced to 0.85
# Can use exponential model
model_exp<-lm((log(Salary)~YearsExperience), data = salary_Data)
summary(model_exp)
#RMSE
sqrt(sum(model_exp$residuals^2)/nrow(salary_Data))
# R^2 value got reduced from 0.96 to 0.93.
# Will check by further transformations
model_log_exp<-lm(log(Salary) ~ log(YearsExperience), data=salary_Data)
summary(model_log_exp)
#RMSE
sqrt(sum(model_log_exp$residuals^2)/nrow(salary_Data))
# R^2 value got reduced to 0.9
# We can check with Quadratic model
attach(salary_Data)
model_quad<-lm(Salary~ YearsExperience+I(YearsExperience^2))
summary(model_quad)
#RMSE
sqrt(sum(model_quad$residuals^2)/nrow(salary_Data))
# R^2 further improved to 0.96
# We wikll check with polynomial model
model_poly<-lm(Salary~YearsExperience+I(YearsExperience^2)+I(YearsExperience^3))
summary(model_poly)
#RMSE
sqrt(sum(model_poly$residuals^2)/nrow(salary_Data))
# R^2 further improved to 0.96
# We can check influencing data points for further improvements
influenceIndexPlot(model_poly)
salary_Data1<-salary_Data[-c(9,20,30),]
model_iip<-lm(Salary~YearsExperience, data=salary_Data1)
summary(model_iip)
#RMSE
sqrt(sum(model_iip$residuals^2)/nrow(salary_Data))
# This also gives R^2 value as 0.96
#Summerization of models
model_R_Squared_values <- list(model=NULL,R_squared=NULL,RMSE=NULL)
model_R_Squared_values[["model"]] <- c("model","model_log","model_exp","model_log_exp","model_quad","model_poly","model_iip")
model_R_Squared_values[["R_squared"]] <- c(0.96,0.85,0.93,0.90,0.96,0.96,0.96)
model_R_Squared_values[["RMSE"]]<-c(5592,10302,0.094,0.11,5590,5142,4875)
Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]],model_R_Squared_values[["RMSE"]])
View(model_R_Squared_values)
View(Final)
# Check for residuals
hist(model$residuals)
hist(model_log$residuals)
hist(model_exp$residuals)
hist(model_log_exp$residuals)
hist(model_quad$residuals)
hist(model_poly$residuals)
hist(model_iip$residuals)
# Among all above, the first model is close to normal distribution and can be considered final
# Therefore, Without sacrificing any data points we can go with the original model
final_model<-lm(Salary~ YearsExperience, data = salary_Data)
summary(final_model)
plot(final_model)
confint(final_model,level=0.95)
pred<-predict(final_model,interval="predict")
pred1<-data.frame(pred)
cor(pred1$fit,salary_Data$Salary)
# Correlation coefficient between fitted value and actual is 0.98
# This model can be considered as best model