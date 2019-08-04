#Giancarlo Carino
#EC 410
#Lab 3

#Install packages
install.packages(pkgs=c("psych", "stargazer", "lmtest", "car", "dplyr"))
library(psych)
library(stargazer)
library(lmtest)
library(car)
library(dplyr)

#Import
setwd("/Users/gc/Desktop//GitHub/EC-410-Food-and-Agricultural-Economics/Lab 3/data")
lb3data <- read.csv("lab3_data.csv")

#Summary statistics
summary(lb3data)
stargazer(
  lb3data[c("education", "wage", "experience", "ability", "mothers.education", "fathers.education", "siblings", "male")], type="text", title="Descriptive Statistics", digits=2, out="table.htm", summary.stat=c("n", "mean", "sd", "min", "max")
)
plot(x=lb3data$education, y=lb3data$wage, xlab = "Education", ylab = "Wage",
     main = "Wage and Education")

#Data Transformations
#Create an indicator variable "Siblings_indicator" that is equal to 1
#if siblings > 0 equal to 0 otherwise
lb3data$siblings_indicator <- ifelse(lb3data$siblings > 0, 1, 0)

#Create an interaction variable education*male
lb3data$education_male <- lb3data$education * lb3data$male

#Linear Regression 1
reg1 <- lm(wage ~ male + education + experience + ability + mothers.education + fathers.education + siblings, data = lb3data, na.action = na.exclude)
summary(reg1)

#Linear Regression 2
reg2 <- lm(wage ~ male + education + experience + ability + mothers.education + fathers.education + siblings + education_male, data = lb3data, na.action = na.exclude)
summary(reg2)

#Plot residuals from model against an IDV
#Look for patterns
lb3data$residuals <- resid(reg1)
plot(x=lb3data$education, y=lb3data$residuals, xlab = "Education", ylab= "Residuals", main = "Residuals vs Education")

#Test for Heteroscedasticity: 
#H0: Homoscedasticity (No hetero)
#Ha: Heteroscedascticity
lmtest::bptest(reg1)

#Test for multicollinearity
#H0: No multicollinearity (VIF < 10)
#Ha: Multicollinearity (VIF > 10)
car::vif(reg1)
