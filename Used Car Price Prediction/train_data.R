#Used Car Price Prediction
#Loading and Cleaning the Dataset
library(readxl)
train_data <- read_excel("D:/PGCBAA/7 Predictive Modeling using R/Project/train-data.xlsx")
View(train_data)
#Dropping the 
train_data <- subset(train_data, select = -c(`New_Price`))
# Converting Mileage, Engine, and Power columns to numeric
train_data$Mileage <- as.numeric(gsub(" km/kg| kmpl", "", train_data$Mileage))
train_data$Engine <- as.numeric(gsub(" CC", "", train_data$Engine))
train_data$Power <- as.numeric(gsub(" bhp", "", train_data$Power))
#Handling missing values
train_data$Mileage[is.na(train_data$Mileage)] <- median(train_data$Mileage, na.rm = TRUE)
train_data$Engine[is.na(train_data$Engine)] <- median(train_data$Engine, na.rm = TRUE)
train_data$Power[is.na(train_data$Power)] <- median(train_data$Power, na.rm = TRUE)
train_data$Seats[is.na(train_data$Seats)] <- median(train_data$Seats, na.rm = TRUE)
#Convert Categorical Variables
#Categorical variables are converted to factors
train_data$Location <- as.factor(train_data$Location)
train_data$Fuel_Type <- as.factor(train_data$Fuel_Type)
train_data$Transmission <- as.factor(train_data$Transmission)
train_data$Owner_Type <- as.factor(train_data$Owner_Type)
#Descriptive Statistics to get summary of data
summary(train_data)
#Fitting Multiple Regression Model
model <- lm(Price ~ Year + Kilometers_Driven + Mileage + Engine + Power + Seats + Location + Fuel_Type + Transmission + Owner_Type, data = train_data)
summary(model)
#Model in Equation Form
cat("Price =", coef(model)[1], "+", coef(model)[2], "* Year +", coef(model)[3], "* Kilometers_Driven +", coef(model)[4], "* Mileage +", coef(model)[5], "* Engine +", coef(model)[6], "* Power +", coef(model)[7], "* Seats +", coef(model)[8], "* Location +", coef(model)[9], "* Fuel_Type +", coef(model)[10], "* Transmission +", coef(model)[11], "* Owner_Type")
#Model Diagnostics
#Normality of Residuals
par(mfrow=c(2,2))
plot(model)
#Multicollinearity (VIF)
vif(model)
library(car)
vif(model)
#Homoscedasticity
plot(model$fitted.values, residuals(model), main="Homoscedasticity Check", xlab="Fitted Values", ylab="Residuals") abline(h=0, col="red")
library(lmtest)
plot(model$fitted.values, residuals(model), main="Homoscedasticity Check", xlab="Fitted Values", ylab="Residuals") abline(h=0, col="red")
plot(model$fitted.values, residuals(model),
     main = "Homoscedasticity Check",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
#Independence of Residuals
durbinWatsonTest(model)
save.image("~/train_data.RData")
save.image("D:/PGCBAA/7 Predictive Modeling using R/Project/train_data.RData")
load("~/train_data.RData")
#Used Car Price Prediction
#Load and Clean the Dataset
library(readxl)
train_data <- read_excel("D:/PGCBAA/7 Predictive Modeling using R/Project/train-data.xlsx")
View(train_data)
train_data <- subset(train_data, select = -c(`New_Price`))
# Converting Mileage, Engine, and Power columns to numeric
train_data$Mileage <- as.numeric(gsub(" km/kg| kmpl", "", train_data$Mileage))
train_data$Engine <- as.numeric(gsub(" CC", "", train_data$Engine))
train_data$Power <- as.numeric(gsub(" bhp", "", train_data$Power))
#Handling missing values
train_data$Mileage[is.na(train_data$Mileage)] <- median(train_data$Mileage, na.rm = TRUE)
train_data$Engine[is.na(train_data$Engine)] <- median(train_data$Engine, na.rm = TRUE)
train_data$Power[is.na(train_data$Power)] <- median(train_data$Power, na.rm = TRUE)
train_data$Seats[is.na(train_data$Seats)] <- median(train_data$Seats, na.rm = TRUE)
#Convert Categorical Variables
#Categorical variables are converted to factors
train_data$Location <- as.factor(train_data$Location)
train_data$Fuel_Type <- as.factor(train_data$Fuel_Type)
train_data$Transmission <- as.factor(train_data$Transmission)
train_data$Owner_Type <- as.factor(train_data$Owner_Type)
#Descriptive Statistics to get summary of data
summary(train_data)
#Fitting Multiple Regression Model
model <- lm(Price ~ Year + Kilometers_Driven + Mileage + Engine + Power + Seats + Location + Fuel_Type + Transmission + Owner_Type, data = train_data)
summary(model)
#Model in Equation Form
cat("Price =", coef(model)[1], "+", coef(model)[2], "* Year +", coef(model)[3], "* Kilometers_Driven +", coef(model)[4], "* Mileage +", coef(model)[5], "* Engine +", coef(model)[6], "* Power +", coef(model)[7], "* Seats +", coef(model)[8], "* Location +", coef(model)[9], "* Fuel_Type +", coef(model)[10], "* Transmission +", coef(model)[11], "* Owner_Type")
#Model Diagnostics
#Normality of Residuals
par(mfrow=c(2,2))
plot(model)
#Multicollinearity (VIF)
vif(model)
#Homoscedasticity
plot(model$fitted.values, residuals(model),
     main = "Homoscedasticity Check",
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
#Independence of Residuals
durbinWatsonTest(model)
