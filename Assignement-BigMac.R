# Removing the global variables
rm(list = ls())

# Setting the working directory
setwd("I:/DATA-SCIENCE/Insofe/Assignments/SimpleLinearRegression-Assignment")
getwd()

# Importing the data from the excel file
bigMac_netHrWages_df = read.csv(file = "BigMac-NetHourlyWage.csv" , header = TRUE)


# Analysing the data
fix(bigMac_netHrWages_df)
plot(bigMac_netHrWages_df$Big.Mac.Price...., bigMac_netHrWages_df$Net.Hourly.Wage....)
cor(bigMac_netHrWages_df$Big.Mac.Price...., bigMac_netHrWages_df$Net.Hourly.Wage....)^2

# Creating a linear regression model
linearRegressionModel = lm(Net.Hourly.Wage....~Big.Mac.Price...., data = bigMac_netHrWages_df)

# Analysing the linear regression model
coefficients(linearRegressionModel)
summary(linearRegressionModel)

# Analysis on the following:-
# Linearity of the linearity of the model
# Independence of errors
# Homoscedasticity 
# Normality of errors (Q-Q plot)
par(mfrow = c(2,2))
plot(linearRegressionModel)

# Estimatig the hourly wages in a country if the big mac price is 3.33
beta0 = coefficients(linearRegressionModel)[1]
beta1 = coefficients(linearRegressionModel)[2]
as.numeric(beta0 + (beta1 * 3.33))

# Using the predictions
testdata = data.frame(Big.Mac.Price.... = 3.33)
testdata
predict(linearRegressionModel, testdata)

########################################################################################3
# Removing the global variables
rm(list = ls())

# Setting the working directory
setwd("I:/DATA-SCIENCE/Insofe/Assignments/SimpleLinearRegression-Assignment")
getwd()

# Importing the data from the excel file
bigMac_netHrWages_df = read.csv(file = "BigMac-NetHourlyWage.csv" , header = TRUE)
library(e1071)
skewness(log10(bigMac_netHrWages_df$Big.Mac.Price....))
bigMac_netHrWages_df$LOg10_BigMacPrice = log10(bigMac_netHrWages_df$Big.Mac.Price....)

# Creating a new linear model with log(bigMac_netHrWages_df$Big.Mac.Price....)
linearRegressionModel1 = lm(bigMac_netHrWages_df$Net.Hourly.Wage....~LOg10_BigMacPrice, data = bigMac_netHrWages_df)
par(mfrow = c(2,2))
plot(linearRegressionModel1)
library(DMwR)
regr.eval(bigMac_netHrWages_df$Net.Hourly.Wage...., linearRegressionModel1$fitted.values)




###############################################################################################
# Removing the global variables
rm(list = ls())

# Setting the working directory
setwd("I:/DATA-SCIENCE/Insofe/Classes/June-18th/20170618_Batch30_CSE_7302c_Lab01_SimpleLinReg")
getwd()

# Importing the data from the excel file
bigMac_netHrWages_df = read.csv(file = "BigMac-NetHourlyWage.csv" , header = TRUE)

# Spliting the data into train and test
set.seed(1234)
trainRows = sample(seq(1,nrow(bigMac_netHrWages_df),1), (70 * nrow(bigMac_netHrWages_df))/100)
trainData = bigMac_netHrWages_df[trainRows,]
testData = bigMac_netHrWages_df[-trainRows,]

library(e1071)
skewness(trainData$Big.Mac.Price....)
trainData$LOg10_BigMacPrice = log10(trainData$Big.Mac.Price....)
skewness(trainData$LOg10_BigMacPrice)


# Creating a linear model with split train data
linearRegressionMode2 = lm(Net.Hourly.Wage....~LOg10_BigMacPrice, data = trainData)
summary(linearRegressionMode2)
plot(linearRegressionMode2)
library(DMwR)
regr.eval(trainData$Net.Hourly.Wage...., linearRegressionMode2$fitted.values)

#Error verification on test data
testData$LOg10_BigMacPrice = log10(testData$Big.Mac.Price....)
Pred<-predict(linearRegressionMode2,testData)
regr.eval(testData$Net.Hourly.Wage...., Pred)

