# importing packages for regression and tidyverse
library(tidyverse)
require(MASS)
require(ISLR)

# importing dataset - hard coded
housingData <- read_csv("https://www.dropbox.com/s/llq5c6lrtu4mdgf/housing.csv?dl=1")

# quick examination of the California Housing Dataset
View(housingData)
names(housingData)

# finding out the correlation between variables through visualization and plotting
require(corrplot)
houseCor <- housingData[,1:9]
houseCor <- na.omit(houseCor)
housingData <- na.omit(housingData)
houses <- cor(houseCor)
corrplot(houses, method = "circle")
houseCorMat <- as.data.frame(corrplot(houses,method = "number"))

# finding out which variables have a correlation of more than 50%
names(houseCorMat) <- names(houseCor)
row.names(houseCorMat)[abs(houseCorMat$total_rooms) > 0.50]

# predicting the median house value as a function of other variables
lm.fit=lm(median_house_value~median_income,data=housingData)
attach(housingData)
lm.fit=lm(median_house_value~median_income)
summary(lm.fit)

# confidence interval of 95%
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

# visualizing data
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(median_income,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")

# multiple linear regression
lm.fit=lm(median_house_value~median_income+households,data=housingData)
summary(lm.fit)
lm.fit=lm(median_house_value~.,data=housingData)
summary(lm.fit)
lm.fit1=lm(median_house_value~.-median_income,data=housingData)
summary(lm.fit1)
lm.fit1=update(lm.fit, ~.-median_income)


summary(lm(median_house_value~median_income*households,data=housingData))
lm.fit2=lm(median_house_value~median_income+I(median_income^2))
summary(lm.fit2)
lm.fit=lm(median_house_value~median_income)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
lm.fit5=lm(median_house_value~poly(median_income,5))
summary(lm.fit5)
summary(lm(median_house_value~log(households),data=housingData))

summary(lm(median_house_value~log(median_income),data=housingData))
summary(lm(median_house_value~log(latitude),data=housingData))
summary(lm(median_house_value~log(housing_median_age),data=housingData))
summary(lm(median_house_value~log(total_rooms),data=housingData))
summary(lm(median_house_value~log(total_bedrooms),data=housingData))
summary(lm(median_house_value~log(population),data=housingData))
summary(lm(median_house_value~log(households),data=housingData))

lm.fit2=lm(median_house_value~median_income+I(median_income^2))
summary(lm.fit2)

lm.fit3=lm(median_house_value~poly(median_income,5))
summary(lm.fit3)
lm.fit3=lm(median_house_value~poly(longitude,5))
summary(lm.fit3)
lm.fit3=lm(median_house_value~poly(latitude,5))
summary(lm.fit3)
lm.fit3=lm(median_house_value~poly(housing_median_age,5))
summary(lm.fit3)
lm.fit3=lm(median_house_value~poly(total_rooms,5))
summary(lm.fit3)
lm.fit3=lm(median_house_value~poly(total_bedrooms,5))
summary(lm.fit3)
lm.fit4=lm(median_house_value~factor(population),data=housingData)
summary(lm.fit4)
lm.fit3=lm(median_house_value~poly(households,5))
summary(lm.fit3)


lm.fit5=lm(median_house_value~median_income + factor(ocean_proximity) + log(total_rooms),data=housingData)
summary(lm.fit5)

lm.fit=lm(median_house_value~median_income+factor(ocean_proximity)+poly(longitude, 5))
plot(median_income,median_house_value)
abline(lm.fit,lwd=3,col="red")

lm.fit=lm(median_house_value~median_income,data=housingdata)
plot(median_income,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~longitude,data=housingdata)
plot(longitude,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~latitude,data=housingdata)
plot(latitude,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~housing_median_age,data=housingdata)
plot(housing_median_age,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~total_rooms,data=housingdata)
plot(total_bedrooms,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~population,data=housingdata)
plot(population,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
lm.fit=lm(median_house_value~households,data=housingdata)
plot(households,median_house_value)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")