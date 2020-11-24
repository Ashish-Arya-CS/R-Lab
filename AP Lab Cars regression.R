cars = read.csv('C:/Users/KIIT/Downloads/cars.csv')

cars[sample(nrow(cars),10),]

XY = read.csv('C:/Users/KIIT/Downloads/XYdata.csv')
XY[sample(nrow(cars),10),]
summary(cars)
summary(XY)

colnames(XY)

names(XY)[names(XY) == "X32.5023452695"] <- "X"
names(XY)[names(XY) == "X31.7070058466"] <- "Y"

colnames(XY)

attach(XY)
plot(X,Y)
title("X Vs Y - XY Dataset")

lm_XY <- lm(Y ~ X)
cat("The R Squared Value for XY =",summary(lm_XY)$r.squared)

a <- summary(lm_XY)
cat("The theta values for XY =",coef(a)[, "t value"])

plot(Y ~ X)
abline(lm_XY$coefficients, col = "blue")
title("Regression Line of XY Dataset")
summary(lm_XY)

preds <- data.frame(X=c(43,315,57,223,52,784,54,55,34,23))
predict(lm_XY, newdata=preds)

attach(cars)
plot(Volume+Weight,CO2)
title("Volume + Weight Vs CO2 -Cars ")

lm_Cars <- lm(CO2 ~ Volume+Weight)
cat("The R Squared Value for Cars =",summary(lm_Cars)$r.squared)

b <- summary(lm_Cars)
cat("Theta values for Cars =",coef(b)[,"t value"])

summary(lm_Cars)

plot(lm_Cars)

preds1 <- data.frame(Volume=1500, Weight=1123)
predict(lm_Cars, newdata = preds1)