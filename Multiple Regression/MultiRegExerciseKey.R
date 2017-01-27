cars <- read.csv("04cars.csv")
head(cars)

# Predict Horsepower
plot(HP ~ EngineSize, data = cars, pch = 19, col="blue")
hpModel <- lm(HP ~ EngineSize, data = cars)
summary(hpModel)
hpModel$coefficients
abline(hpModel, lwd=2)

# Interval

# Interval for individual value
xVar <- data.frame(EngineSize=4)
# Average
singleConfInt <- predict(hpModel, xVar, interval = "confidence")
singleConfInt

xVar <- data.frame(EngineSize=4)
# Generalization (not an average)
singlePredInt <- predict(hpModel, xVar, interval = "prediction")
singlePredInt

# Explanatory
explanatory <- data.frame(EngineSize=cars$EngineSize)
hpConfInt <- predict(hpModel, explanatory, interval = "confidence")
str(hpConfInt)
hpConfInt[1,]
lines(cars$EngineSize, hpConfInt[,2], lty=3, lwd=2, col="darkgrey")
lines(cars$EngineSize, hpConfInt[,3], lty=3, lwd=2, col="red")

# PredictionhpConfInt <- predict(hpModel, explanatory, interval = "confidence")
hpPredInt <- predict(hpModel, explanatory, interval = "prediction")
hpPredInt[1,]
lines(cars$EngineSize, hpPredInt[,2], lty=3, lwd=2, col="green") # Lower bound of confidence interval
lines(cars$EngineSize, hpPredInt[,3], lty=3, lwd=2, col="blue") # Upper bound of confidence interval

# Examine for sport and non-sport
plot(HP ~ EngineSize, data = cars, pch = 19)
points(cars$EngineSize, cars$HP, pch=19, col=((cars$sport==1)*1+1))

sportFilter <- cars$sport==1
hpModelSport <- lm(cars$HP[sportFilter] ~ cars$EngineSize[sportFilter])
hpModelNotSport <- lm(cars$HP[!sportFilter] ~ cars$EngineSize[!sportFilter])
abline(hpModelSport,lwd=2,col="red")
abline(hpModelNotSport,lwd=2)
hpModelSport$coefficients
hpModelNotSport$coefficients

hpModelBoth <- lm(HP ~ EngineSize + sport, data = cars)
plot(HP ~ EngineSize + sport, data = cars, pch=19)
summary(hpModelBoth)
hpModelBoth$coefficients




