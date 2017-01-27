library("ggplot2")
library("dplyr")

data <- read.csv("IowaHousing.csv")
View(data)

## Begin to model different aspects of homes with their relative prices.

#____________________________________________________________________________________________________________________________________________
# Model 1: By shape
#____________________________________________________________________________________________________________________________________________
shape <- function(n) 
{
  if (n == 'Reg')
    return("regular")
  else 
    return("irregular")
}
data$Shape <- sapply(data$Lot.Shape, shape)

model1 <- ggplot(data, aes(Shape, SalePrice))
model1 <- model1 + geom_boxplot()
model1 <- model1 + xlab("Shape")
model1 <- model1 + ggtitle("Shape to Price")
model1

plot(SalePrice ~ Lot.Shape, data = data, pch = 19)
points(data$Lot.Shape, data$SalePrice, pch=19)

priceModel <- lm(SalePrice ~ Shape, data = data)
summary(priceModel)

# -----------------> Suitable <------------------
# -----------------> Interaction <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/


#____________________________________________________________________________________________________________________________________________
# Model 2: By road access
#____________________________________________________________________________________________________________________________________________
road_access <- function(n) 
{
  if (n == 'Pave')
    return("paved")
  else 
    return("not paved")
}
data$Road.Access <- sapply(data$Street, road_access)

model2 <- ggplot(data, aes(Road.Access, SalePrice))
model2 <- model2 + geom_boxplot()
model2 <- model2 + xlab("Road Access")
model2 <- model2 + ggtitle("Road Access to Price")
model2

priceModel <- lm(SalePrice ~ Shape + Road.Access, data = data)
summary(priceModel)

# -----------------> Suitable <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Model 3: By residential/nonresidential area
#____________________________________________________________________________________________________________________________________________
data$Residential <- ifelse(grepl('R[A-Z]', data$MS.Zoning), "residential", "non-residential")

model3 <- ggplot(data, aes(Residential, SalePrice))
model3 <- model3 + geom_boxplot()
model3 <- model3 + xlab("Residential")
model3 <- model3 + ggtitle("Residential Area to Price")
model3

priceModel <- lm(SalePrice ~ Shape + Road.Access + Residential, data = data)
summary(priceModel)

# -----------------> Suitable <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Model 4: Quality of materials and finish of the house
#____________________________________________________________________________________________________________________________________________
quality <- function(n) 
{
  if (n >= 7)
    return("good and above")
  else 
    return("average and below")
}
data$Quality <- sapply(data$Overall.Qual, quality)

model4 <- ggplot(data, aes(Quality, SalePrice))
model4 <- model4 + geom_boxplot()
model4 <- model4 + xlab("Quality of Materials")
model4 <- model4 + ggtitle("Material Quality to Price")
model4

priceModel <- lm(SalePrice ~ Shape + Road.Access + Residential + Quality, data = data)
summary(priceModel)

# -----------------> Suitable, Remove[Road.Access] <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Model 5: By condition of the house at sell
#____________________________________________________________________________________________________________________________________________
condition <- function(n) 
{
  if (n >= 7)
    return("good and above")
  else 
    return("average and below")
}
data$Condition <- sapply(data$Overall.Cond, condition)

model5 <- ggplot(data, aes(Condition, SalePrice))
model5 <- model5 + geom_boxplot()
model5 <- model5 + xlab("Condition of Home")
model5 <- model5 + ggtitle("Home Condition to Price")
model5

priceModel <- lm(SalePrice ~ Shape + Residential + Quality + Condition, data = data)
summary(priceModel)

# -----------------> Not Suitable, Remove[Condition] <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Model 6: By condition of sale
#____________________________________________________________________________________________________________________________________________
condition_sale <- function(n) 
{
  if (n == "Normal")
    return("normal")
  else 
    return("not normal")
}
data$SaleCond <- sapply(data$Sale.Condition, condition_sale)

model6 <- ggplot(data, aes(SaleCond, SalePrice))
model6 <- model6 + geom_boxplot()
model6 <- model6 + xlab("Condition at Sale of Home")
model6 <- model6 + ggtitle("Condition at Sale to Price")
model6

priceModel <- lm(SalePrice ~ Shape + Residential + Quality + SaleCond, data = data)
summary(priceModel)

# -----------------> Suitable <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Model 7: Special Homes
#____________________________________________________________________________________________________________________________________________
data$Special.Add <- ifelse(grepl('[a-zA-Z]+', data$Misc.Feature), "special", "normal")

model7 <- ggplot(data, aes(Special.Add, SalePrice))
model7 <- model7 + geom_boxplot()
model7 <- model7 + xlab("Some Special Addition")
model7 <- model7 + ggtitle("Special Addition to Price")
model7

priceModel <- lm(SalePrice ~ Shape + Residential + Quality + SaleCond + Special.Add, data = data)
summary(priceModel)

# -----------------> Suitable, but an interaction. Remove[Special.Add] <------------------
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

#____________________________________________________________________________________________________________________________________________
# Homes with quality exteriors
#____________________________________________________________________________________________________________________________________________
ext_qual <- function(n) 
{
  if (n == "Ex" || n == "Gd")
    return("quality")
  else 
    return("avg. to poor")
}
data$Ext.Qual <- sapply(data$Exter.Qual, ext_qual)

model8 <- ggplot(data, aes(Ext.Qual, SalePrice))
model8 <- model8 + geom_boxplot()
model8 <- model8 + xlab("External Quality")
model8 <- model8 + ggtitle("External Quality to Price")
model8

priceModel <- lm(SalePrice ~ Shape + Residential + Quality + SaleCond + Ext.Qual, data = data)
summary(priceModel)

#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/




