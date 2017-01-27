
weight <- read.csv("weightlift.csv")
names <- colnames(weight)
View(weight)

grep("magnet",names)

getVars <- function(n){
  name <- colnames(weight[,n])
  return(name)
}

lm_weight <- lm(classe~., data=weight)
lm_weight$coefficients

sapply(lm_weight, abs(lm_weight))

a <- sapply(weight, getVars())

