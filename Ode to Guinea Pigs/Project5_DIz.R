library(ggplot2)
library(dplyr)


##  Load the ToothGrowth data set (included with R data sets). [Read the help file for this data set
## to familiarize yourself with the data; you will need to include some of this information in your
## report to provide context and explain what you are doing.]
data <- data.frame(ToothGrowth)
?ToothGrowth
View(data)
# Adding a variable to help distinguish dose
classify <- function(n) 
{
  if (n >= 2)
    return("High")
  else if(n>=1)
    return("Moderate")
  else
    return("Low")
}
data$dosage <- sapply(data$dose,classify)

##  Create plots to compare growth. Make a single plot for each bullet item below. Each plot should
## show side-by-side boxplots to allow comparison.
# By delivery method of supplement:
data_plot1 <- ggplot(data, aes(supp, len))
data_plot1 <- data_plot1 + geom_boxplot()
data_plot1

# The orange juice supplement produces a higher amount
# of growth in the teeth.It has a higher values in its 5# summary 

# By dosage
data_plot2 <- ggplot(data, aes(factor(dose), len))
data_plot2 <- data_plot2 + geom_boxplot()
data_plot2

# The high dosage amount produces the greatest amount of
# tooth length growth.

# By both variables combined
data_plot3 <- ggplot(data, aes(supp, len))
data_plot3 <- data_plot3 + facet_grid(.~dose)
data_plot3 <- data_plot3 + geom_boxplot()
data_plot3

# From the combined graphs, the highest dosage amount
# produces the most growth. This growth trend is present
# of the supplements. It actually seems to be that dosage
# amount has a strong influence in tooth growth.

## Construct a 95% confidence interval for mean amount of growth one could expect for
# Guinea pigs who receive a supplement as ascorbic acid (any dosage):
data_vc <- data %>% 
  select(supp, len) %>% 
  filter(supp == "VC")
t_test_vc <- t.test(data_vc$len)
conf_vc <- t_test_vc$conf.int
conf_vc

?t.test

# Guinea pigs who receive 2 mg of the supplement, delivered via orange juice:
data_oj <- data %>% 
  select(dosage, supp, len) %>% 
  filter(supp == "OJ") %>% 
  filter(dosage == "High")
data_oj
t_test_oj <- t.test(data_oj$len)
conf_oj <- t_test_vc$conf.int
conf_oj

## Conduct t-tests* to compare mean growth between the following pairs.
## Between guinea pigs who receive ascorbic acid and those who receive orange juice,
## regardless of dosage (2-sided test).
# Ho: The growth of teeth is equal when the supplements is ascorbic acid or orange juice.
# Ha: The growth of teeth is not equal when the supplements is ascorbic acid or orange juice.
test1 <- t.test(data_vc$len, data_oj$len)
test1
# Explain


# Between guinea pigs who receive a 1 mg dose from orange juice and those who receive
# a 2 mg dose from orange juice (1-sided test)
data_2_oj_mod <- data %>% 
  filter(supp == "OJ") %>% 
  filter(dosage == "Moderate")
data_2_oj_high <- data %>% 
  filter(supp == "OJ") %>% 
  filter(dosage == "High")
# Ho: A moderate amount of dosage will be equal to a high amount of dosage.
# Ha: A moderate amount of dosage will be less than a high amount of dosage.
test2 <- t.test(data_2_oj_mod$len, data_2_oj_high$len, alternative = "less")
test2
# The result is significant, so the test rejects the null hypothesis.
# The 2 populations being compared are the guinea pigs that are given 
#   a moderate dosage of orange juice and a high dosage of orange juice.
#   It is likely that a a moderate amount of dosage produces less growth
#   than a high dosage.

## Between guinea pigs who receive a 1 mg dose from ascorbic acid and those who receive
## a 2 mg dose from ascorbic acid (1-sided test)
data_3_vc_mod <- data %>% 
  filter(supp == "VC") %>% 
  filter(dosage == "Moderate")
data_3_vc_high <- data %>% 
  filter(supp == "VC") %>% 
  filter(dosage == "High")
# Ho: A moderate dosage of ascorbic acid produces tooth growth equal to 
#   the high dosage of ascorbic acid.
# Ha:  A moderate dosage of ascorbic acid produces tooth growth less than 
#   the high dosage of ascorbic acid.
test3 <- t.test(data_3_vc_mod$len, data_3_vc_high$len, alternative = "less")
test3
# The result is significant, so the test rejects the null hypothesis.
# The 2 populations being compared are the guinea pigs that are given 
#   a moderate dosage of ascorbic acid and a high dosage of ascorbic acid. There
#   is a significant difference between the moderate dosage and the high dosage.

## Between guinea pigs who receive a 1 mg dose from ascorbic acid and those who receive
## a 1 mg dose from orange juice (2-sided test)
data_4_vc_mod <- data %>% 
  filter(supp=="VC") %>% 
  filter(dosage=="Moderate")
data_4_oj_mod <- data %>% 
  filter(supp=="OJ") %>% 
  filter(dosage=="Moderate")
# Ho: A moderate dosage of ascorbic acid produces tooth growth equal to 
#   a moderate dosage of orange juice.
# Ha:  A moderate dosage of ascorbic acid produces tooth growth not equal to 
#   the moderate dosage of orange juice.
test4 <- t.test(data_4_vc_mod$len,data_4_oj_mod$len)
test4
# The result is significant, so the test rejects the null hypothesis.
# The 2 populations being compared are the guinea pigs that are given 
#   a moderate dosage of ascorbic acid and a moderate dosage of orange juice. There
#   is a significant difference between the moderate dosage of ascorbic acid
#   and moderate dosage of orange juice.

# Between guinea pigs who receive a 2 mg dose from ascorbic acid and those who receive
# a 2 mg dose from orange juice (2-sided test)
data_5_vc_high <- data %>% 
  filter(supp=="VC") %>% 
  filter(dosage=="High")
data_5_oj_high <- data %>% 
  filter(supp=="OJ") %>% 
  filter(dosage=="High")
# Ho: A high dosage of ascorbic acid produces tooth growth equal to 
#   a high dosage of orange juice.
# Ha:  A high dosage of ascorbic acid produces tooth growth not equal to 
#   the high dosage of orange juice.
test5 <- t.test(data_5_vc_high$len,data_5_oj_high$len)
test5
# The result is not significant, so the test fails to reject the null hypothesis.
# The 2 populations being compared are the guinea pigs that are given 
#   a high dosage of ascorbic acid and a high dosage of orange juice. There
#   is no significant difference between the high dosage of ascorbic acid
#   and high dosage of orange juice.

