library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

## Set reference up incase of use
#REF <- readRDS("Source_Classification_Code.rds")
#r <- head(REF)
#View(r)
#tbl_df(r)

# Analysing the head because of ~6million records.
NEI <- readRDS("summarySCC_PM25.rds")
h <- head(NEI)
View(h)
tbl_df(h)

##  Have total PM2.5 emissions decreased in the United States from 1999 to 2008?
## Your plot(s) should show the total PM2.5 emission (from all sources) for each of the years
## 1999, 2002, 2005, and 2008?
# Summarize the data for each respective year
data1 <- NEI %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))
# Plot the summarized data for Question 1
q1 <- ggplot(data1, aes(year, Emissions))
q1 <- q1 + geom_point(aes(color=factor(year)), size = 4)
q1 <- q1 + geom_smooth(method = "lm", se=FALSE, size = .7)
q1 <- q1 + geom_line()
q1 <- q1 + scale_y_continuous(labels = scales::comma)
q1 <- q1 + xlab("Year") + labs(color = "Year")
q1 <- q1 + ggtitle("Total Emissions by Year")
# Print the plot
#png(file="Q1.png")
q1
#dev.off()
print("Q1: Yes, total emissions have been declining.")

##  Have total emissions from PM2.5 decreased in the area of Baltimore City, Maryland from
## 1999 to 2008?
# Filter for the desired fips code records
data2_a <- filter(NEI, fips==24510)
data2_b <- data2_a %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))
data2_b
# Plot the filtered data
q2 <- ggplot(data2_b, aes(year, Emissions))
q2 <- q2 + geom_point(aes(color=factor(year)), size = 3) 
q2 <- q2 + geom_line()
q2 <- q2 + geom_smooth(method = "lm", se = FALSE, size = .7)
q2 <- q2 + xlab("Year") + labs(color = "Year")
q2 <- q2 + ggtitle("Total Emissions by Year in Baltimore, MD")
# Print the plot
#png(file="Q2.png")
q2
#dev.off()
print("Q2: Yes, total emissions from the baltimore area have been declining in total. However they had a spike in 2005.")

##  The type variable identifies four types of pollutant sources (point, nonpoint, onroad,
## nonroad). Of these four types of sources, which have seen emission increases in the
## Baltimore City area from 1999 to 2008? Which have seen emission decreases in that area
## during the same time period?
# Use the same filtered data from Q2, but group by type and year 
data3 <- data2_a %>%
  group_by(type, year) %>%
  summarise(Emissions = sum(Emissions))
data3
# Plot the data which has been wrangled
q3 <- ggplot(data3, aes(year, Emissions))
q3 <- q3 + facet_grid(type~.)
q3 <- q3 + geom_point(aes(color=factor(year)), size = 3, alpha = 0.8) 
q3 <- q3 + geom_line()
q3 <- q3 + geom_smooth(method = "lm", se = FALSE, size = .7)
q3 <- q3 + xlab("Year") + labs(color = "Year")
q3 <- q3 + ggtitle("Total Emissions by Year in Baltimore, MD by Type")
# Print the plot
#png(file="Q3.png")
q3
#dev.off()
print("Q3: There has been decline in each of the types except for Point Emissions. Point emissions were at their lowest in 1999, and have only been greater than that since then.")

##  Compare emissions from onroad sources in Baltimore City with emissions from onroad
## sources in Los Angeles County, California (fips code b 06037b ). Which city has seen greater
## changes over time in onroad emissions?
# Filter for the respective fips codes
data4_a <- filter(NEI, fips=="06037")
data4_b <- filter(NEI, fips==24510)
# Create a single data set for the fips codes
data4_c <- bind_rows(data4_a, data4_b)
# Filter for only the ON-POINT type
data4_d <- filter(data4_c, type=="ON-ROAD")
data4_d
# Summarize the data
data4 <- data4_d %>%
  group_by(fips,type, year) %>%
  summarise(Emissions = sum(Emissions))
# Create labels 
fips_labels <- c('06037'='Los Angeles, CA', '24510'='Baltimore, MD')
# Plot the filtered data
q4 <- ggplot(data4, aes(year, Emissions))
q4 <- q4 + facet_grid(fips~., labeller=as_labeller(fips_labels))
q4 <- q4 + geom_point(aes(color=factor(year)), size = 3)
q4 <- q4 + geom_smooth(method = "lm", se = FALSE, size = .7)
q4 <- q4 + geom_line()
q4 <- q4 + xlab("Year") + labs(color = "Year")
q4 <- q4 + ggtitle("Total Emissions Comparison of Baltimore, MD and Los Angeles, CA")
# Print the plot
#png(file="Q4.png")
q4
#dev.off()
print("Q4: Emissions have actually gone up in Los Angeles, while emissions in Baltimore have been declining. However overall, Los Angeles has been polluting far greater amounts of PM25 than Baltimore has.")