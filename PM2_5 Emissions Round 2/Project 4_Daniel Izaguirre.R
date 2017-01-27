## Daniel Izaguirre
## Dr. Spence
## Selected Topics: Data Science
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

## Set reference for use in Q2
REF <- readRDS("Source_Classification_Code.rds")
#r <- head(REF)
#View(REF)
#tbl_df(r)
## Set NEI data
NEI <- readRDS("summarySCC_PM25.rds")
#h <- head(NEI)
#View(h)
#tbl_df(h)

## Q1: A data frame containing only rows for these 3 California counties: Los Angeles County
## (fips code "06037"), Orange County ("06059"), and San Bernadino County ("06071"). The
## only columns in the data frame should be fips, SCC, and Emissions.
# Filter by respective fips code, this will make adding county column in q2
data1_a <- filter(NEI, fips=="06037")
data1_b <- filter(NEI, fips=="06059")
data1_c <- filter(NEI, fips=="06071")
# Combine respective data frames
data1_d <- do.call("rbind", list(data1_a,data1_b,data1_c))
# Select only the required variables. Variables: fips, Solvent, Emissions.
data1 <- data1_d %>%
  select(fips, SCC, Emissions)
# View the data frame
View(data1)

## Q2: From the data frame constructed in step 1, add two additional columns: One called 'County'
## with the name of the county (based on fips code), and one called 'Solvent'. The value of
## 'Solvent' should be YES for any pollutant source that involves solvents, and NO otherwise.
# Mutate each fips for their respective county and bind them to create a single data frame
mut1_a <- mutate(data1_a, County = 'Los Angeles')
mut1_b <- mutate(data1_b, County = 'Orange County')
mut1_c <- mutate(data1_c, County = 'San Bernadino')
# Combine the respective data frames
data2_a <- do.call("rbind", list(mut1_a,mut1_b,mut1_c))
#View(data2_b)
# Assign yes and no values to the Source Reference File and create a 2 variable data frame (Variables: SCC and Solvent)
# Search for 'solvent' in EI.Sector. EI.Sector is chosen because it provides  
sol <- REF %>% 
  select(SCC, EI.Sector) %>% 
  mutate(Solvent = ifelse(grepl('Solvent', REF$EI.Sector, ignore.case=TRUE), "YES", "NO")) %>% 
  select(-EI.Sector)
#View(sol)
# Create a joined data frame of the Mutated data frame and the solvent data frame by left join
data2_b <- data2_a %>% 
  left_join(sol, by ='SCC')
# Create a data frame that has only the required variables. Variables fips, SCC, Emissions, County, Solvent.
data2 <-data2_b %>% 
  select(-Pollutant, -type, -year)
# View the data frame
View(data2)

## Q3: From the data frame constructed in step 2, create a data frame with all years except 1999,
## and with only the columns County, year, Emissions, and Solvent.
# Step 1: group data by year. 
# Step 2: Filter for years greater than 1999.
# Step 3: Remove the unwanted columns, and have a data frame with pollutant to accomodate for question 6.
data3 <- data2_b %>% 
#  group_by(year) %>% 
  filter(year > 1999) %>% 
  select(-fips,-SCC,-Pollutant) %>% 
  select(County, year, Emissions, Solvent)
# View the data table
View(data3)

## Q4: Using your results from step 3, create a data frame that shows the total amount of
## Emissions for each county, by year, for each type of pollutant (solvent and non-solvent).
# Step 1: Group by the variables that will be summarised; Grouping: County, Year, Solvent.
# Step 2: Summarize the data by the respective emissions.
data4 <- data3 %>% 
  group_by(County, year, Solvent) %>% 
  summarise(Emissions = sum(Emissions))
# View the data frame
View(data4)

## Q5: Again using your results from step 3, create a data frame that shows the total amount of
## Emissions for each county and each pollutant type (for 2002, 2005, and 2008 combined).
# Step 1: Group by the variables that will be summarised; Grouping: County, Solvent.
# Step 2: Summarise the data by the respective emissions.  
data5 <- data3 %>% 
  group_by(County, Solvent) %>% 
  summarise(Emissions = sum(Emissions))
# View the data frame
View(data5)

## Q6: Again with results from step 3, create a data frame that shows the total 
## amount of solvent related Emissions by year for each pollutant 
## type (for the 3 counties combined).
# Step 1: Group by the variables that will be summarised; Grouping: year, Solvent
# Step 2: Summarise the data by the respective emissions.
data6 <- data3 %>% 
  group_by(year, Solvent) %>%
  summarise(Emissions = sum(Emissions))
# View the data frame
View(data6)


## Q7: Create a data frame with one row for each county showing the total amount 
## of Emissions for that county in 2008 only.
# Step 1: Group the data from question 2 by year.
# Step 2: Filter out anything that is not from the year 2008.
# Step 3: Regroup the data by county
# Step 4: Summarise the county data by Emissions
data7 <- data2_b %>% 
  group_by(County, year) %>% 
  filter(year == 2008) %>% 
  summarise('2008 Emissions' = sum(Emissions))
# View the data frame
View(data7)

## Q8: Create a data frame with a single row showing the total amount of Emissions in all three
## counties combined, for all three years combined.
# Step 1: Summarise the data by Emissions.
data8 <- data3 %>% 
  summarise(Emissions = sum(Emissions))
# View the data frame
View(data8)


