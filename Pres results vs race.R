install.packages("tidyverse")
install.packages("reshape2")

library(tidyverse)
library(reshape2)
library(VIM)
library(dplyr)
library(ggplot2)

# Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

# Get the county info
county_id <- read.csv("county_id.csv")

# Read County Age Demographics
age_data <- read.csv("county_age_data.csv")

# Uses only data from 2016 and all age groups
age_data <- subset(age_data, YEAR == 9)
age_data <- subset(age_data, AGEGRP == 0)

# Keeps only count of white people
age_data <- age_data[, c(4, 5, 8, 11, 12)]

# Converts count into percentage
age_data[3] <- (age_data[4] + age_data[5])/age_data[3]

# Removes Count Data
age_data[4:5] <- NULL

# Renames column header, useful for merging data
names(age_data)[1] <- "state_name"
names(age_data)[2] <- "county"
names(age_data)[3] <- "%_white_alone"

# Merges data with County IDs, gets FIPS code
white_data <- merge(county_id, age_data, by= c("county", "state_name"))

# Reads 2016 Presidential Data
pres_data_2016 <- read.csv("2016_pres_results.csv")

# Removes unnecessary columns
pres_data_2016$pct_report <- NULL
#pres_data_2016$fips <- NULL
pres_data_2016$lead <- NULL
pres_data_2016$state.name <- NULL
pres_data_2016$st <- NULL
pres_data_2016$county <- NULL

# Converts non-major party candidates into one category, other
pres_data_2016[is.na(pres_data_2016)] <- 0
pres_data_2016[3] <- pres_data_2016[2] - pres_data_2016[9] - pres_data_2016[15]
names(pres_data_2016)[3] <- "other"

# Removes unnecessary columns
pres_data_2016[c(4:8, 10:14, 16:36)] <- NULL

# Merge county demographics and presidential results
names(county_id)[2] <- "county"
pres_data_2016 <- merge(county_id, pres_data_2016, by="fips")

# Rearranges columns
pres_data_2016 <- pres_data_2016[, c(1:5, 8, 7, 6)]

# Melts the candidate columns into one column
pres_data_2016 <- melt(pres_data_2016, id = 1:5)
names(pres_data_2016)[6] <- "candidate"
names(pres_data_2016)[7] <- "votes"
pres_data_2016[8] <- pres_data_2016[7] / pres_data_2016[5]
names(pres_data_2016)[8] <- "share"

# Gets individual candidate data
trump_subset <- subset(pres_data_2016, candidate == "Donald.Trump")

# Merges white % data with pres data
new_data <- merge(trump_subset, white_data, by="fips")

# Plots the relationship between white % and Trump %
marginplot(new_data[, c("%_white_alone", "share")], pch = 20)

# Gets data on county demographics
county_facts <- read.csv("county_facts.csv")

# Use only FIPS and education columns
county_facts <- county_facts[, c(1, 23, 24)]

# Rename column headers
names(county_facts)[2] <- "%_high_school+"
names(county_facts)[3] <- "%_bachelors+"

# Convert to decimals
county_facts[2] <- county_facts[2] / 100
county_facts[3] <- county_facts[3] / 100

# Calculate new data
county_facts[4] <- 1-county_facts[3]
names(county_facts)[4] <- "%_no_bachelors"

# Prepares data for merging
to_merge_data <- county_facts[, c(1, 4)]
new_data <- merge(new_data, to_merge_data, by="fips")

# Gets county income data
county_income <- read.csv("county_income.csv")

# Prepares data for merging
county_income <- county_income[,c(1, 47)]
names(county_income)[1] <- "fips"
names(county_income)[2] <- "median_income"
new_data <- merge(new_data, county_income, by="fips")

# Writes the new data file so can be used in python
write.csv(new_data, file = "pres_prediction_data.csv")

marginplot(new_data[, c("%_white_alone", "share")], pch = 20)


