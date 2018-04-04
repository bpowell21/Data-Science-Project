library(tidyverse)

#Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

county_id <- read.csv("county_id.csv")

pres_data_2016 <- read.csv("2016_pres_results.csv")
pres_data_2016$pct_report <- NULL
#pres_data_2016$fips <- NULL
pres_data_2016$lead <- NULL
pres_data_2016$state.name <- NULL
pres_data_2016$st <- NULL
pres_data_2016$county <- NULL

pres_data_2016[is.na(pres_data_2016)] <- 0
pres_data_2016[3] <- pres_data_2016[2] - pres_data_2016[9] - pres_data_2016[15]
names(pres_data_2016)[3] <- "other"
pres_data_2016[c(4:8, 10:14, 16:36)] <- NULL

#Merge county demographics and presidential results
names(county_id)[2] <- "county"
pres_data_2016 <- merge(county_id, pres_data_2016, by="fips")

pres_data_2016 <- pres_data_2016[, c(1:5, 8, 7, 6)]

pres_data_2016 <- melt(pres_data_2016, id = 1:5)
names(pres_data_2016)[6] <- "candidate"
names(pres_data_2016)[7] <- "votes"
pres_data_2016[8] <- pres_data_2016[7] / pres_data_2016[5]
names(pres_data_2016)[8] <- "share"

#Get individual candidate data
trump_subset <- subset(pres_data_2016, candidate == "Donald.Trump")

#Read income data
county_income_data <- read.csv("county_income.csv")
county_income_data[c(2, 3, 4:46, 48)] <- NULL
names(county_income_data)[1] <- "fips"
names(county_income_data)[2] <- "median_income"
county_income_data$median_income <- as.numeric(levels(county_income_data$median_income))[county_income_data$median_income]
pres_data_2016 <- merge(pres_data_2016, county_income_data, by = "fips")

clinton_subset <- subset(pres_data_2016, candidate == "Hillary.Clinton")
marginplot(clinton_subset[, c("median_income", "share")], pch = 20)