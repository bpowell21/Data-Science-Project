library(tidyverse)

#Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

county_id <- read.csv("county_id.csv")

pres_data_2016 <- read.csv("2016_pres_results.csv")
pres_data_2016$pct_report <- NULL
pres_data_2016$fips <- NULL
pres_data_2016$lead <- NULL
pres_data_2016$state.name <- NULL

pres_data_2016 <- pres_data_2016[, c(1:3, 10, 16, 5:9, 11:15, 17:35)]

pres_data_2016[is.na(pres_data_2016)] <- 0
pres_data_2016[6] <- pres_data_2016[3] - pres_data_2016[4] - pres_data_2016[5]
names(pres_data_2016)[6] <- "other"
pres_data_2016[7:35] <- NULL

names(pres_data_2016)[2] <- "state_abbreviation"

#Merge county demographics and presidential results
names(county_id)[2] <- "county"
pres_data_2016 <- merge(county_id, pres_data_2016, by=c("county", "state_abbreviation"))

pres_data_2016 <- pres_data_2016[, c(3, 1, 2, 4:8)]

pres_data_2016 <- melt(pres_data_2016, id = 1:5)
names(pres_data_2016)[6] <- "candidate"
names(pres_data_2016)[7] <- "votes"
pres_data_2016[8] <- pres_data_2016[7] / pres_data_2016[5]
names(pres_data_2016)[8] <- "share"

trump_subset <- subset(pres_data_2016, candidate == "Donald.Trump")
clinton_subset <- subset(pres_data_2016, candidate == "Hillary.Clinton")

#ct_subset <- subset(pres_data_2012, State.Postal == "CT")
pres_data_2012 <- read.csv("pres_results_08_16.csv")

names(pres_data_2012)[8] <- "Barack.Obama"
names(pres_data_2012)[9] <- "Mitt.Romney"
names(pres_data_2012)[1] <- "fips"
names(pres_data_2012)[7] <- "total_votes"
pres_data_2012[, c(2:6, 11:14)] <- NULL

pres_data_2012 <- merge(county_id, pres_data_2012, by="fips")
#pres_data_2012 <- pres_data_2012[c(52:4639),]

#Removes missing values
pres_data_2012 <- pres_data_2012[- c(1773, 2388),]

pres_data_2012 <- melt(pres_data_2012, id = 1:5)
names(pres_data_2012)[6] <- "candidate"
names(pres_data_2012)[7] <- "votes"
pres_data_2012[8] <- pres_data_2012[7] / pres_data_2012[5]
names(pres_data_2012)[8] <- "share"

romney_subset <- subset(pres_data_2012, candidate == "Mitt.Romney")
obama_subset <- subset(pres_data_2012, candidate == "Barack.Obama")

#Checks for missing values
#fips_2012 <- pres_data_2012[1]
#fips_2016 <- pres_data_2016[1]
#setdiff(fips_2012, fips_2016)

#Adds missing values
#pres_data_2016[3111,] <- c(46113, "Shannon County", "SD", "South Dakota", 2905, 241, 2510, 154)
#pres_data_2016[3112,] <- c(35013, "Dona Ana County", "NM", "New Mexico", 70648, 25374, 37947, 7327)

#Tries to convert state names to characters, doesn't work though
#pres_data_2016[2] <- as.character(pres_data_2016[2])

dem_2020 <- obama_subset[, c(1:4, 7)]
names(dem_2020)[5] <- "Barack.Obama"
dem_2020 <- merge(clinton_subset, dem_2020, by= c("fips", "county", "state_abbreviation", "state.name"))
names(dem_2020)[7] <- "Hillary.Clinton"
dem_2020[c(5, 6, 8)] <- NULL
