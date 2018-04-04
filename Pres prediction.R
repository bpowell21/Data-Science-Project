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
clinton_subset <- subset(pres_data_2016, candidate == "Hillary.Clinton")

#Include population data
pop_data = read.csv("county_pop_est.csv")

#Remove unwanted data
pop_data$GEO.id <- NULL
pop_data$GEO.display.label <- NULL
pop_data$rescen42010 <- NULL
pop_data$resbase42010 <- NULL
pop_data$respop72010 <- NULL
pop_data$respop72011 <- NULL
pop_data$respop72013 <- NULL
pop_data$respop72014 <- NULL
pop_data$respop72015 <- NULL

#Remove unwanted rows
pop_data <- pop_data[-c(1:54),]

#Convert to integer
pop_data$respop72016 <- as.numeric(levels(pop_data$respop72016))[pop_data$respop72016]
pop_data$respop72012 <- as.numeric(levels(pop_data$respop72012))[pop_data$respop72012]

#Organize Data
names(pop_data)[1] <- "fips"
names(pop_data)[2] <- "2012_pop_est"
names(pop_data)[3] <- "2016_pop_est"
pop_data <- merge(county_id, pop_data, by = "fips")
pop_data[7] <- (pop_data[6] / pop_data[5])
names(pop_data)[7] <- "pop_change_%"

#Merge with election data
dem_2020 <- clinton_subset[c(1, 7)]
names(dem_2020)[2] <- "Hillary.Clinton"
trump <- trump_subset[c(1, 7)]
dem_2020 <- merge(dem_2020, trump, by="fips")
names(dem_2020)[3] <- "Donald.Trump"

#Use the population to data to predict 2020 votes
dem_2020 <- merge(pop_data, dem_2020, by="fips")
dem_2020[10] <- dem_2020[7] * dem_2020[8]
dem_2020[10] <- round(dem_2020[10], digits = 0)
names(dem_2020)[10] <- "Dem_2020_predict"

dem_2020[11] <- dem_2020[7] * dem_2020[9]
dem_2020[11] <- round(dem_2020[11], digits = 0)
names(dem_2020)[11] <- "GOP_2020_predict"

az_subset <- subset(dem_2020, state.name == "Arizona")
dem <- sum(az_subset$Dem_2020_predict)
gop <- sum(az_subset$GOP_2020_predict)
share_dem <- dem / (dem+gop)
share_gop <- gop / (dem+gop)
marg <- share_dem - share_gop

ga_subset <- subset(dem_2020, state.name == "Florida")
dem <- sum(ga_subset$Dem_2020_predict)
gop <- sum(ga_subset$GOP_2020_predict)
share_dem <- dem / (dem+gop)
share_gop <- gop / (dem+gop)
marg <- share_dem - share_gop
