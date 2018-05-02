install.packages("reshape")

library(tidyverse)
library(VIM)
library(lattice)
library(reshape2)
library(reshape)

#Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

county_id <- read.csv("county_id.csv")

################## 2016 DATA #####################

# Read 2016 Pres Data
pres_data_2016 <- read.csv("2016_pres_results.csv")

# Removes unnecessary columns
pres_data_2016$pct_report <- NULL
#pres_data_2016$fips <- NULL
pres_data_2016$lead <- NULL
pres_data_2016$state.name <- NULL
pres_data_2016$st <- NULL
pres_data_2016$county <- NULL

names(pres_data_2016)[2] <- "total_votes_2016"

# Converts non-major party candidates into one category, other
pres_data_2016[is.na(pres_data_2016)] <- 0
pres_data_2016[3] <- pres_data_2016[2] - pres_data_2016[9] - pres_data_2016[15]
names(pres_data_2016)[3] <- "other"

# Removes unnecessary columns
pres_data_2016[c(4:8, 10:14, 16:36)] <- NULL

pres_data_2016 <- merge(county_id, pres_data_2016, by="fips")

# Calculate's National Margin
margin_2016 <- (sum(pres_data_2016[8])-sum(pres_data_2016[7]))/sum(pres_data_2016[5])

# Calculate County Margin
pres_data_2016[9] <- (pres_data_2016[8]-pres_data_2016[7])/pres_data_2016[5]
names(pres_data_2016)[9] <- "margin_2016"

# Calculate County Lean
pres_data_2016[10] <- pres_data_2016[9] - margin_2016
names(pres_data_2016)[10] <- "partisan_lean_2016"

################## 2012 DATA #####################

# Read 2012 Pres Data
pres_data_2012 <- read.csv("pres_results_08_16.csv")

# Cleans Data
names(pres_data_2012)[8] <- "Barack.Obama"
names(pres_data_2012)[9] <- "Mitt.Romney"
names(pres_data_2012)[1] <- "fips"
names(pres_data_2012)[7] <- "total_votes_2012"
pres_data_2012[, c(2:6, 11:14)] <- NULL

pres_data_2012 <- merge(county_id, pres_data_2012, by="fips")
#pres_data_2012 <- pres_data_2012[c(52:4639),]

#Removes missing values
pres_data_2012 <- pres_data_2012[- c(1773, 2388),]

# Calculate's National Margin
margin_2012 <- (sum(pres_data_2012[6])-sum(pres_data_2012[7]))/sum(pres_data_2012[5])

# Calculate County Margin
pres_data_2012[9] <- (pres_data_2012[6]-pres_data_2012[7])/pres_data_2012[5]
names(pres_data_2012)[9] <- "margin_2012"

# Calculate County Lean
pres_data_2012[10] <- pres_data_2012[9] - margin_2012
names(pres_data_2012)[10] <- "partisan_lean_2012"

################## POPULATION DATA #####################

# Read Population Data
detailed_pop_data <- read.csv("county_age_data.csv")

total_pop <- detailed_pop_data[, c(1:8)]

VAP_data <- subset(total_pop, AGEGRP %in% c(5:18))

# 2016 VAP Cleaning
VAP_data_2016 <- subset(VAP_data, YEAR == 9)
VAP_data_2016 <- VAP_data_2016[, c(4, 5, 7, 8)]
VAP_data_2016 <- cast(VAP_data_2016, STNAME+CTYNAME~AGEGRP,sum)
VAP_data_2016[3] <- VAP_data_2016[3] + VAP_data_2016[4] + VAP_data_2016[5] + VAP_data_2016[6] + VAP_data_2016[7] + VAP_data_2016[8] + VAP_data_2016[9] + VAP_data_2016[10] + VAP_data_2016[11] + VAP_data_2016[12] + VAP_data_2016[13] + VAP_data_2016[14] + VAP_data_2016[15] + VAP_data_2016[16]
names(VAP_data_2016)[1] <- "state.name"
names(VAP_data_2016)[2] <- "area_name"
names(VAP_data_2016)[3] <- "VAP_2016"
VAP_data_2016 <- VAP_data_2016[, c(1:3)]

VAP_data_2016 <- merge(county_id, VAP_data_2016, by=c("area_name", "state.name"))
VAP_data_2016 <- VAP_data_2016[, c(3, 5)]

pres_data_2016 <- merge(pres_data_2016, VAP_data_2016, by = "fips")
pres_data_2016[12] <- pres_data_2016[5] / pres_data_2016[11]
names(pres_data_2016)[12] <- "VAP_turnout_2016"

pres_data_2016 <- pres_data_2016[, c(1:5, 10, 9)]

# 2012 VAP Cleaning
VAP_data_2012 <- subset(VAP_data, YEAR == 5)
VAP_data_2012 <- VAP_data_2012[, c(4, 5, 7, 8)]
VAP_data_2012 <- cast(VAP_data_2012, STNAME+CTYNAME~AGEGRP,sum)
VAP_data_2012[3] <- VAP_data_2012[3] + VAP_data_2012[4] + VAP_data_2012[5] + VAP_data_2012[6] + VAP_data_2012[7] + VAP_data_2012[8] + VAP_data_2012[9] + VAP_data_2012[10] + VAP_data_2012[11] + VAP_data_2012[12] + VAP_data_2012[13] + VAP_data_2012[14] + VAP_data_2012[15] + VAP_data_2012[16]
names(VAP_data_2012)[1] <- "state.name"
names(VAP_data_2012)[2] <- "area_name"
names(VAP_data_2012)[3] <- "VAP_2012"
VAP_data_2012 <- VAP_data_2012[, c(1:3)]

VAP_data_2012 <- merge(county_id, VAP_data_2012, by=c("area_name", "state.name"))
VAP_data_2012 <- VAP_data_2012[, c(3, 5)]

pres_data_2012 <- merge(pres_data_2012, VAP_data_2012, by = "fips")
pres_data_2012[5] <- pres_data_2012[5] / pres_data_2012[11]
names(pres_data_2012)[5] <- "VAP_turnout_2012"

pres_data_2012 <- pres_data_2012[, c(1:5, 10, 9)]

pop_data <- read.csv("county_pop_est.csv")

# Clean Data
pop_data <- pop_data[, c(2, 8, 12)]
pop_data <- pop_data[- c(1:54), ]

# Rename Columns
names(pop_data)[1] <- "fips"
names(pop_data)[2] <- "pop_2012"
names(pop_data)[3] <- "pop_2016"

################## COMBINED DATA #####################

essential_2016 <- pres_data_2016[, c(1, 5:7)]
essential_2012 <- pres_data_2012[, c(1, 5:7)]

combined_data <- merge(essential_2012, essential_2016, by = "fips")
combined_data <- merge(county_id, combined_data, by = "fips")

combined_data[11] <- combined_data[9] - combined_data[6]
names(combined_data)[11] <- "partisan_shift"

################## ANALYSIS #####################
pres_data_2012[8] <- abs(pres_data_2012[6])
names(pres_data_2012)[8] <- "abs_partisan_lean_2012"

pres_data_2016[8] <- abs(pres_data_2016[6])
names(pres_data_2016)[8] <- "abs_partisan_lean_2016"

# Writes the new data file so can be used in python
write.csv(casting_data, file = "VAP_data_cast.csv")

#xyplot(partisan_lean_2012 ~ VAP_turnout_2012, combined_data)

######################### Create Trend Lines for Age Distribution ######################
casting_data <- subset(VAP_data, YEAR == 9)
casting_data <- casting_data[, c(4, 5, 7, 8)]
names(casting_data)[1] <- "state.name"
names(casting_data)[2] <- "area_name"
casting_data <- cast(casting_data, state.name+area_name~AGEGRP,sum)

casting_data[17] <- casting_data[3] + casting_data[4] + casting_data[5] + casting_data[6] + casting_data[7] + casting_data[8] + casting_data[9] + casting_data[10] + casting_data[11] + casting_data[12] + casting_data[13] + casting_data[14] + casting_data[15] + casting_data[16]

casting_data[3] <- casting_data[3] / casting_data[17]
casting_data[4] <- casting_data[4] / casting_data[17]
casting_data[5] <- casting_data[5] / casting_data[17]
casting_data[6] <- casting_data[6] / casting_data[17]
casting_data[7] <- casting_data[7] / casting_data[17]
casting_data[8] <- casting_data[8] / casting_data[17]
casting_data[9] <- casting_data[9] / casting_data[17]
casting_data[10] <- casting_data[10] / casting_data[17]
casting_data[11] <- casting_data[11] / casting_data[17]
casting_data[12] <- casting_data[12] / casting_data[17]
casting_data[13] <- casting_data[13] / casting_data[17]
casting_data[14] <- casting_data[14] / casting_data[17]
casting_data[15] <- casting_data[15] / casting_data[17]
casting_data[16] <- casting_data[16] / casting_data[17]

casting_data <- merge(county_id, casting_data, by=c("area_name", "state.name"))
merge_2016 <- pres_data_2016[, c(1, 9, 12)]
casting_data <- merge(casting_data, merge_2016, by = "fips")
names(casting_data)[19] <- "total"

write.csv(casting_data, file = "VAP_data_cast.csv")