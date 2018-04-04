install.packages("maptools")
install.packages("ggplot2")
install.packages("mapdata")
install.packages("ggthemes")

library(tidyverse)
library(ggplot2)
library(maptools)
library(mapdata)
library(ggthemes)
library(tibble)
library(viridis)
library(VIM)

#Sets the root folder for reading files
setwd("~/Documents/Marquette/Sophomore/Intro to Data Science/Pandas Project/")

#Read county demographic data
raw_data = read.csv("county_facts.csv")
pop_data = read.csv("county_pop_est.csv")

#Merging the two datasets using FIPS ID
  #Sets the column name to be correct
names(pop_data)[2] <- "fips"
combined_data <- merge(raw_data, pop_data, by="fips")

#Removes extraneous data (i.e. duplicate county names)
combined_data$GEO.id <- NULL
combined_data$GEO.display.label <- NULL

#Moves population columns to the front and removes extraneous variables
combined_data$rescen42010 <- NULL
combined_data$resbase42010 <- NULL
combined_data$respop72010 <- NULL
combined_data$respop72011 <- NULL
combined_data$respop72012 <- NULL
combined_data$respop72013 <- NULL
combined_data$respop72014 <- NULL
combined_data$respop72015 <- NULL
combined_data <- combined_data[, c(1:7, 55, 8:54)]

#Changes type of 2016 population estimate to integer (DOESN"T WORK PROPERLY)
combined_data$respop72016 <- as.integer(combined_data$respop72016)

#Renaming columns to be more readable
combined_data$PST045214 <- NULL
combined_data$PST040210 <- NULL
combined_data$PST120214 <- NULL
names(combined_data)[4] <- "census_count_2010"
names(combined_data)[5] <- "2016_pop_est"
combined_data$AGE135214 <- NULL
names(combined_data)[6] <- "%_under_18"
names(combined_data)[7] <- "%_over_65"
names(combined_data)[8] <- "%_female"
# Create Column for male percent, = 1-female
names(combined_data)[9] <- "%_white_w/hispanic"
names(combined_data)[10] <- "%_black"
names(combined_data)[11] <- "%_native"
names(combined_data)[12] <- "%_asian"
names(combined_data)[13] <- "%_polynesian"
names(combined_data)[14] <- "%_2+_races"
names(combined_data)[15] <- "%_hispanic"
names(combined_data)[16] <- "%_white_alone"
combined_data$POP715213 <- NULL
names(combined_data)[17] <- "%_foreign_born"
names(combined_data)[18] <- "%_non_english"
names(combined_data)[19] <- "%_high_school+"
names(combined_data)[20] <- "%_bachelor's+"
names(combined_data)[21] <- "%_veterans"
combined_data$LFE305213 <- NULL
combined_data$HSG010214 <- NULL
names(combined_data)[22] <- "%_homeowners"
combined_data$HSG096213 <- NULL
names(combined_data)[23] <- "median_home_value"
names(combined_data)[24] <- "num_households"
names(combined_data)[25] <- "persons_per_household"
names(combined_data)[26] <- "per_capita_income"
names(combined_data)[27] <- "median_household_income"
names(combined_data)[28] <- "%_in_poverty"
combined_data$BZA010213 <- NULL
combined_data$BZA110213 <- NULL
combined_data$BZA115213 <- NULL
combined_data$NES010213 <- NULL
combined_data$SBO001207 <- NULL
combined_data$SBO315207 <- NULL
combined_data$SBO115207 <- NULL
combined_data$SBO215207 <- NULL
combined_data$SBO515207 <- NULL
combined_data$SBO415207 <- NULL
combined_data$SBO015207 <- NULL
combined_data$MAN450207 <- NULL
combined_data$WTN220207 <- NULL
combined_data$RTN130207 <- NULL
combined_data$RTN131207 <- NULL
combined_data$AFN120207 <- NULL
combined_data$BPS030214 <- NULL
names(combined_data)[29] <- "land_area_2010"
combined_data$POP060210 <- NULL

combined_data[30] <- combined_data[5] / combined_data[29]
names(combined_data)[30] <- "pop_density"

#Converts integer percents to doubles < 1
combined_data[6:20] <- combined_data[6:20] / 100.0
combined_data[21] <- combined_data[21] / combined_data[5]
combined_data[22] <- combined_data[22] / 100.0
combined_data[28] <- combined_data[28] / 100.0

#Read Presidential Results
pres_data <- read.csv("2016_pres_results.csv")
#names(pres_data)[2] <- "pres_fips"
pres_data$fips <- NULL
pres_data$pct_report <- NULL
names(pres_data)[2] <- "state_abbreviation"

#Merge county demographics and presidential results
names(combined_data)[2] <- "county"
combined_data <- merge(combined_data, pres_data, by=c("county", "state_abbreviation"))

#Set FIPS as the index
row.names(combined_data) <- combined_data$fips

#Reorganize columns
  #Moves state.name next to state_abbreviation
  #Moves Donald.Trump, Hillary.Clinton, Gary.Johnson, Jill.Stein to be the first candidates listed
combined_data <- combined_data[, c(1:2, 33, 3:32, 40, 46, 44, 48,  34:39, 41:43, 45, 47, 49:65)]

#Combining non-major candidates
combined_data[is.na(combined_data)] <- 0
combined_data[38] <- combined_data[32] - combined_data[34] - combined_data[35] - combined_data[36] - combined_data[37]
names(combined_data)[38] <- "other"
combined_data[39:65] <- NULL

#Read County Age Demographics (Incredidbly large file)
#age_data <- read.csv("county_age_data.csv")
#age_data[9:80] <- NULL
#age_data[1:3] <- NULL

#age_data <- subset(age_data, YEAR == 9)
#age_data <- subset(age_data, AGEGRP != 0)
#age_data$YEAR = "2016"

#Mapping the pres results
county <- map_data("county")

gg <- ggplot()
gg <- gg + geom_map(data=county, map=county,
                    aes(long, lat, map_id=region),
                    color="#2b2b2b", fill=NA, size=0.15)
gg <- gg + geom_map(data=combined_data, map=county,
                    aes(fill = combined_data$Hillary.Clinton/combined_data$total_votes, map_id=fips),
                    color="white", fill=NA)
gg <- gg + coord_map("polyconic")
gg <- gg + theme_map()
gg <- gg + theme(plot.margin=margin(20,20,20,20))
gg

classcolors <- c('red', 'cyan' )
usmapDvR=map('county', fill= TRUE, col = classcolors[combined_data$lead], resolution = 0) 

combined_data[39] <- combined_data[34] / combined_data[32]
names(combined_data)[39] <- "trump_%"

combined_data[40] <- combined_data[20] - combined_data[21]
names(combined_data)[40] <- "%_high_school_only"
combined_data <- combined_data[, c(1:19, 40, 20:39)]

combined_data[41] <- 1 - combined_data[21]
names(combined_data)[41] <- "%_no_high_school"
combined_data <- combined_data[, c(1:19, 41, 20:40)]

marginplot(combined_data[, c("%_bachelor's+", "trump_%")], pch = 20)
marginplot(combined_data[, c("%_no_high_school", "trump_%")], pch = 20)
