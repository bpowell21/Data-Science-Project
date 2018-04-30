#Load library
library(mice)
library(VIM)
library(ggplot2)
library(datasets)
#nstall.packages("sm")
library(sm)
library(tidyverse)
library(dplyr)
library(reshape2)
install.packages("xlsx")
library("xlsx")


# load data set
rata <- read.csv("county_facts.csv")

rat <-read.csv("Votes per state.csv")

#Get data set to where the states are only in set
rata1 <- rata[rata$state_abbreviation == "",]
rat1 <- rat[rat$Electoral..method =="WTA",]

#Make a new data set about education by state
Highschool <-c(rata1$EDU635213)
Bachelord <-c(rata1$EDU685213)
Highschoold <- Highschool-Bachelord
State1<-rata1$area_name
education<- data.frame(State1, Highschoold, Bachelord)
education1<-education[c(3:52),]
education1$ID <- 1:nrow(education1)

#Make a new data set about vote by state
state4 <- rat1$ï..State.or..district
Hillary <- rat1$Hillary.Clinton..Democratic..
Donald<- rat1$Donald.Trump..Republican..
votes <- data.frame(state4, Hillary, Donald)
votes$ID <- 1:nrow(votes)

#Bar graph
Viewbar <- ggplot(data=total, aes(x=State1,y=Bachelord))
bar + geom_bar(stat="identity") + geom_text(aes(label=ba), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()

bar <- ggplot(data=total, aes(x=State1,y=Highschoold))
bar + geom_bar(stat="identity") + geom_text(aes(label=ba), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()

# Merge data
total<-merge (education1, votes, b.x = "ID")
View(total)
total$state4 <- NULL
total$ID <- NULL
View(total)

#Bar graph
bar1 <- ggplot(data=total, aes(x=State1,y=Bachelord))
bar + geom_bar(stat="identity") + geom_text(aes(label=Bachelord), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()

bar2 <- ggplot(data=total, aes(x=State1,y=Highschoold))
bar + geom_bar(stat="identity") + geom_text(aes(label=Highschoold), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()






write.xlsx(x = total, file = "Education.xlsx", sheetName = "TestSheet", row.names = FALSE)
write.csv(total, file = "Education.csv")
