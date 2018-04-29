library(mice)
library(VIM)
library(ggplot2)
library(datasets)
#nstall.packages("sm")
library(sm)
library(tidyverse)
library(dplyr)
library(reshape2)



rata <- read.csv("county_facts.csv")


rat <-read.csv("Votes per state.csv")



highschool <-c(rata$EDU635213)
bachelord <-c(rata$EDU685213)
highschoold <- highschool-bachelord
state1<-rata$area_name
# state1[c("Alabama", 
#          Alaska, 
#          Arizona ,
#          Arkansas ,
#          California ,
#          Colorado ,
#          Connecticut, 
#          Delaware, 
#          Florida, 
#          Georgia ,
#          Hawaii ,
#          Idaho ,
#          Illinois, Indiana ,
#          Iowa ,
#          Kansas 
#          Kentucky 
#          Louisiana 
#          Maine 
#          Maryland 
#          Massachusetts 
#          Michigan 
#          Minnesota 
#          Mississippi 
#          Missouri 
#          Montana Nebraska 
#          Nevada 
#          New Hampshire 
#          New Jersey 
#          New Mexico 
#          New York 
#          North Carolina 
#          North Dakota 
#          Ohio 
#          Oklahoma 
#          Oregon 
#          Pennsylvania 
#          Rhode Island 
#          South Carolina 
#          South Dakota 
#          Tennessee 
#          Texas 
#          Utah 
#          Vermont 
#          Virginia 
#          Washington 
#          West Virginia 
#          Wisconsin 
#          Wyoming
# )]
education<- data.frame(state1, highschoold, bachelord)

state <- rat$ï..State.or..district
hillary <- rat$Hillary.Clinton..Democratic..
Do<- rat$Donald.Trump..Republican..
votes <- data.frame(state, hillary, Do)
vote2 <-votes[c(1:19,22:29,33:55),]

bar <- ggplot(data=dat, aes(x=state,y=bachelord))
bar + geom_bar(stat="identity") + geom_text(aes(label=ba), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()

bar <- ggplot(data=dat, aes(x=state,y=highschoold))
bar + geom_bar(stat="identity") + geom_text(aes(label=ba), vjust=1.6, color="Blue", size=3.5)+
  xlab("States") +  ylab("Percentage of individual with bachelor's degree or higher") + ggtitle("Bachelor's Degree Education of people who votes by States") + coord_flip()


total<-merge (education, vote2)
View(total)
