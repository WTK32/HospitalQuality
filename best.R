setwd("C:/Users/fruit/OneDrive/Documents/R/Coursera/RProgramming_Assignment3")
## getwd();

##############R Programming Week 4 Assignment 3 #################################
############################## Q2 ###############################################

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[,11] <- as.numeric(data[,11])
data[,17] <- as.numeric(data[,17])
data[,23] <- as.numeric(data[,23])
state_list <- unique(data$State)

best <- function(state, outcome) {
  if (outcome == "heart attack") {num <- 11}
  else if (outcome == "heart failure") {num <- 17}
  else if (outcome == "pneumonia") {num <- 23}
  else {stop("invalid outcome")}
  
  if (is.na(match(state,state_list))) {stop("invalid state")}
  
  min_rate <- min(data[data$State==state,][,num], na.rm = T)
  
  hospital_name_list <- data[data$State == state & data[,num]==min_rate & !is.na(data[,num]),]$Hospital.Name
  sort(hospital_name_list)[1]
  
}
