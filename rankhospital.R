setwd("C:/Users/fruit/OneDrive/Documents/R/Coursera/RProgramming_Assignment3")
## getwd();

##############R Programming Week 4 Assignment 3 #################################
############################## Q3 ###############################################

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[,11] <- as.numeric(data[,11])
data[,17] <- as.numeric(data[,17])
data[,23] <- as.numeric(data[,23])
state_list <- unique(data$State)

rankhospital <- function(state, outcome, num = "best") {
  hospital_name <- NA
  if (outcome == "heart attack") {o_index <- 11}
  else if (outcome == "heart failure") {o_index <- 17}
  else if (outcome == "pneumonia") {o_index <- 23}
  else {stop("invalid outcome")}
  
  if (is.na(match(state,state_list))) {stop("invalid state")}
  
  count_hospital <- NROW(na.omit(data[data$State==state,][,o_index]))  
  if (num == "best") {num <- 1}
  else if (num == "worst") {num <- count_hospital}
  else {num <- as.numeric(num)}
  
  if (num > count_hospital) {hospital_name}
  else {
    target_data <- data[data$State == state & !is.na(data[,o_index]),][,c(2,o_index)]
    rank <- target_data[order(target_data[,2],target_data[,1]),]
    rank$Hospital.Name[num]      
  }
}  
