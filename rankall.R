setwd("C:/Users/fruit/OneDrive/Documents/R/Coursera/RProgramming_Assignment3")
## getwd();

##############R Programming Week 4 Assignment 3 #################################
############################## Q4 ###############################################

data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
data[,11] <- as.numeric(data[,11])
data[,17] <- as.numeric(data[,17])
data[,23] <- as.numeric(data[,23])
state_list <- unique(data$State)

rankall <- function(outcome, num = "best") {
  
  if (outcome == "heart attack") {o_index <- 11}
  else if (outcome == "heart failure") {o_index <- 17}
  else if (outcome == "pneumonia") {o_index <- 23}
  else {stop("invalid outcome")}
  
  count_state <- data.frame(table(data[!is.na(data[,o_index]),]$State))  
  if (num == "best") {num_state <- data.frame("State" = count_state[1], "num" = 1)}
  else if (num == "worst") {num_state <- count_state}
  else {num_state <-  data.frame("State" = count_state[1], "num" = as.numeric(num))}
  names(num_state) <- c("State","rank")
  
  target_data <- data[!is.na(data[,o_index]),][,c(2,7,o_index)]
  target_data <- target_data[order(target_data[,2],target_data[,3],target_data[,1]),]
  target_data$rank <- unlist(lapply(split(target_data[3],target_data$State),function(x) rank(unlist(x), ties.method = "first")))
  result <- merge(x = num_state, y = target_data, by = c("State", "rank"), all.x = TRUE)      
  r <- result[,c(3,1)]
  names(r) <- c("hospital","state")
  r
}  

