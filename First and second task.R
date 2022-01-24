outcome <- read.csv("outcome-of-care-measures.csv")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
with(outcome, State=="AL")
outcome[,outcome$State["AL"]]
test<-outcome[,outcome$State["AL"]]

which(colnames(outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") 
which(colnames(outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") 
which(colnames(outcome) == "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") 

outcome_w_state<-outcome[outcome$State=='AL',]
mortality<-outcome_w_state[,11]

outcome_w_state[,11]
mortality
min(mortality,na.rm = TRUE)


best <- function(state, outcome) {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  column<-NULL
  if (outcome=="heart attack"){column<-11}else if(outcome=="heart failure"){column<-17}else if(outcome=="pneumonia"){column<-23}
  file_w_state<-file[file$State==state,]
  mortality<-file_w_state[,column]
  lowest_rate<-min(mortality,na.rm = TRUE)
}
best("TX","heart attack")
