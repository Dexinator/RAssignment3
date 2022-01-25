outcome <- read.csv("outcome-of-care-measures.csv")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


##Function best inputs State Abrv. and a disease,
##it will return the name of the hospital with the lowest given disease mortality rate
##in that state. It will return error mssgs if inputs are not well written as requested.
best <- function(state, outcome) {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  
  if (isTRUE(outcome!="heart attack" & outcome!="heart failure"& outcome!="pneumonia")&isTRUE(any(file$State==state)==FALSE)){
    stop ("invalid state and invalid outcome")  
  }
  
  if (any(file$State==state)==FALSE){
    stop ("invalid state")
  }
  if (outcome!="heart attack" & outcome!="heart failure"& outcome!="pneumonia"){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  column<-NULL
  DIS_HA<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  DIS_HF<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  DIS_PN<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (outcome=="heart attack"){column<-DIS_HA}else if(outcome=="heart failure"){column<-DIS_HF}else if(outcome=="pneumonia"){column<-DIS_PN}
  reduced_data<-subset(file, State==state, select=c("Hospital.Name",column))
  Hospital_RATE<-reduced_data[which(reduced_data[,2] == min(reduced_data[,2], na.rm = TRUE)),]
  Hospital_NAME<-Hospital_RATE[,1]
  if (length(Hospital_NAME)>1){
    Hospitals<-Hospital_NAME[order(Hospital_NAME)]
    Hospitals[1]
  }
  else{
    Hospital_NAME}
}
best("WA","pneumonia")

