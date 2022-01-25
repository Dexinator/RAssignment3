## Function rankhospital inputs State Abrv., a disease and the rank of hospital,
##it will return the name of the hospital with the given rank using disease mortality rate
##in that state. It will return error mssgs if inputs are not well written as requested.
rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  column<-NULL
  DIS_HA<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  DIS_HF<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  DIS_PN<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  if (outcome=="heart attack"){column<-DIS_HA}else if(outcome=="heart failure"){column<-DIS_HF}else if(outcome=="pneumonia"){column<-DIS_PN}
  reduced_data<-subset(file, State==state, select=c("Hospital.Name",column))
  reduced_data[,2]<-as.numeric(reduced_data[,2])
  ord_red_data<-reduced_data[order(reduced_data[,2],na.last = NA,reduced_data[,1]),]
  if (num=="best"){
    ord_red_data[1,1]
  }else if (num=="worst"){
    tail(ord_red_data[,1],n=1)
  }else if (num>0 & num<length(reduced_data[,1])){
  ord_red_data[num,1]
  }
  else {
    "NA"
  }
  
}
rankhospital("TX","heart failure",2000)
rankhospital("TX", "heart failure", 4)
