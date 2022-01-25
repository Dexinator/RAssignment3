##Cleaned test and experimental variables. Function best inputs State Abrv. and a disease,
##it will return the name of the hospital with the lowest given disease mortality rate
##in that state. It will return error mssgs if inputs are not well written as requested.
file2 <- read.csv("outcome-of-care-measures.csv")
column<-NULL
DIS_HA<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
DIS_HF<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
DIS_PN<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
outcome<-"heart failure"
state<-"TX"
if (outcome=="heart attack"){column<-DIS_HA}else if(outcome=="heart failure"){column<-DIS_HF}else if(outcome=="pneumonia"){column<-DIS_PN}
reduced_data<-subset(file2, State==state, select=c("Hospital.Name",column))
reduced_data[order(as.numeric(reduced_data[,2])),]
reduced_data[order(reduced_data[,2]),]
as.numeric(reduced_data[,2])
reduced_data2<-reduced_data
reduced_data2[,2]<-as.numeric(reduced_data2[,2])
reduced_data2[order(reduced_data2[,2],na.last = NA),]



?order
Hospital_RATE<-reduced_data[which(reduced_data[,2] == min(reduced_data[,2], na.rm = TRUE)),]
Hospital_NAME<-Hospital_RATE[,1]
if (length(Hospital_NAME)>1){
  Hospitals<-order(Hospital_NAME)
  Hospitals[1]
}else{
  Hospital_NAME}


rankhospital <- function(state, outcome) {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv")
  ## Check that state and outcome are valid
  
  if (isTRUE(outcome!="heart attack" & outcome!="heart failure"& outcome!="pneumonia")&isTRUE(any(file$State==state)==FALSE)){
  return ("invalid state and invalid outcome")  
  }
  
  if (any(file$State==state)==FALSE){
    return ("invalid state")
  }
  if (outcome!="heart attack" & outcome!="heart failure"& outcome!="pneumonia"){
    return("invalid outcome")
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
    Hospitals<-order(Hospital_NAME)
    Hospitals[1]
  }else{
    Hospital_NAME}
}
best("TX","heart attack")
