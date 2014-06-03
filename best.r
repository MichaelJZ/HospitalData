best <- function(state,outcome){
    #Finds hospital with the lowest 30-Day Death Rate for the affliction specified by "outcome"
    #in the US state specificed by "state."
    #If multiple hospitals in the state are tied in lowest death rates, 
    #the hospitals are sorted alphabetically and first hospital is returned.
    
    #Read outcome data
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    
    #Checks that state and outcome are valid
    if (!state %in% unique(data$State)){
        stop("invalid state")
    }
    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    
    #sets i to the index of the column pertaining to the 30-Day death rate of the specified affliction
    if (outcome == "heart attack"){
        i<-11
    }else if (outcome == "heart failure"){
        i<-17
    }else if (outcome == "pneumonia"){
        i<-23
    }
    
    #only saves data for the relevant state
    data<-split(data,data$State)
    data<-data[[state]]
    
    #finds minimum 30-Day death rate for the relevent affliction amongst all hospitals in the state
    numout<-suppressWarnings(as.numeric(data[[i]])) 
    low<-min(numout,na.rm=TRUE)
    
    #finds all hospitals with the minimum rate, orders them alphabetically, and then returns the first hospital
    bestHos<-data$Hospital.Name[numout==low]
    bestHos<-bestHos[!is.na(bestHos)]
    sort(bestHos)[1]
}
