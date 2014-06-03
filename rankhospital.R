rankhospital <- function(state, outcome, num = "best"){
    
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
    #saves data for the relevant outcome as numeric in numout
    numout<-suppressWarnings(as.numeric(data[[i]]))        
        
    if(num == "worst"){    
        #finds maximum 30-Day death rate for the relevent affliction amongst all hospitals in the state
        high<-max(numout,na.rm=TRUE)
        
        #finds all hospitals with the maximum rate, orders them alphabetically, and then returns the first hospital
        worstHos<-data$Hospital.Name[numout==high]
        worstHos<-worstHos[!is.na(worstHos)]
        sort(worstHos)[1]
        
    }else if (num == "best"){
        #finds minimum 30-Day death rate for the relevent affliction amongst all hospitals in the state
        low<-min(numout,na.rm=TRUE)
        
        #finds all hospitals with the minimum rate, orders them alphabetically, and then returns the first hospital
        bestHos<-data$Hospital.Name[numout==low]
        bestHos<-bestHos[!is.na(bestHos)]
        sort(bestHos)[1]
    
    }else{
        #finds the hospital with "num" lowest 30-Day death rate for the relevant afflication
        ranked<-data[["Hospital.Name"]][order(numout,data$Hospital.Name,na.last=NA)]
        if (num > length(ranked)){
            NA
        }else{
            ranked[num]
        }
    }
}