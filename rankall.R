rankall <- function(outcome, num = "best"){
 
    data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
    #Checks that outcome is valid
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
    
    #internal function that finds the "num" ranked hospital per state for outcome
    rankhosState <- function(state, outcome, num = "best"){
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
            ranked<-data[["Hospital.Name"]][order(numout,data$Hospital.Name,na.last=NA)]
            if (num > length(ranked)){
                NA
            }else{
                ranked[num]
            }
        }
    }
    
    #creates a vector containing all the states in alphabetical order
    states<-sort(unique(data$State))
    
    #creates a vector containing the name of the "num" ranked hospital 
    hos<-sapply(states,function(elt) rankhosState(elt,outcome,num))
    
    output <- data.frame (hospital=hos,state=states)
    output  
}