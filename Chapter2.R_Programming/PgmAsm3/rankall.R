rankall <- function(outcome, num = "best") {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # Read the source file, each column as character
        
        stateVect <- unique(data[, "State"])
        # Find the unique state names and store to a vector
        
        outcomeVect <- c("heart attack", "heart failure", "pneumonia")
        # The vector of 3 different outcomes
        
        if(is.na(match(outcome, outcomeVect))) stop("invalid outcome")
        # check whether the outcome is valid
        
        hospitalColId <- 2
        # The hospital name is at column 2
        
        stateId <- 7
        # The state is at column 7
        
        if(outcome == "heart attack") outcomeColId <- 11
        else if(outcome == "heart failure") outcomeColId <- 17
        else if(outcome == "pneumonia") outcomeColId <- 23
        # Assign the column number to 'outcomeColId' based on the 'outcome' value
        
        hospitalDF <- data[c(hospitalColId, stateId, outcomeColId)]
        # Create a 'hospitalDF' data frame which contais only the hospital name, state and death rate
        
        names(hospitalDF) <- c("hospital", "state", "rate")
        # Rename the column names in the data frame
        
        hospitalDF$rate <- as.numeric(hospitalDF$rate)
        # Convert the rate column to 'numeric' as data was read as 'character'
        
        hospitalDF <- hospitalDF[complete.cases(hospitalDF), ]
        # Subsetting 'hospitalDF' by eliminating rows with NA, as invalid data won't count
        # in ranking.
        
        hospitalDF <- hospitalDF[order(hospitalDF$state), ]
        # Sort the data frame by state, ascending order
        
        hospitalList <- split(hospitalDF, hospitalDF$state)
        # Split the hospital data frame by state, a list with elements of data frame
        # will be returned. There are 54 states, hence 54 elements in the list
        
        hospitalList <- lapply(hospitalList, function(x) x[order(x$rate, x$hospital), ])
        # Order the each element of the hospital list, which is a data frame of one state,
        # by rate first then by hospital name, ascending order
        
        output <- data.frame()
        # Initialize an empty data frame
        if(is.numeric(num) | num == "best" | num == "worst") {
                if(num == "best") hosInState <- 1
                # If the input is "best", which means the 1st hospital in the data frame
                else if(is.numeric(num)) hosInState <- num
                # If a normal input number
                for(id in 1:length(hospitalList)) {
                        if(num == "worst") hosInState <- nrow(hospitalList[[id]])
                        # If the input is "worst", needs to find the index of the hospital
                        # in the bottom of a ranked data frame. It's equal to the number
                        # of rows in the data frame.
                        
                        curDF <- hospitalList[[id]][hosInState, c(1,2)]
                        # Extract the 'num'th row of data in the data frame. The [[]] will
                        # return the element value of the list, which is a data frame,
                        # can't use the [], as that'll return a list whose length is 1,
                        # the data frame is the element in the list.
                        
                        if(is.na(curDF$state)) curDF$state <- hospitalList[[id]]$state[1]
                        # If the num is larger than the number of rows of the data frame,
                        # a "NA" will be returned, hence need to find out the name of the
                        # state and assign to the state column of current data frame (one row)
                        
                        output <- rbind(output, curDF)
                        # Row bind the current data frame
                }
        }
        else stop("Invalid input!")
        
        output
}