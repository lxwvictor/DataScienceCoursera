## Finding the best hospital in a state

best <- function(state, outcome) {
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # Read the source file, each column as character
        
        stateVect <- unique(data[, "State"])
        # Find the unique state names and store to a vector
        
        outcomeVect <- c("heart attack", "heart failure", "pneumonia")
        # The vector of 3 different outcomes
        
        ## Check that state and outcome are valid
        if(is.na(match(state, stateVect))) stop("invalid state")
        if(is.na(match(outcome, outcomeVect))) stop("invalid outcome")

        hospitalColId <- 2
        # The hospital name is at column 2
        
        if(outcome == "heart attack") outcomeColId <- 11
        else if(outcome == "heart failure") outcomeColId <- 17
        else if(outcome == "pneumonia") outcomeColId <- 23
        # Assign the column number to 'outcomeColId'
        
        hospitalDF <- data[which(data$State == state), c(hospitalColId, outcomeColId)]
        # Create a 'hospitalDF' data frame which contais only the hospital name and death rate
        
        hospitalDF[, 2] <- as.numeric(hospitalDF[, 2])
        # Convert column 2 to 'numeric' as data was read as 'character'
        
        hospitalDF <- hospitalDF[complete.cases(hospitalDF), ]
        # Subsetting 'hospitalDF' by eliminating rows with NA, as invalid data won't count
        # in ranking.
        
        hospitalDF <- hospitalDF[order(hospitalDF[, 2], hospitalDF[, 1]), ]
        # Sort the data frame by death rate first, then by hospital name ascending
        #print(str(hospitalDF))
        hospitalDF[1, 1]
        # Return the 1st hospital in the ranking list
}