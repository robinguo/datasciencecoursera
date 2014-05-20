best <- function(state, outcome) {
    var <- if (outcome == "heart attack") {
        11
    } else if (outcome == "heart failure") {
        17
    } else if (outcome == "pneumonia") {
        23
    } else {
        stop("invalid outcome")
    }
    
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    if (!(state %in% data$State)) {
        stop("invalid state")
    }

    outcome.state <- data[data$State == state, c(2,var)]
    outcome.state[,2] <- as.numeric(outcome.state[,2])
    outcome.state.ordered <- outcome.state[order(outcome.state[,2],outcome.state[,1]),]
    outcome.state.ordered[1,1]
}


