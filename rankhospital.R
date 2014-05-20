rankhospital <- function(state, outcome, num = "best") {
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
    rank <- seq_len(nrow(outcome.state.ordered))
    outcome.state.rank <- cbind(outcome.state.ordered, rank)
    outcome.state.rank <- outcome.state.rank[!is.na(outcome.state.rank[,2]),]
    
    if (num == "best") {
#         outcome.state.rank[1, "Hospital.Name"]
        head(outcome.state.rank[,1],1)
    } else if (num == "worst") {
        tail(outcome.state.rank[,1],1)
    } else {
        outcome.state.rank[num, "Hospital.Name"]
    }
}