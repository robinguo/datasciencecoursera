rankall <- function(outcome, num = "best") {
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
    data <- data[, c(2,7,var)]
    data[,3] <- as.numeric(data[,3])
    data <- data[!is.na(data[,3]), ]
    
    state.f <- as.factor(data[,2])
    outcome.state <- split(data, state.f)
    hospital <- sapply(outcome.state, rankstate, num, simplify=T, USE.NAMES=F)
    state <- levels(state.f)
    as.data.frame(cbind(hospital, state))
}

rankstate <- function(outcome, num) {
    outcome.ordered <- outcome[order(outcome[,3],outcome[,1]),]
    if (num == "best") {
        head(outcome.ordered[,1],1)
    } else if (num == "worst") {
        tail(outcome.ordered[,1],1)
    } else {
        outcome.ordered[num, 1]
    }
}