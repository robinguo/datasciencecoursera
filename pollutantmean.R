pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    count <- 0
    total <- 0
    for (i in seq_along(id)) {
        filename <- paste("000", id[i], sep="")
        filename <- substr(filename, nchar(filename) - 2, nchar(filename))
        filename <- paste(filename, ".csv", sep="")
        path <- paste("./", directory, "/", filename, sep="")
        specdata <- read.table(path, sep=",", header=T)
        
        data <- specdata[[pollutant]]
        data <- data[!is.na(data)]
        count <- count + length(data)
        total <- total + sum(data)
    }
    total / count
}