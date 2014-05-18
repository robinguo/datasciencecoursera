complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    filelist <- file.path(directory, list.files(directory))
    filelist <- filelist[id]
    nobs <- NULL
    for (file in filelist) {
        specdata <- read.table(file, sep=",", header=T)
        specdata <- specdata[!is.na(specdata$sulfate) & !is.na(specdata$nitrate),]
        nobs <- c(nobs, nrow(specdata))
    }
    result <- data.frame(id=id, nobs=nobs)
    result
}