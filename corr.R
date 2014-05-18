corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    files <- complete(directory)
    monitors <- files[files$nobs > threshold, ]$id
    filelist <- file.path(directory, list.files(directory))
    filelist <- filelist[monitors]
    result <- NULL
    for (file in filelist) {
        specdata <- read.table(file, sep=",", header=T)
        result <- c(result, cor(specdata[,2], specdata[,3], use="complete.obs"))
    }
    result
}
