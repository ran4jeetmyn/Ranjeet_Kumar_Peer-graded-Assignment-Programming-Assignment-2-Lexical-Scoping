getwd()
getwd()
pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  setwd(file.path(getwd(), directory)) ## setting the directory
  total = 0                            ## the sum of all observed values of pollutant (either sulfate or nitrate)
  observations = 0                     ## the total number of observed values of pollutant (either sulfate or nitrate)
  
  #Looping thru the directory's files specified in the 'id' argument 
  for (i in id)
  {
    
    
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10) { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    
    
    else       { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data)    
    ##  cumulative addition of the complete observations
    observations = observations + nrow(data)
    ## depending the poluttant ( sulfate or nitrate), we aggregate the observed values
    if (pollutant == "sulfate") {total = total + sum(data$sulfate)}
    else {total = total + sum(data$nitrate)}
    
  }
  
  ## reset directory path
  setwd("..")
  ## returning the mean of the pollutant values
  return (total/observations)
  
}
Let's put this function to work,

pollutantmean("specdata", "sulfate", 34)
## [1] 4.064128
pollutantmean("specdata", "nitrate" )
## [1] 1.732979
pollutantmean("specdata", "nitrate", 34)
## [1] 1.280833
These answers match the expected outputs

Part-2
Write a function that reads a directory full of files and reports the number of completely observed cases in each data file. The function should return a data frame where the first column is the name of the file and the second column is the number of complete cases.

complete <- function(directory, id = 1:332) {
  
  
  dataframe = NULL  ## initializing the dataframe we want from this function   
  setwd(file.path(getwd(), directory)) ## setting the directory
  
  #Looping thru the directory's files specified in the 'id' argument 
  for (i in id)
  {
    
    
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10) { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    
    
    else       { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data) 
    ##  make it a matrix to easily fill each successive row of our dataframe
    data = as.matrix(data)
    dataframe = rbind(dataframe, c(i,nrow(data))) # fill each successive row of our dataframe. Each row contains the monitor ID,
    # and its total complete observed cases (no rows containg NAs)
    
    
    
  }
  
  setwd("..")  # reseting working directory path
  dataframe = data.frame(dataframe)  # from matix to data frame 
  names(dataframe) = c('id', 'nobs') # set the column names of the data frame
  return (dataframe) 
}
complete <- function(directory, id = 1:332) {
  
  
  dataframe = NULL  ## initializing the dataframe we want from this function   
  setwd(file.path(getwd(), directory)) ## setting the directory
  
  #Looping thru the directory's files specified in the 'id' argument 
  for (i in id)
  {
    
    
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10) { 
      data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
                       header = T, 
                       na.strings=c("NA","NaN", " "))
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    
    
    else       { 
      data <- read.csv(paste(as.character(i), ".csv", sep=""),     ## Normal
                       header = T, 
                       na.strings=c("NA","NaN", " ") 
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data) 
    ##  make it a matrix to easily fill each successive row of our dataframe
    data = as.matrix(data)
    dataframe = rbind(dataframe, c(i,nrow(data))) # fill each successive row of our dataframe. Each row contains the monitor ID,
    # and its total complete observed cases (no rows containg NAs)
    
    
    
  }
  
  setwd("..")  # reseting working directory path
  dataframe = data.frame(dataframe)  # from matix to data frame 
  names(dataframe) = c('id', 'nobs') # set the column names of the data frame
  return (dataframe) 
}
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
corr <- function(directory, threshold = 0) {
  setwd(file.path(getwd(), directory)) ## setting the directory
  
  correlationVector = NULL ## initializing the correlation matrix
  
  #Looping thru ALL the directory's files 
  for (i in 1:332)
  {
    
    
    ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works but not efficient, 
    ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
    if (i <10) { 
      data <- read.csv(
        paste("0","0", as.character(i), ".csv", sep=""),  ## for example, if 'id' =7, we get 007.csv
        header = T, 
        na.strings=c("NA","NaN", " ")
        
      )
    }
    
    else if (i>=10 & i<100) { 
      data <- read.csv(
        paste("0", as.character(i), ".csv", sep=""),  ## for example, if 'id' = 17, we get 017.csv
        header = T, 
        na.strings=c("NA","NaN", " ") 
        
      )
    }
    
    
    
    else       { 
      data <- read.csv(
        paste(as.character(i), ".csv", sep=""),     ## Normal
        header = T, 
        na.strings=c("NA","NaN", " ") 
        
      )
    }
    
    ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations (the ones with at least one NA in row)
    data = na.omit(data) 
    
    ## if the number of complete observed cases meets the quota, find the correlation between the pollutants for the given monitor AND
    ## store the results in the correlation matrix
    if (nrow(data) > threshold) {
      correlationVector = c(correlationVector, cor(data[,2], data[,3]))
    }
    
    
  }
  
  
  
  setwd("..")  # reseting working directory path
  return (correlationVector)
}
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
