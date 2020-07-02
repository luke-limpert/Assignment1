#Part 1
directory <- "~/R/specdata/"

pollutantmean <- function(directory, pollutant, id=1:332){
  directory <- "~/R/specdata/"
  files_list <-list.files(directory)
  poldata <- data.frame()
  for (i in id){
    poldata <- rbind(poldata, read.csv(paste(directory,files_list[i],sep="")))
  }
  mean(poldata[,pollutant], na.rm=TRUE)
}

pollutantmean("specdata", "nitrate")

#pollutantmean("specdata","sulfate",1:10)

#pollutantmean("specdata", "nitrate", 70:72)

#pollutantmean("specdata", "sulfate", 34)


#Part 2
complete <- function(directory, id = 1:332) {
  directory <- "~/R/specdata/"
  count_complete <- function(fname) {
    sum(complete.cases(read.csv(fname)))
  }
  fnames <- list.files(directory, full.names=TRUE)[id]
  data.frame(id = id, nobs = unlist(lapply(fnames, count_complete)))
}
#complete ("specdata", 30:25)

cc <- complete("specdata", 54)
print(cc$nobs)

#cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
#print(cc$nobs)

#Part 3
corr <- function(directory, threshold=0){
  directory <- "~/R/specdata/"
  #List of all csv files
  filelist <- list.files(path = directory, pattern = ".csv", full.names = TRUE)
  
  #Vector for values to be input into
  cor_vector <- numeric()
  
  #Loop for each file in list
  for (i in 1:length(filelist)) {
    data <- read.csv(filelist[i])
    
    cc <- sum(complete.cases(data))
    
    if (cc > threshold){
      
      dataN <- data[complete.cases(data[c('sulfate', 'nitrate')]),]
      
      cor_vector <- c(cor_vector, cor(dataN$sulfate, dataN$nitrate))
      
    }
    
  }
  
  cor_vector
  
}
cr <- corr("specdata", 150)
head(cr)

#Q7
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#Q8
cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

#Q9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

Q#10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
