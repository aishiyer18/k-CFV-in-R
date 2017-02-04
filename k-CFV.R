"
	Assignment 1

Name: Aishwarya Vaidyanathan
ID #: 1001255978
"


#=============================	CODE EXECUTION BEGINS	  ============================

#=============================		PART I		===============================

# Set directory path for program execution
setwd("C:\\Users\\Aishwarya\\Desktop\\Asg1")		#Here set path where the CSV & Program are present.


# Defined Func 'readk' to accept number of partitions values as input from user:

readk <- function(){ 
	k <- as.numeric(readline(prompt="\nEnter Desired Number of Partion: "))
 	return(k)
}

k <- print(readk())		# Calls func readk(), prints its output & stores value in k

#=============================		PART II	 ===============================

# Defined Func 'readcsv' to read and store CSV in different data structures
readcsv <- function(){
	ecoli <- read.csv("ecoli.csv")			# By default reads first row as header
	class <- data.frame(locsite = (ecoli[,8]))	# Assign last attribute in the CSV to vector 'class'
	example <- data.frame(ecoli[, 1:7])			# Assign all other attribute in the CSV to vector 'example'
	return(ecoli)
}



# Defined Func for normalization of CSV values
normalizecsv <- function(ecoli){
	ecoliNorm <- ecoli									# Copy data into ecoliNorm to store Normalized values
	cat("\n\nTotal Cols in Ecoli: ", ncol(ecoli), "\n")
	cat("\nTotal Cols to be Traversed: ", ncol(ecoli)-1, "\n")

	for (j in 1:(ncol(ecoli)-1)){
		maxValue <- max(ecoli[,j])							# Computes maxValue for the col
		cat("\n\nMax col[", j,"]: ", maxValue, "\n")
		minValue <- min(ecoli[,j])							# Computes minValue for the col
		cat("Min col[", j,"]: ", minValue, "\n")
	
		for (i in 1:nrow(ecoliNorm)){
			value <- ecoliNorm[i,j]
			# cat("\n\nOld Value[", i,",", j,"]: ",value)			# Displays Original Value
			ecoliNorm[i,j] <- (value - minValue)/(maxValue - minValue)	# newValue calculated as per formula provided
			# cat("\nNew Value[", i,",", j,"]: ",ecoliNorm[i,j])		# Displays Original Value
		}
	}
	return(ecoliNorm)
}


# Defined Func for kFCV
kFCV <- function(rand, k, ecoliNorm){
	
	sampleList = list()				# Initialize sampleList
	cnt <- 1						# Initialize counter
	
	krows <- ceiling(nrow(ecoliNorm)/k)		# Ceil the value of krows in a sample
	cat("\n\nTotal Number of Rows in a Sample: ", krows, "\n")
	
	for(j in 1:k){
		print(j)
		
		for(i in 1:krows){
			if(cnt > nrow(ecoliNorm)){
				cat("\nCnt: ",cnt,"\n")
				break
			}
			
			# IF it is the 1st row f sample creates new data frame 
			# ELSE appends to existing data frame
			if(i==1){
				sample <- data.frame(ecoliNorm[(rand[cnt,]),])				
			}else{
				sample <- rbind(sample, data.frame(ecoliNorm[(rand[cnt,]),]))
			}
			
			cnt <- cnt+1
		}
		sampleList [j] <- list(sample)
	}
	return(sampleList)
}

#=============================		FUNCTION CALLS	  ===============================

csvFile <- readcsv()

csvNFile <- normalizecsv(csvFile)
write.csv(csvNFile, file = "ecoliNorm.csv", row.names=FALSE)

rand <- data.frame(sample(1:336, 336, replace=F))	# Random number generation without duplicates

sampleList <- kFCV(rand, k, csvNFile)			
sampleList

# ls()			# Displays all variables in the space
# rm(j)			# Removes a single value from the space
# rm(list = ls())		# Removes all value from the space

