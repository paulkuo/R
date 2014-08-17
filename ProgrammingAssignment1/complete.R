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
	
	completes <- data.frame(id, nobs = vector("numeric", length = length(id)))
	row.idx = 1
	for (i in id) {
		filename <- paste(directory, "/", formatC(i, width = 3, format = "d", flag = "0"), ".csv", sep = "") 
		#print(filename)
		table <- read.table(filename, header = T, sep = ",", colClasses = c("character", "numeric", "numeric"), comment.char="")
		complete <- complete.cases(table[, 2:3])
		completes[row.idx, "nobs"] <- length(complete[complete == T])
		row.idx <- row.idx + 1
	}
	completes
}