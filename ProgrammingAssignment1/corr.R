corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
	
	corrs <- vector("numeric")
	for (csv in dir(directory)) {
		filename <- paste(directory, "/", csv, sep = "") 
		table <- read.table(filename, header = T, sep = ",", colClasses = c("character", "numeric", "numeric"), comment.char="")
		complete <- complete.cases(table[, 2:3])
		if (length(complete[complete == T]) <= threshold) {
			next()
		}
		corr <- cor(table[, 2], table[, 3], use = "complete.obs")
		corrs = c(corrs, corr)
	}
	corrs
}
