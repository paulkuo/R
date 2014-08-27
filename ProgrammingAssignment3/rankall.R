rankall <- function(outcome, num = "best") {
	outcomes <- list(
		"heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
		"heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
		"pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	
	rank <- function(data.by.state, num, outcome.name) {
		rates <- as.numeric(data.by.state[, outcome.name])
		names <- data.by.state[, "Hospital.Name"]
		if (num == "worst") {
			num <- length(names)
		}
		rank <- order(rates, names)[num]
		if (is.na(rank)) {
			hospitalName <- NA
		} else {
			hospitalName <- names[rank]
		}
		hospitalName
	}
	
	## Check that outcome is valid
	outcome.key <- names(outcomes)[names(outcomes) == outcome]
	if (length(outcome.key) == 0) {
		stop("invalid outcome")
	}	
	outcome.name <- outcomes[[outcome.key]]
	
	## Read outcome data
	data.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	data <- data.all[, c("State", "Hospital.Name", outcome.name)]
	
	## Check that state is valid
	index.valid <- which(data[, outcome.name] != "Not Available")
	if (length(index.valid) == 0) {
		stop("invalid state")
	}
	
	## Check that num is valid
	if (num == "best") {
		num <- 1
	} else if (num != "worst" && !is.numeric(num)) {
		stop("invalid num")
	}
	
	## Get valid data
	data.valid <- data[c(index.valid), ]
	
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	data.by.state <- split(data.valid[, c("Hospital.Name", outcome.name)], data.valid[, "State"])
	result.raw <- sapply(data.by.state, rank, num, outcome.name)
	result.format <- data.frame(cbind(hospital = result.raw, state = names(result.raw)))
	result.format
}
