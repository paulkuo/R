rankhospital <- function(state, outcome, num = "best") {	outcomes <- list(
		"heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
		"heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
		"pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
	
	## Read outcome data
	data.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	outcome.key <- names(outcomes)[names(outcomes) == outcome]
	if (length(outcome.key) == 0) {
		stop("invalid outcome")
	}
	outcome.name <- outcomes[[outcome.key]]
	data <- data.all[, c("State", "Hospital.Name", outcome.name)]
	
	index.valid <- which(data[, "State"] == state & data[, outcome.name] != "Not Available")
	if (length(index.valid) == 0) {
		stop("invalid state")
	}
	
	## Check that num is valid
	if (num == "best") {
		num <- 1
	} else if (num == "worst") {
		num <- length(index.valid)
	} else if (!is.numeric(num)) {
		stop("invalid num")
	}
	
	## Return hospital name in that state with the given rank
	## 30-day death rate
	rates <- as.numeric(data[c(index.valid), outcome.name])
	names <- data[c(index.valid), "Hospital.Name"]
	rank <- order(rates, names)[num]
	if (is.na(rank)) {
		hospitalName <- NA
	} else {
		hospitalName <- names[rank]
	}
	
	hospitalName
}