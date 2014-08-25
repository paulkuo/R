outcomes <- list(
	"heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
	"heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
	"pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")

best <- function(state, outcome) {
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
	data.valid <- data[c(index.valid), ]
	
	## Return hospital name in that state with lowest 30-day death
	## rate
	rates <- as.numeric(data.valid[, outcome.name])
	index.min <- which(rates == min(rates))
	bestHospitalName <- min(data.valid[c(index.min), "Hospital.Name"])
	
	bestHospitalName
}
