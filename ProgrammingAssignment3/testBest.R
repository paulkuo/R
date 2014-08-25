source("best.R")

testBest <- function() {
	test <- function(state, outcome, result) {
		if (try(best(state, outcome), silent = T) == result || geterrmessage() == result) {
			print("Pass")
		} else {
			print("Fail")
		}
	}
	
	test("TX", "heart attack", "CYPRESS FAIRBANKS MEDICAL CENTER")
	test("TX", "heart failure", "FORT DUNCAN MEDICAL CENTER")
	test("MD", "heart attack", "JOHNS HOPKINS HOSPITAL, THE")
	test("MD", "pneumonia",	"GREATER BALTIMORE MEDICAL CENTER")
	test("BB", "heart attack", "Error in best(state, outcome) : invalid state\n")
	test("NY", "hert attack", "Error in best(state, outcome) : invalid outcome\n")
}
