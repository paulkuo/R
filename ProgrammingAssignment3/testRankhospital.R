source("rankhospital.R")

testRankhospital <- function() {
	test <- function(state, outcome, num = "best", result) {
		if (is.na(result)) {
			if (is.na(try(rankhospital(state, outcome, num), silent = T))) {
				print("Pass")
			} else {
				print("Fail")
			}
		}
		else if (try(rankhospital(state, outcome, num), silent = T) == result || geterrmessage() == result) {
			print("Pass")
		} else {
			print("Fail")
		}
	}
	
	test("TX", "heart failure", 4, "DETAR HOSPITAL NAVARRO")	test("MD", "heart attack", "worst", "HARFORD MEMORIAL HOSPITAL")
	test("MN", "heart attack", 5000, NA)}
