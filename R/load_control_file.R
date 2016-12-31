load_control_file <-
function(xFilename) {

	# load the control csv
	cat("Loading PiB control information from file: ", xFilename, "\n\n")
	csv.df <- read.csv(xFilename, header=TRUE, stringsAsFactors=FALSE)

	# We are returning the data.frame
	csv.df

}

