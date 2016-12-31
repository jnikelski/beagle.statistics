load_AAL_label_info <- function(lblFile) {
	#
	# load the AAL label information.  We are particularly interested in
	# being able to make a correspondence between label number and descriptive
	# name, but we may as well load it all.
	
	# load from csv file
	lbl_info.df <- read.csv(lblFile, header=TRUE, stringsAsFactors=FALSE)

	# rearrage the data.frame so that we have one line per ROI (long format)
	lbl_info_left.df <- subset(lbl_info.df, select=c(1,2,3,5,7,8,9,10))
	names(lbl_info_left.df) <- c("Description", "Abbrev", "LabelNo", "LabelName","isCerebrum", "isCortical", "isNeoCortical", "isSecondaryLbl")

	lbl_info_right.df <- subset(lbl_info.df, select=c(1,2,4,6,7,8,9,10))
	names(lbl_info_right.df) <- c("Description", "Abbrev", "LabelNo", "LabelName","isCerebrum", "isCortical", "isNeoCortical", "isSecondaryLbl")

	lbl_info.df <- rbind(lbl_info_left.df, lbl_info_right.df)
	#print(lbl_info.df)

	# sort into label number order
	lbl_info.df <- lbl_info.df[order(lbl_info.df$LabelNo),]
	lbl_info.df <- lbl_info.df[!duplicated(lbl_info.df$LabelNo),]
	#print(lbl_info.df)

	# send it back
	return(lbl_info.df)
}
