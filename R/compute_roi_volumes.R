compute_roi_volumes <- function(AAL_label_filename, lblVolname, roi_vector) {
	#
	# given a labeled volume as input, return a data,frame
	# containing the number of voxels for each label requested
	# in the roi_vector
	
	# read in the AAL label details
	lblFile <- AAL_label_filename
	lbl_info.df <- load_AAL_label_info(lblFile)
	lbl_info.df$volume <- rep(0, nrow(lbl_info.df))
	
	# loop over all ROIs, and compute volumes
	for ( roi in roi_vector ) {
		lbl_df_ndx <- match(roi, lbl_info.df$LabelNo)
		# only process if requested ROI exists in the ROI data.frame
		if ( !is.na(lbl_df_ndx) ) {
			cat(sprintf("processing ROI number %d -- %s ... ", 
			             roi, 
			             lbl_info.df$LabelName[lbl_df_ndx]))
			maskVol <- create3dRoiMask(lblVolname, roi)
			roiVolume <- sum(maskVol)
			cat(sprintf("Volume is %d voxels\n", roiVolume))
			lbl_info.df$volume[lbl_df_ndx] <- roiVolume
		}
	}
	# return the modified AAL data.frame
	return(lbl_info.df)
}
