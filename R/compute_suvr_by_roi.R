compute_suvr_by_roi <- function(pibVolname, lblVolname, labels_table) {
	#
	# This is the main SUVR function.
	# Given (1) a ratio volume, (2) a AAL labels volume, and 
	# (3) a data.frame of ROIs ...
	# ... compute the volume and mean SUVR at each ROI, append the results
	# ... to the AAL labels data.frame, and then return the data.frame 
	
	#

	# read the PiB ratio volume -- only need to do this once
	pibVolume <- mincIO.readVolume(pibVolname)

	# init 2 new receiving variables
	labels_table$nVoxels <- rep(0, nrow(labels_table))
	labels_table$suvr <- rep(-1, nrow(labels_table))
	
	# loop over all ROIs, and compute volumes
	for ( ndx in 1:nrow(labels_table) ) {
		
		roi <- labels_table$LabelNo[ndx]
		cat(sprintf("processing ROI number %d -- %s ... ", 
			            roi, 
			            labels_table$LabelName[ndx]))
			            
		# compute the number of voxels (i.e., volume) for this ROI
		maskVol <- create_3d_roi_mask(lblVolname, roi)

		# in the case that the label volume does not contain an ROI that
		# exists in the .csv file, set both fields to NA
		if ( sum(maskVol) == 0 ) {
		  labels_table$nVoxels[ndx] <- NA
		  labels_table$suvr[ndx] <- NA
		  cat(sprintf("volume: None SUVR: NA \n"))

		} else {
		  labels_table$nVoxels[ndx] <- sum(maskVol)
		  # apply mask to PiB volume and compute the mean SUVR
		  pibVolMasked <- pibVolume * maskVol
		  labels_table$suvr[ndx] <- sum(pibVolMasked)/sum(maskVol)
		  # tell the world
		  cat(sprintf("volume: %d SUVR: %g \n", 
						labels_table$nVoxels[ndx],
						labels_table$suvr[ndx]))
		}
	}
	
	# return the modified AAL data.frame
	return(labels_table)
}
