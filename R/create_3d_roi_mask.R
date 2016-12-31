create_3d_roi_mask <- function(lblVolume, lblVector) {
	#
	# given a labeled volume and label value(s) as input, 
	# return a 3D minc volumeIO object
	#
	
	# read in label volume
	vol <- mincIO.readVolume(lblVolume, volumeType="label")

	# check whether passed label(s) exist in label volume
	lblValidValues <- sort(unique(as.vector(vol)))
	if ( sum(lblVector %in% lblValidValues) != length(lblVector) ) {
		warning(sprintf("One of the specified labels does *not* exist in the label volume"))
	}

	# create label mask by  matching each voxel in the volume 
	# against the vector of input label numbers
	mask_vector <- vol %in% lblVector

	# create icbm152 sampled mask volume
	maskVolume <- mincIO.makeNewVolume(filename="create3dRoiMask_mask_volume", 
										likeTemplate="icbm152")

	# place mask into mask volume
	maskVolume <- maskVolume + mask_vector

	# return mask volume
	return(maskVolume)
}
