vbm_quantify_by_roi <- function(vbmVolname, lblVolname, labels_table, thresholds.v) {
   # ==============================================================================
   # Function: vbm_quantify_by_roi
   # Purpose:
   #  This is the main VBM quantification function. Given:
   #
   #     (1) a VBM z-score volume
   #     (2) a AAL labels volume
   #     (3) a data.frame of ROIs
   #  ... this function will compute the number of supra-threshold voxels 
   #  at each ROI, append the results to the AAL labels data.frame, 
   #  and then return the data.frame 
   # ==============================================================================
   #
   #DEBUG 1 ===========================================
   #vbmVolname <- args_in$gm_zscore_volname_fullpath
   #lblVolname <- args_in$aal_template_volname_fullpath
   #labels_table <- lbl_details.df
   #threshold <- opts_in$zscoreThold
   #DEBUG 1 ===========================================


   #
   # read the VBM stat and label volumes-- only need to do this once
   tStatVol <- mincIO.readVolume(vbmVolname)
   lblVol <- mincIO.readVolume(lblVolname, volumeType="label")

   # threshold the tStat volume, creating a mask for each theshold
   tStatVol_tholded <- list()
   for ( ndx in 1:length(thresholds.v) ) {
      tStatVol_tholded[[ndx]] <- ifelse(tStatVol < thresholds.v[ndx], 1, 0)
      cat(sprintf("Total number of supra-threshold [z=%g] voxels: %d  \n", thresholds.v[ndx], sum(tStatVol_tholded[[ndx]])))
   }
   

   # init a new receiving list / data.frames
   quantify_rx.lst <- list()
   for ( ndx in 1:length(thresholds.v) ) {
      quantify_rx.lst[[ndx]] <- data.frame(nSupraTholdVxls = rep(sum(tStatVol_tholded[[ndx]]), nrow(labels_table)),
                                       nVxlsROI = rep(0, nrow(labels_table)),
                                       nSupraTholdVxlsROI = rep(0, nrow(labels_table)))
   }

   
   # loop over all ROIs, and compute volumes
   #ndx <- 1
   for ( lbl_ndx in 1:nrow(labels_table) ) {
      roi <- labels_table$LabelNo[lbl_ndx]
      cat(sprintf("processing ROI number %d -- %s ... \n", 
            roi, 
            labels_table$LabelName[lbl_ndx]))
            
      # compute the number of voxels (i.e., volume) for this ROI
      roiMasked_lblVol <- volume.explodeLabelVolume(lblVol, labels=c(roi), civetLabels=FALSE)[[1]]

      # in the case that the label volume does not contain an ROI that
      # exists in the .csv file, set both fields to NA
      if ( sum(roiMasked_lblVol) == 0 ) {
         for ( ndx2 in 1:length(thresholds.v) ) {
            quantify_rx.lst[[ndx2]]$nVxlsROI[lbl_ndx] <- 0
            quantify_rx.lst[[ndx2]]$nSupraTholdVxlsROI[lbl_ndx] <- NA
         }
      } else {
         #
         # apply mask to thresholded VBM volume and count the number of 
         # supra-threshold voxels in this ROI
         for ( ndx2 in 1:length(thresholds.v) ) {
            quantify_rx.lst[[ndx2]]$nVxlsROI[lbl_ndx] <- sum(roiMasked_lblVol)
            tStatVol_tholded_masked_by_roi <- tStatVol_tholded[[ndx2]] * roiMasked_lblVol
            quantify_rx.lst[[ndx2]]$nSupraTholdVxlsROI[lbl_ndx] <- sum(tStatVol_tholded_masked_by_roi)
         }
      }
   }

   # return the quantified results
   return(quantify_rx.lst)
}
