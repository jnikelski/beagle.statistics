# ==============================================================================
# PURPOSE:
#    Compute the area under the curve (AUC) at each voxel in the 4D
#    PiB volume.
#    Note: Only integrate over the *selected* frames
#
#  Input args:
#     1. pkt$keyname
#     2. pkt$pib_4d_vol
#     3. pkt$frameStart
#     4. pkt$frameStop
#     5. pkt$nFramesToProcess
#    
# ==============================================================================
compute_dynamic_volume_auc <- function(pkt, opt) {
   if ( opt$debug )  {cat(sprintf("\n%s -- compute_dynamic_volume_auc() -- Start\n\n", date()))}
   if ( opt$verbose ) {cat("Computing Area Under the Curve (AUC) at each voxel for the 4D PiB volume for subject: ", pkt$keyname, "\n\n")}
   
   # function to compute AUC  (thanks to Frank E Harrell from the R lists)
   trap.rule <- function(y,x) sum(diff(x)*(y[-1]+y[-length(y)]))/2
   
   # get PiB volume header details
   pibVol <- mincIO.readMincInfo(pkt$pib_4d_vol)

   # create output volume
   pibAucVol <- mincIO.makeNewVolume("pibAucVolume.mnc", likeTemplate="icbm152")
   
   # mid-frame times = time at the middle of each frame
   mid_frame_times <- mincIO.getProperty(pibVol, "timeOffsets") + (mincIO.getProperty(pibVol, "timeWidths")/2)
   cat("Mid-frame times across *all* frames:\n")
   if ( opt$debug ) {
      cat("compute_dynamic_volume_auc():  Print *mid_frame_times* -- \n")
      print(mid_frame_times)
   }
   #
   cat("Mid-frame times across *selected* frames:\n")
   mid_frame_timesX <- mid_frame_times[pkt$frameStart:pkt$frameStop]
   if ( opt$debug ) {
      cat("compute_dynamic_volume_auc():  Print *mid_frame_times* across SELECTED frames -- \n")
      print(mid_frame_timesX)
   }

   # loop over all slices, computing integrated values for each voxel
   nSlices <- mincIO.getProperty(pibVol, "sizes")["zspace"]
   
   pbar <- txtProgressBar(min=1, max=nSlices, initial=1, style=1, width=80)
   cat("\n** Computing AUC for all voxels/frames over slices: \n")
   for ( sliceNo in 1:nSlices) {

      # read all of the frames for this slice, returning a MincSliceIO object
      pib_slice_mat <- mincIO.readBySlice(pkt$pib_4d_vol, sliceNo)
      #if ( opt$debug ) {
      #   cat("compute_dynamic_volume_auc():  Print *pib_slice_mat* structure -- \n")
      #   print(str(pib_slice_mat))
      #}

      # if we are not integrating over all frames, select a subset
      if ( pkt$nFramesToProcess != pkt$frameStop ) {
         pib_slice_mat <- pib_slice_mat[,pkt$frameStart:pkt$frameStop]
      }
   
      # compute the AUC for all voxels in this slice and save them to volume
      pibAucVol[,,sliceNo] <- apply(pib_slice_mat, 1, trap.rule, mid_frame_timesX)
      
      # increment the progress bar
      setTxtProgressBar(pbar, sliceNo)
   }
   # close progress bar
   close(pbar)
   
   # return AUC volume
   if ( opt$debug )  {cat(sprintf("%s -- compute_dynamic_volume_auc() -- Exit\n\n", date()))}
   return(pibAucVol)
}
