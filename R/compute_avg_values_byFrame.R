# ==============================================================================
# PURPOSE:
#    Given an input 4D volume and a mask, compute the average, of all
#    of the voxels that fall within the mask.  As this is done
#    over all frames, the output will consist of a vector of length
#    no_of_frames.
#
# ==============================================================================
compute_avg_values_byFrame <- function(dynamicVolname4D, opt, maskVolname=null) {
   if ( opt$debug )  {cat(sprintf("\n%s -- compute_avg_values_byFrame() -- Start\n\n", date()))}

   if ( opt$debug )  {
      cat(sprintf("compute_avg_values_byFrame: dynamic volume: %s \n", dynamicVolname4D))   
      cat(sprintf("compute_avg_values_byFrame: reference tissue mask volume: %s\n", maskVolname))
   }

   # read in mask
   stopifnot( !is.null(maskVolname) )
   maskVol <- mincIO.readVolume(maskVolname, volumeType="mask")
   if ( opt$debug )  {
   cat(sprintf("compute_avg_values_byFrame: reference tissue mask volume header info ---\n"))
   print(maskVol)
   }

   # read in dynamic 4D volume header to get some meta-data
   dynVol <- mincIO.readMincInfo(dynamicVolname4D)
   if ( opt$debug )  {
      cat(sprintf("compute_avg_values_byFrame: dynamic volume header info ---\n"))
      print(dynVol)
   }
   frameStart <- 1
   frameStop <- mincIO.getProperty(dynVol, "nFrames")
   nFramesToProcess <- frameStop - frameStart +1
   
   # loop over all frames, reading all slices
   avg_values_byFrame.v <- rep(0, nFramesToProcess)
   
   # loop over all frames in the dynamic volume
   pbar <- txtProgressBar(min=frameStart, max=frameStop, initial=frameStart, style=1, width=80)
   #
   for ( frameNo in frameStart:frameStop ) {
      #
      # read all of the slices for this frame
      dynFrame <- mincIO.readVolume(dynamicVolname4D, frameNo, volumeType="functional")
      #print(dynFrame)
   
      # apply the mask against the dynamic frame
      dynFrame_masked <- dynFrame * maskVol
   
      # compute the mean of the masked dynamic volume values
      dyn_masked_values <- dynFrame_masked[dynFrame_masked > 0]
      avg_values_byFrame.v[frameNo] <- mean(dyn_masked_values)
      
      # increment the progress bar
      setTxtProgressBar(pbar, frameNo)
   }
   # close progress bar
   close(pbar)
   
   # return vector
   if ( opt$debug )  {cat(sprintf("%s -- compute_avg_values_byFrame() -- Exit\n\n", date()))}
   return(avg_values_byFrame.v)
}
