# ==============================================================================
# PURPOSE:
#    Basically, this function computes the Area Under the Curve for given
#    reference tissue.
#    The reference tissue is defined by a masked minc volume, with all
#    voxels falling within the mask are to be used as reference.
#
# Flow:
#  1. for each frame, compute the average intensity value of all voxels
#     falling within the reference tissue mask. This will produce a vector
#     whose length reflects the number of frames.
#     Note: doing this makes the implicit assumption that all tissues
#        comprising the mask should behave similarly, and as such,
#        the signal can be averaged.
#
#  2. compute the AUC on the averaged intensity vector
#
#  3. create a pretty plot of the AUC vector
#
#  4. return: AUC scalar, AUC vector, pretty plot
#
# ==============================================================================
compute_ref_tissue_auc <- function(pkt, opt) {
   if ( opt$debug )  {cat(sprintf("\n%s -- compute_ref_tissue_auc() -- Start\n\n", date()))}
   if ( opt$verbose ) {cat("Computing PiB reference tissue AUC values for subject: ", pkt$keyname, "\n\n")}

   # function to compute AUC  (thanks to Frank E Harrell from the R lists)
   trap.rule <- function(y,x) sum(diff(x)*(y[-1]+y[-length(y)]))/2


   # compute the average reference activity across all frames
   returnList <- list()
   avg_refValues_byFrame.v <- compute_avg_values_byFrame(pkt$pib_4d_vol, opt, pkt$refTissue_volname_fullPath)
    if ( opt$debug ) {
        cat("\nAverage reference tissue activity for all frames:\n")
        print(avg_refValues_byFrame.v)
    }
   returnList$avg_refValues_byFrame.v <- avg_refValues_byFrame.v
   
   
   # compute the area under the curve (AUC) for the reference vector
   # ... first, need to get the mid-frame times from the PiB volume
   #        mid-frame times = time at the middle of each frame
   pibVol <- mincIO.readMincInfo(pkt$pib_4d_vol)
   mid_frame_times.v <- mincIO.getProperty(pibVol, "timeOffsets") + (mincIO.getProperty(pibVol, "timeWidths")/2)
   if ( opt$debug ) {
      cat("\nMid-frame times for all frames:\n")
      print(mid_frame_times.v)
   }
   returnList$mid_frame_times.v <- mid_frame_times.v
   
   
   # compute AUC
   # ... only use the last nFramesToProcess frames to compute the AUC
   avg_refValues_byFrameX.v <- avg_refValues_byFrame.v[pkt$frameStart:pkt$frameStop]
   mid_frame_timesX.v <- mid_frame_times.v[pkt$frameStart:pkt$frameStop]
   #
   refAuc <- trap.rule(avg_refValues_byFrameX.v, mid_frame_timesX.v)
   if ( opt$verbose )  {
      cat(sprintf("\nReference tissue area under the curve (AUC): %f\n", refAuc))
   }
   returnList$refAuc <- refAuc
   
   
   # create an AUC plot using all frames
   # ... set ordinate minimum to ZERO to minimize scale compression effects,
   # ... and set the maximum to at least 100
   # ... thus we have a range of at least (0,100)
   ylim_min <- 0
   ylim_max <- round(max(max(avg_refValues_byFrame.v),100))
   refAUCplot <- xyplot(avg_refValues_byFrame.v ~ mid_frame_times.v/60, valueAUC=refAuc,
      main = "Average Reference Tissue PiB Activity Over Time",
      sub = sprintf("Subject: %s", pkt$pibAucPlotSubtitle),
      xlab = "Mid-Frame Times Relative to Acquisition Start (in minutes)",
      ylab = "PiB Intensity",
      ylim = c(ylim_min,ylim_max),
      panel = function(valueAUC=refAUC, ...) {
         panel.fill(col="yellow", ...)
         panel.grid(col="black", lwd=.5)
         panel.points(...)
         panel.lines(lwd=2,...)
         panel.xyplot(...)
         panel.text(min(mid_frame_times.v/60), 
                  5, 
                  sprintf("AUC = %-12.f", valueAUC), adj=c(0.0,0.0))
      })
   #
   returnList$refAUCplot <- refAUCplot
   if ( opt$debug )  {cat(sprintf("%s -- compute_ref_tissue_auc() -- Exit\n\n", date()))}
return(returnList)
}


