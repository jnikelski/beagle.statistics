\name{compute_GM_ref_vector}
\alias{compute_GM_ref_vector}

\title{Compute Average Cerebellar Gray Matter PiB Activation B Frame}

\description{
PiB ratio values are comprised of a ratio between activity at a given voxel and that in the cerebellar gray. This function computes the mean PiB intensity within the cerebellar gray matter for each frame and then returns a vector result.
}
\usage{
compute_GM_ref_vector(pib4D_volume, gmMask_volume, frameStart, frameStop)
}
\arguments{
  \item{pib4D_volume}{
A 4-dimensional PiB volume.  The first dimension is expected to be named 'time'.
}
  \item{gmMask_volume}{
A 3-D mask volume, containing 1's at the cerebellar gray matter voxels, and 0's everywhere else.
}
  \item{frameStart}{
A 1-relative start frame.  As we are ultimately interested in computing area under the curve, this should be the first frame of interest for the AUC computation.  For the HCLab, this value will be '1' under the new protocol, and '31' (34-5) under the old protocol.}
  \item{frameStop}{
A 1-relative number indicating the last frame to process.  Usually, this will be the last frame of the volume.}
}

\value{
A vector of length equal to the number of frames sampled, containing the average PiB intensity value within the cerebellar gray matter, for each frame.
}

\author{Jim Nikelski \email{nikelski@bic.mni.mcgill.ca}}
