\name{compute_ratios}
\alias{compute_ratios}

\title{
Compute PiB Ratio Values at each Voxel}

\description{
For each subject, compute PiB ratio values at each voxel.}

\usage{
compute_ratios(pkt)
}

\arguments{
  \item{pkt}{
Rather than passing a ga-zillion arguments, a list of arguments specific to the processing of the current subject is passed.  See the sources for a description of each of these arguments.}
}

\value{
As I didn't want this function to perform any IO, a 2-item list() is retuned instead to the calling routine, who can then extract the returned components and perform any IO as needed.

\item{gmAUCplot}{A lattice plot object in which the average cerebellar intensity is plotted against time.  The average cerebellar intensity vector is produced by \code{\link{compute_GM_ref_vector}}}

\item{pibTacVol}{A 3-dimensional mincIO object, containing PiB ratios at every gray matter voxel.  Note that the PiB ratio is set to zero (0) for every non-gray matter voxel, via application of a Civet-derived gray matter mask.}
}

\author{Jim Nikelski \email{nikelski@bic.mni.mcgill.ca}}
