\name{ldiPibStatistics-package}
\alias{ldiPibStatistics-package}
\alias{ldiPibStatistics}

\docType{package}

\title{PiB Statistical Analysis in the Howard Chertkow Lab}

\description{
At the current time, the primary focus of this package is to compute volume-level PiB ratios.  Other stuff may follow, but this seems to be a perfectly reasonable starting point.}

\details{
\tabular{ll}{
Package: \tab ldiPibStatistics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-03-07\cr
License: \tab BSD\cr
LazyLoad: \tab yes\cr
}
The purpose of this package is to create PiB ratio values at every voxel of a given 4-dimensional functional PiB volume.  Prior to using this package, some fairly sophisticated pre-processing needs to be done first.  For example, every PiB scan requires a matched T1 anatomical MRI that has been run through the MNI Civet pipeline prior to processing.  The pipeline furnishes a spatial normalization target in addition to a bunch of other things, such as tissue classification (INSECT) and ROI labelling (ANIMAL).  The Civet components are critical to the pre-processsing of the functional PiB volume.  Note that Civet-generated volumes are in minc2 format -- which is really good, since this package really wants all volumes to use minc2.

Note that the output of this package does not include much in the way of visualization.  This is because surface and volumetric visualization is accomplished via a set of specialized post-processing scripts.  So, that is to say, the primary output of this package is one PiB ratio volume (minc) for each subject processed.

}
\author{Jim Nikelski \email{nikelski@bic.mni.mcgill.ca}}
