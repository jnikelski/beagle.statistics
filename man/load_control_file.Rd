\name{load_control_file}
\alias{load_control_file}

\title{Load Processing Control File}

\description{
Create a data.frame containing details of all of the subjects to be processed. Information is loaded from a CSV control file.}

\usage{
load_control_file(xFilename)
}
\arguments{
  \item{xFilename}{
A full path and filename specifying from where to load the control file.  The first line of the file must be a header containing the names of the 3 variables to be loaded.  All subsequent lines contain 3 comma-separated values: (1) subject keyname, (2) PiB sub-directory identifier, and (3) Civet sub-directory identifier.}
}
\value{
A data.frame containing 3 values:
\item{subject }{Subject keyname, e.g., "trinity"}
\item{pibDir }{PiB sub-directory identifier, e.g., "trinity-20100121"}
\item{civetDir }{Civet sub-directory identifier, e.g., "trinity-20091118"}
}
\author{Jim Nikelski \email{nikelski@bic.mni.mcgill.ca}}

