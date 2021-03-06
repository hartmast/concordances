% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCWB.R
\name{getCWB}
\alias{getCWB}
\title{Read in concordances created with the CWB Corpus Query Processor (CQP).}
\usage{
getCWB(filename, dt = TRUE, keep.context.anno = FALSE)
}
\arguments{
\item{filename}{The name of the file you read in.}

\item{dt}{If TRUE (the default), the results will be returned as a
data.table (see ?'data.table-package' for further information).
If FALSE, they will be returned as a standard data frame.}

\item{keep.context.anno}{If TRUE, the resulting dataframe will contain
the columns "Left_with_anno" and "Right_with_anno", which contain the
left and right context with annotation (if present). If FALSE (the default),
all annotations in the left and right context will be dropped and only
the raw text will be preserved.}
}
\description{
This function reads in export files created with
the Corpus Query Processor (CQP) of the Corpus Workbench (CWB)
as data.tables (from the data.table package) or normal R dataframes.
}
\examples{
~~ getCWB(myfile) # do not run
}
