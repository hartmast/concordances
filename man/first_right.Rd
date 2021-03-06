% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/first_right.R
\name{first_right}
\alias{first_right}
\title{get the first n words of a string}
\usage{
first_right(x, column, n = 5, omit_punctuation = TRUE)
}
\arguments{
\item{x}{A vector or data frame.}

\item{column}{If x is a data frame, the number or name of the column.}

\item{n}{number of words to return. Default is 5.}

\item{omit_punctuation}{If TRUE (the default), strings consisting exclusively of
non-alphanumeric characters will be omitted.}
}
\value{
A vector containing the last n words of each vector element.
}
\description{
This function returns the first n words of a string (by default, five).
This can, for example, be helpful for extracting words from the right context column
of a concordance data frame.
}
