# These functions are shortcuts for combinations of functions that I
# use very often in the package.
# They are not needed outside of the package.



# .splitter ---------------------------------------------------------------

# combination of unlist() and strsplit() to get results
# of strsplit as a vector rather than a list
.splitter <- function(x, y, ...) {
    return(unlist(strsplit(x, split=y, ...)))
}



# .selectsubset -----------------------------------------------------------

# select a subset of a vector from the x-th to the y-th
# (usually the last) item:
# This simplifies getting a subset of vectors from the
# n-th to the last item.

.selectsubset <- function(vector, start=1, end) {
  if(missing(end)) {
    end=length(vector)
  }

  return(vector[start:end])

}




# Last Left ---------------------------------------------------------------

# core function of last_left
.last_left <- function(string, n, omit_punctuation) {

  string <- as.character(string)
  string <- unlist(strsplit(string, " "))

  # omit punctuation if specified
  if(omit_punctuation) {
    string <- string[grep("[[:alnum:]]", string)]
  }

  # length of string
  l      <- length(string)

  if(l < n) {
    return(paste(string, sep="", collapse = " "))
  } else {

    return(paste(string[(l-(n-1)):l], sep="", collapse = " "))
  }
}



# first_right -------------------------------------------------------------

# core function of first_right
.first_right <- function(string, n, omit_punctuation) {

  string <- as.character(string)
  string <- unlist(strsplit(string, " "))


  # omit punctuation if specified
  if(omit_punctuation) {
    string <- string[grep("[[:alnum:]]", string)]
  }

  # length of string
  l      <- length(string)

  if(l < n) {
    return(paste(string, sep="", collapse = " "))
  } else {

    return(paste(string[1:n], sep="", collapse = " "))
  }
}



