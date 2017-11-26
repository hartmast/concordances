# These functions are shortcuts for combinations of functions that I 
# use very often in the package.
# They are not needed outside of the package.

# combination of unlist() and strsplit() to get results of strsplit as a vector rather than a list
.splitter <- function(x,y, pasteitems = FALSE) {
  if(pasteitems) {
    return(paste(unlist(strsplit(x, split=y)), sep="", collapse=" "))
  } else {
    return(unlist(strsplit(x, split=y)))
  }
}

# select a subset of a vector from the x-th to the y-th (usually the last) item:
# This simplifies getting a subset of vectors from the n-th to the last item.
.selectsubset <- function(vector, start=1, end) {
  if(missing(end)) {
    end=length(vector)
  }

  return(vector[start:end])

}


# remove spaces at the beginning and at the end of character strings
.nospaces <- function(x) {
  return(gsub("^ *| *$", "", x))
}

