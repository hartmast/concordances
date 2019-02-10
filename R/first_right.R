#' @export first_right
#' @title get the first n words of a string
#' @description This function returns the first n words of a string (by default, five).
#' This can, for example, be helpful for extracting words from the right context column
#' of a concordance data frame.
#' @param x A vector or data frame.
#' @param column If x is a data frame, the number or name of the column.
#' @param n number of words to return. Default is 5.
#' @param omit_punctuation If TRUE (the default), strings consisting exclusively of
#' non-alphanumeric characters will be omitted.
#' @return A vector containing the last n words of each vector element.


# wrapper function for df and vectors -------------------------------------
first_right <- function(x, column, n = 5, omit_punctuation = TRUE) {


  if(is.data.frame(x)) {

    # if column name is missing, try to guess it
    if(missing(column)) {
      if(is.element("Right", colnames(x))) {
        column <- "Right"
      } else if(is.element("Right_context", colnames(x))) {
        column <- "Right_context"
      } else if(is.element("Right_Context", colnames(x))) {
        column <- "Right_Context"
      } else if(is.element("right_context", colnames(x))) {
        column <- "right_context"
      }
    }

    # substitute to avoid quoting
    column1 <- substitute(column)

    # also allow for entering column name as character
    if(is.character(column1)) {
      column1 <- as.symbol(column1)
    }

    # deparse column name (again, to avoid quoting)
    column2 <- deparse(column1)

    # also allow for entering column number instead of name
    if(is.numeric(column1)) {
      vec <- x[,column]
    } else {
      vec <- x[[column2]]
    }

    # get last left
    fr <- sapply(1:length(vec), function(i) .first_right(vec[i], n = n,
                                                         omit_punctuation = omit_punctuation))

  } else {
    fr <- sapply(1:length(x), function(i) .first_right(x[i], n = n,
                                                       omit_punctuation = omit_punctuation))
  }




  # return last left
  return(fr)

}



