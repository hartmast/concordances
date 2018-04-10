#' @export export
#' @title Export concordances as tab-separated files
#' @description This function - a convenient wrapper for write.table - exports R objects as tab-separated files (UTF-8 by default), which is usually the ideal format for corpus concordances.
#' @param x The name of the object to be exported.
#' @param sep the field separator string. Values within each row of x are separated by this string
#' @param fileEncoding Intended encoding of the export file
#' @param filename Path to the export file. By default, the file is named like the object and saved in the current working directory.
#' @param extension File extension. Default is tsv. Ignored if a filename / file path is specified.
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written. Dafault is FALSE (i.e. no row names)
#' @param quote a logical value (TRUE or FALSE) or a numeric vector. If TRUE, any character or factor columns will be surrounded by double quotes. If a numeric vector, its elements are taken as the indices of columns to quote. In both cases, row and column names are quoted if they are written. If FALSE (the default), nothing is quoted.
#' @param ... additional arguments to be passed on to write.table.
#' @return A file containing a tab-separated concordance sheet.

export <- function(x, sep = "\t", row.names = F, fileEncoding = "UTF-8", extension = "tsv",
                   filename, quote, ...) {
  if(missing(filename)) {
    x2 <- deparse(substitute(x))
    filename <- paste(as.character(x2), ".", extension, sep = "", collapse = "")
  }
     write.table(x, sep = sep, row.names = row.names, fileEncoding = fileEncoding,
                file = filename, quote = quote, ...)
}

