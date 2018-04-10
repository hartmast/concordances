#' @export export
#' @title Export concordances as tab-separated files
#' @description This function exports R objects as tab-separated files (UTF-8 by default), which is usually the ideal format for corpus concordances.
#' @param x The name of the object to be exported.
#' @param sep the field separator string. Values within each row of x are separated by this string
#' @param fileEncoding Intended encoding of the export file
#' @param filename Path to the export file. By default, the file is named like the object and saved in the current working directory.
#' @param extension File extension. Default is tsv. Ignored if a filename / file path is specified.
#' @param row.names either a logical value indicating whether the row names of x are to be written along with x, or a character vector of row names to be written. Dafault is FALSE (i.e. no row names)
#' @param ... additional arguments to be passed on to write.table.
#' @return A file containing a tab-separated concordance sheet.

export <- function(x, sep = "\t", row.names = F, fileEncoding = "UTF-8", extension = "tsv", filename, ...) {
  if(missing(filename)) {
    write.table(x, sep = sep, row.names = row.names, fileEncoding = fileEncoding,
                filename = paste(as.character(x), ".t", extension = extension, sep = "", collapse = ""), ...)

  } else {
    write.table(x, sep = sep, row.names = row.names, fileEncoding = fileEncoding,
                filename = filename, sep = "", collapse = "", ...)
  }
}
