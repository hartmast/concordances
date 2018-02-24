#' @export getWACKY
#' @title Read in concordances from the WaCkY Corpus
#' @description This function allows for reading in concordances exported from the free web interface for searching the WaCkY corpora, http://nl.ijs.si/noske/wacs.cgi/first_form.
#' @param filename The name of the file you read in. You can use file.choose() to choose a file interactively. The file must be in XML (not .txt) format. It has to be formatted in UTF-8 (which should be the case by default).
#' @param tags string indicating how tokens and tags should be displayed, in case you exported a lemmatized or otherwise tagged concordance. "omit" will only show tokens and omit the lemmas. "display" will display the lemmas alongside the tokens in the same column, separated by /. "column" will add a "lemma" column to the concordance. "only" will only show lemmas in the keyword column. Default is "omit".
#' @param XML Is the concordance file in XML format? Default is TRUE. Currently only XML files are supported, support for .txt files will probably be implemented in the near future.
#' @return A file containing a tab-separated concordance sheet.


getWACKY <- function(filename, tags=c("display", "omit", "column"), XML=TRUE) {

  if(missing(tags)) {
    tags="column"
  }

  if(XML) {

    con <- file(filename, encoding="UTF-8")



    # scan data
    wackxml <- scan(con, what="character", sep="\n", fileEncoding = "UTF-8")
    close(con)

    # give warning if dealing with German data and locale not set to German
    if(any(grepl("\u00fc|\u00e4|\u00f6|\u00DF", wackxml))) {
      if(length(grep("DE|Ger.*|ger.*", Sys.getlocale()))<1) {
        warning("Your locale is not set to German, which might cause problems in handling
                umlaut characters. \n To avoid these problems, enter: \n
                - on Windows: Sys.setlocale(\"LC_ALL\", \"German_Germany\"),\n
                - on Mac or Linux: Sys.setlocale(\"LC_ALL\", \"de_DE\")).")
      }
    }

    # get start and end tags
    start_tags <- grep("<line>", wackxml)
    end_tags <- grep("</line>", wackxml)


    # function for getting current line
    .getCurrentLine <- function(i, tags=c("display", "omit", "column")) {

      if(missing(tags)) {
        tags="column"
      }

      # get KWIC + metadata
      documentSource <- gsub(" ", "", gsub("<ref>|</ref>.*", "", wackxml[start_tags[i]+1]))
      left <- gsub("^ *| *$", "", gsub(".*<left_context>|</left_context>.*", "", wackxml[start_tags[i]+1]))
      key <- gsub("^ | *$","", gsub(".*<kwic>|</kwic>.*", "", wackxml[start_tags[i]+1]))
      right <- gsub("^ | *$", "", gsub(".*<right_context>|</right_context>.*", "", wackxml[start_tags[i]+1]))
      currentKey <- .splitter(key, "  ")
      NumberOfTags <- length(.splitter(currentKey[1], "/"))
      AllTags <- sapply(1:NumberOfTags, function(k) paste(sapply(1:length(currentKey), function(j) trimws(.splitter(currentKey[j], "/"))[k]),
                                                          sep="", collapse=" "))

      # assemble KWIC dataframe
      if(tags=="column") {
        return(c(documentSource, left, AllTags[1], right, .selectsubset(AllTags, 2)))
      } else if (tags=="display") {
        return(c(documentSource, left, key, right))
      } else if (tags=="omit") {
        return(c(documentSource, left, AllTags[1], right))
      }

    }


    current.df <- as.data.frame(matrix(unlist(lapply(1:length(grep("<kwic>", wackxml)), .getCurrentLine, tags=tags)), byrow = T, ncol=length(.getCurrentLine(1, tags=tags))),
                                stringsAsFactors = FALSE)
    colnames(current.df)[1:4] <- c("Source", "Left_Context", "Key", "Right_Context")
    if(length(current.df)>4) {
      colnames(current.df)[5:length(current.df)] <- sapply(1:(length(current.df)-4), function(i) paste("tag_", i, sep="", collapse=""))
    }

    return(current.df)
  } else {
    cat("getWACKY is currently only implemented for XML files.")
  }




}
