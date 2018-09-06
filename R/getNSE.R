#' @export getNSE
#' @title Read in concordances created with NoSketchEngine (in particular, the NSE installation used by the COW corpora)
#' @description Function for reading in concordances created with NoSketchEngine.
#' @param filename Path to the original concordance file.
#' @param tags Boolean: Does the concordance file contain tags (lemma, pos, ...)? Default is TRUE.
#' @param convert Should the document be read into R as the dataframe (the default: convert = FALSE) or exported as a tab-separated document (if convert = TRUE)?
#' @param filename_new If convert = TRUE, the path to the new file. Default is the old filename with "_formatted" appended, in .csv format.
#' @return A KWIC data frame or a KWIC csv sheet.

getNSE <- function(filename, filename_new, tags = TRUE, convert = FALSE) {

  if(missing(filename_new)) {
    if(length(grep("\\.", filename)) > 0) {
      filename_new <- paste(unlist(strsplit(filename, "\\."))[1:(length(unlist(strsplit(filename, "\\.")))-1)], "_formatted.csv", sep="")
    } else {
      filename_new <- paste(filename, "_formatted.csv", sep="")
    }
  }

  tx <- scan(filename, what="character", sep="\n", encoding = "UTF-8")

  # strip lines with metadata
  md <- grep("^# ", tx)
  if(length(md)>0) tx <- tx[-md]

  # get metadata, if available
  if(grepl("\t", unlist(strsplit(tx[1], "<|>"))[1])) {
    mt <- lapply(1:length(tx),
                 function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[1], ",")))

    # get maximal length
    mtMax <- max(sapply(1:length(mt), function(i) length(mt[[i]])))

    # if necessary, fill mt
    for(i in 1:length(mt)) {
      if(length(mt[[i]])<mtMax) {
        mt[[i]][(length(mt[[i]])):(mtMax)] <- NA
      }
    }

    # transform to df
    kwic <- as.data.frame(matrix(ncol=mtMax, nrow=length(mt)))
    for(i in 1:nrow(kwic)) {
      kwic[i,] <- mt[[i]]
    }

    # add colnames
    colnames(kwic) <- paste("Metatag", 1:mtMax, sep="")

    # add Left, Key, Right
    l <- length(kwic)
    kwic[,((l+1):(l+3))] <- NA
    colnames(kwic)[(l+1):(l+3)] <- c("Left", "Key", "Right")

    # get left, key, right from concordance
    kwic$Left <- sapply(1:length(tx),
                        function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[1])
    kwic$Key <- sapply(1:length(tx),
                       function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[2])
    kwic$Right <- sapply(1:length(tx),
                         function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[3])


  } else {

    left = sapply(1:length(tx),
                  function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[1])

    key = sapply(1:length(tx),
                 function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[2])

    right = sapply(1:length(tx),
                   function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[3])

    kwic <- data.frame(Left = left,
                       Key = key,
                       Right = right
    )

  }





  # strip Key tags, if present
  if(tags) {

    pb <- utils::txtProgressBar(min = 1, max = nrow(kwic), style = 3)

    for(i in 1:nrow(kwic)) {
      utils::setTxtProgressBar(pb, i)
      if(length(grep("/", kwic$Key[i]))>0) {
        # current line
        x <- kwic$Key[i]

        # find word boundaries
        x <- gsub(" (?!/)", "_&_%_", x, perl=T)
        x <- unlist(strsplit(x, "_&_%_"))
        x <- x[which(x!="")]

        # replace double / multiple slash
        x <- gsub("//+", "SLA$H", x)

        # keywords (= first in the string, i.e. before first slash)
        ky <- gsub(" ", "", sapply(1:length(x), function(j) unlist(strsplit(x[j], "/"))[1]))
        ky <- paste(ky, collapse = " ")
        kwic$Key[i] <- ky

        # tags (= all after the first slash)

        # length of tags:
        l_t <- max(sapply(1:length(x), function(j) length(unlist(strsplit(x[j], "/")))))

        # add columns if necessary
        if(length(grep("Tag", colnames(kwic)))<(l_t-1)) {
          # get current length of KWIC
          l_k <- length(kwic)

          # check which "Tag" columns already exist
          t_ex <- length(grep("Tag", colnames(kwic)))

          # add columns
          if(t_ex < (l_t-1)) {
            kwic[,((l_k)+1):((l_k+t_ex)+(l_t-1))] <- NA
            colnames(kwic)[((l_k+1)):((l_k+t_ex)+(l_t-1))] <- paste("Tag", (1+t_ex):(l_t-1), sep="")

          }


        }

        # insert tags
        for(itm in 1:(l_t-1)) {
          tg <- gsub(" ", "", sapply(1:length(x), function(j) unlist(strsplit(x[j], "/"))[itm+1]))
          tg <- paste(tg, collapse = " ")
          kwic[i,which(colnames(kwic)==paste("Tag", itm, sep=""))] <- tg

        }




      }
    }

    close(pb)

  }

  if(convert) {
    utils::write.table(kwic, file = filename_new, sep="\t", row.names = F, quote = F,
                fileEncoding = "UTF-8")
  } else {
    return(kwic)
  }


}

