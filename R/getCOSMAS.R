#' @export getCOSMAS
#' @title Read in concordances created with COSMAS II.
#' @description Function for reading in concordances created with the search interface of the Institute for the German Language (IDS), COSMAS II, as data frames.
#' @param filename Path to the COSMAS II output file.
#' @param merge If matches consist of more than one word
#' (e.g. if you searched for something like "the X-er the Y-er" or
#' "the ADJ person" rather than just for e.g. "word"),
#' should they be merged in one column?
#' @param years Set to TRUE if you exported "Jahresansicht" or "Jahrzehntansicht".
#' Set to FALSE (the default) if you exported an unsorted concordance (which is
#' COSMAS II's default setting). The concordances package currently only supports
#' unsorted concordances, "Jahresansicht", and "Jahrzehntansicht".
#' @param ... arguments passed on from other methods
#' @return A KWIC data frame.

getCOSMAS <- function(filename, merge=FALSE, years=FALSE, ...) {

  filename <- filename

  if(length(grep("DE|Ger.*|ger.*", Sys.getlocale()))<1) {
    warning("Your locale is not set to German, which might cause problems in handling
            umlaut characters. \n To avoid these problems, enter: \n
            - on Windows: Sys.setlocale(\"LC_ALL\", \"German_Germany\"),\n
            - on Mac or Linux: Sys.setlocale(\"LC_ALL\", \"de_DE\")).")
  }



  con <- file(filename, encoding = "latin1")
  myCosmas <- scan(con, what="character", sep="\n")
  close(con)
  myKWIC <- myCosmas[(grep("KWIC", myCosmas)[1]+1):(grep("^Belege", myCosmas)-1)]
  rm(myCosmas)
  myKWIC <- .selectsubset(myKWIC, grep("_____", myKWIC)+1)



  ###################################
  ## function for getting one line ##
  ###################################

  getCurrentLine <- function(i, key=c("combine", "split")) {

    if(missing(key)) {
      key <- "split"
    }

    dataSource <- gsub("[:alnum:]* .*", "", myKWIC[i])
    currentLine <- gsub("^[[:alnum:]]* *", "", myKWIC[i])
    left <- .nospaces(gsub("<B>.*", "", currentLine))
    right <- .nospaces(gsub(".*</>", "", currentLine))
    getKeywords <- .nospaces(.selectsubset(.splitter(currentLine, "<B>"), 2))

    if (key=="combine") {
      keyword <- gsub("</>", "", paste(c(getKeywords[1:(length(getKeywords)-1)], .splitter(getKeywords[length(getKeywords)], "</>")[1]),
                                       sep="", collapse=" "))
    } else if (key=="split") {
      keyword <- .nospaces(.splitter(getKeywords, "</>")[1:(length(.splitter(getKeywords, "</>"))-1)])
    }


    return(c(dataSource, left, keyword, right))
  }

  ############
  # get KWIC #
  ############

  # get length (= number of columns for the KWIC)
  if (merge) {
    l <- 4
  } else {
    l <- max(sapply(1:length(myKWIC), function(i) length(getCurrentLine(i))))
  }


  # dataframe for KWIC
  kwic <- as.data.frame(matrix(ncol=l, nrow=length(myKWIC)))

  ## set column names ##
  colnames(kwic)[1] <- "Source"
  colnames(kwic)[2] <- "Left"

  if(merge) {
    colnames(kwic) <- c("Source", "Left", "Key", "Right")
  } else {
    # how many keyword cells?
    kwc <- length(3:(length(colnames(kwic))-1))
    colnames(kwic)[3:(length(colnames(kwic))-1)] <- paste("Key", 1:kwc, sep="")
    colnames(kwic)[length(colnames(kwic))] <- "Right"
  }


  ## get data into the dataframe ##

  pb <- utils::txtProgressBar(min = 1, max = length(myKWIC), style = 3)

  if(merge) {
    for(i in 1:length(myKWIC)) {
      utils::setTxtProgressBar(pb, i)
      kwic$Source[i] <- getCurrentLine(i)[1]
      kwic$Left[i] <- getCurrentLine(i)[2]
      kwic$Right[i] <- getCurrentLine(i)[length(getCurrentLine(i))]

      # get number of keywords
      l2 <- length(getCurrentLine(i))
      l2 <- l2-3 # subtract Source, Left, Right

      kwic$Key[i] <- paste(getCurrentLine(i)[3:(3+(l2-1))], sep="", collapse=" ")

    }
  } else {
    for(i in 1:length(myKWIC)) {
      utils::setTxtProgressBar(pb, i)
      kwic$Source[i] <- getCurrentLine(i)[1]
      kwic$Left[i] <- getCurrentLine(i)[2]
      kwic$Right[i] <- getCurrentLine(i)[length(getCurrentLine(i))]

      # get number of keywords
      l2 <- length(getCurrentLine(i))
      l2 <- l2-3 # subtract Source, Left, Right

      kwic[i,(3:(3+(l2-1)))] <- getCurrentLine(i)[3:(3+(l2-1))]

    }
  }


  close(pb)

  ################
  # find years #
  ################

  if(years) {
    yr <- grep("Jahr unbekannt|[0-9][0-9][0-9][0-9]", kwic$Source)

    if(length(yr)>0) {
      kwic$Year <- NA

      for(j in 1:(length(yr)-1)) {
        kwic$Year[yr[j]:(yr[j+1]-1)] <- kwic$Source[yr[j]]
      }

      kwic <- kwic[-yr,]

      # add decades
      kwic$Decade <- NA
      for(j in 1:nrow(kwic)) {
        if(!is.na(kwic$Year[j]) & kwic$Year[j]!="Jahr unbekannt") {
          kwic$Decade[j] <- paste(c(unlist(strsplit(kwic$Year[j], ""))[1:3], "5"), sep="", collapse="")
        }
      }
    }




  }

  return(kwic)

}

