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
#' @param more_context If TRUE (the default), the "Belege" section from the
#' concordance file is used to preserve the user-defined context window.
#' If FALSE, only the "KWIC" section from the concordance file is used,
#' which means that only ca. 5 words left and right are preserved,
#' ignoring the user-defined context window.
#' @param ... arguments passed on from other methods
#' @return A KWIC data frame.

# getCOSMAS function
getCOSMAS <- function(filename, merge=FALSE, years=FALSE, more_context=TRUE, ...) {
  filename <- filename

  if(length(grep("DE|Ger.*|ger.*", Sys.getlocale()))<1) {
    warning("Your locale is not set to German, which might cause problems in handling
            umlaut characters. \n To avoid these problems, enter: \n
            - on Windows: Sys.setlocale(\"LC_ALL\", \"German_Germany\"),\n
            - on Mac or Linux: Sys.setlocale(\"LC_ALL\", \"de_DE\")).")
  }



  con <- file(filename, encoding = "latin1")
  myCosmas <- scan(con, what="character", sep="\n", blank.lines.skip = F)
  close(con)

  # check export format: KWIC only, KWIC + full text, ...
  if(length(grep("^Belege", myCosmas)) > 0) {
    file_end <- "^Belege"
  } else if (length(grep("^Belege", myCosmas)) == 0 &
             length(grep("^Suchbegriff-Expansionslisten", myCosmas)) > 0) {
    file_end <- "^Suchbegriff-Expansionslisten"
  } else {
    file_end <- NA
  }

  if(!is.na(file_end)) {
    myKWIC <- myCosmas[(grep("KWIC", myCosmas)[1]+1):(grep(file_end, myCosmas)-1)]
  } else {
    myKWIC <- myCosmas[(grep("KWIC", myCosmas)[1]+1):(max(grep("<B>", myCosmas)))]
  }

  # rm(myCosmas)

  myKWIC <- .selectsubset(myKWIC, grep("_____", myKWIC)[1]+1)

  # remove empty lines
  myKWIC <- myKWIC[which(myKWIC!="")]






  ###################################
  ## function for getting one line ##
  ###################################

  getCurrentLine <- function(i) {

    dataSource <- gsub("[:alnum:]* .*", "", myKWIC[i])
    currentLine <- gsub("^[[:alnum:]]* *", "", myKWIC[i])

    return(c(dataSource, extractAttestationParts(currentLine)))
  }

  extractAttestationParts <- function(currentLine) {
    left <- trimws(gsub("<B>.*", "", currentLine))
    right <- trimws(gsub(".*</B?>", "", currentLine))
    getKeywords <- trimws(.selectsubset(.splitter(currentLine, "<B>"), 2))

    end_offset <- 1
    if (right=='') {
	    end_offset <- 0
    }
    keyword <- trimws(.splitter(getKeywords, "</B?>")[1:(length(.splitter(getKeywords, "</B?>"))-end_offset)])

    return(c(left, keyword, right))
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

  pb <- utils::txtProgressBar(min = 0, max = length(myKWIC), style = 3)

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

      kwic[(yr[length(yr)]+1):nrow(kwic),]$Year <- kwic$Source[yr[length(yr)]]

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

  # find extended left and right context
  if(more_context) {

    # find start of extended context
    ec_start <- grep("^Belege \\(", myCosmas)

    # find end of extended context
    ec_end <- grep("Suchbegriff-Expansionslisten", myCosmas)

    if(length(ec_end)==0) {
      ec_end <- grep("Zusammensetzung des aktiven Korpus", myCosmas)
    }

    if(length(ec_end)==0) {
      ec_end <- length(myCosmas) + 1
    }

    # extended KWIC
    if(length(ec_start)==1) {
      myExtendedKWIC <- myCosmas[ec_start:(ec_end-1)]

      # remove header
      exKWIC_start <- grep("__________________+", myExtendedKWIC)
      myExtendedKWIC <- myExtendedKWIC[(exKWIC_start+1):length(myExtendedKWIC)]




      # merge attestations that are split up into two and more lines
      # (no idea why this happens....)

      # for this purpose, find all lines empty lines
      myExtendedKWIC_empty <- which(myExtendedKWIC=="")
      # merge lines between empty lines
      myExtendedKWIC <- trimws(unlist(Map(function(x, y) paste0(myExtendedKWIC[x:y], collapse=" "), c(1, myExtendedKWIC_empty), c(myExtendedKWIC_empty, length(myExtendedKWIC)))))

      # remove blanks
      myExtendedKWIC <- myExtendedKWIC[which(myExtendedKWIC!="")]

      # check if there are unmatched keyword markers
      no_end   <- which(!grepl("</B?>", myExtendedKWIC))

      if(length(no_end) > 0) {
        for(i in 1:length(no_end)) {
          myExtendedKWIC[(no_end[i]-1)] <- paste(myExtendedKWIC[(no_end[i]-1)], myExtendedKWIC[no_end], collapse = "")
        }

        myExtendedKWIC <- myExtendedKWIC[-no_end]

      }



      attParts <- lapply(myExtendedKWIC, extractAttestationParts)
      left_ext <- sapply(attParts, "[[", 1)
      right_ext <- sapply(attParts, tail, 1)

      # get keywords
      key_ext <- lapply(attParts, head, -1)
      key_ext <- lapply(key_ext, tail, -1)
      if (merge) {
          key_ext <- lapply(key_ext, paste, sep="", collapse=" ")
      }
      else {
         # convert list with keywords to matrix
          key_ext <- t(sapply(key_ext, '[', seq(max(sapply(key_ext, length)))))
          colnames(key_ext) <- paste("key_ext", 1:ncol(key_ext), sep="")
      }

      # get rid of POS tags if there are any in the extended context
      left_ext <- gsub("<.*?>", "", left_ext)
      right_ext <- gsub("<.*?>", "", right_ext)
      key_ext <- gsub("<.*?>", "", key_ext)


      # add to KWIC
      if(length(left_ext) == length(right_ext) & length(left_ext) == nrow(kwic)) {
        kwic$left_ext <- left_ext
        kwic$right_ext <- right_ext

	kwic <- cbind(kwic, key_ext)
      }


    }









  }


  return(kwic)

}

