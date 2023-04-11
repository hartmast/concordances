#' @export getNSE
#' @title Read in concordances created with NoSketchEngine (in particular,
#' the NSE installation used by the COW corpora)
#' @description Function for reading in concordances created with NoSketchEngine.
#' @param filename Path to the original concordance file.
#' @param tags Boolean: Does the concordance file contain tags (lemma, pos, ...)
#' for the keyword(s)?
#' If missing, the function will try to determine automatically if there are tags.
#' @param context_tags Boolean: Does the concordance file contain tags (lemma, pos, ...)
#' for the context?
#' If missing, the function will try to determine automatically if there are tags.
#' @param xml Boolean: Is the export file an XML or a TXT file? (If missing,
#' the function will try to determine this automatically.) Using XML files instead
#' of TXT files is strongly recommended: The identification of KWIC columns can be
#' problematic in the latter as pointed brackets (< / >) are used as separators in the
#' TXT export files, but they sometimes also occur in the concordance text.
#' @param verbose Boolean: If TRUE (the default), the function will display occasional
#' status updates, which can helpful in the case of large files.
#' @return A KWIC data frame.

getNSE <- function(filename, xml, tags, context_tags, verbose = TRUE) {

  tx <- scan(filename, what="character", sep="\n", encoding = "UTF-8")

  if(missing(xml)) {
    if(grepl("<\\?xml", tx[1])) {
      xml = TRUE
    } else {
      xml = FALSE
    }
  }

  if(xml) {

    # get Left, Right, Key
    findlines <- grep("<kwic>", tx)
    lc <- gsub(".*<left_context>|</left_context>.*", "", tx[findlines])
    key <- gsub(".*<kwic>|</kwic>.*", "", tx[findlines])
    rc <- gsub(".*<right_context>|</right_context.*", "", tx[findlines])


    # get metadata, if available
    if(any(grepl("<ref>", tx))) {
      mt <- grep("<ref>", tx)
      mt <- gsub(".*<ref>|</ref>.*", "", tx[mt])

      # in some cases, the metadata element in <ref> is empty,
      # therefore we have to check first whether mt contains
      # any strings at all:
      if(!all(sapply(1:length(mt), function(i) identical(mt[[i]], character(0)))) &
         !all(sapply(1:length(mt), function(i) mt[[i]]==""))) {
        # if the metadata contain URLs, this can cause problems if the URL
        # contains commas. This line detects URLs with commas and
        # replaces them
        mt <- gsub(",(?=.*html?,)", "%%%COMMA%%%", mt, perl=T)


        # get individual metadata
        mt <- lapply(1:length(mt), function(i) unlist(strsplit(mt[i], ",")))

        # get maximal length
        mtMax <- max(sapply(1:length(mt), function(i) length(mt[[i]])))

        # if necessary, add more elements to mt (= vector for metadata)
        for(i in 1:length(mt)) {
          if(length(mt[[i]])<mtMax) {
            mt[[i]][((length(mt[[i]]))+1):(mtMax)] <- NA
          }
        }

        # transform to df
        kwic <- as.data.frame(matrix(ncol=mtMax, nrow=length(mt)),
                              stringsAsFactors = F)
        for(i in 1:nrow(kwic)) {
          kwic[i,] <- mt[[i]]
        }

        # add colnames
        colnames(kwic) <- paste("Metatag", 1:mtMax, sep="")

        # transform %%%COMMA%%% in URLs back to real commas
        kwic$Metatag1 <- gsub("%%%COMMA%%%", "", kwic$Metatag1)

        # add left, right, key
        kwic$Left <- lc
        kwic$Key <- key
        kwic$Right <- rc
      } else {
        kwic <- data.frame(Left  = lc,
                           Key   = key,
                           Right = rc)
      }




    } else {

      kwic <- data.frame(Left  = lc,
                         Key   = key,
                         Right = rc)

    }



  } else {

    # strip lines with metadata about query infos
    md <- grep("^# ", tx)
    if(length(md)>0) tx <- tx[-md]

    # get metadata, if available
    if(grepl("\t", unlist(strsplit(tx[1], "<|>"))[1])) {


      # if the metadata contain URLs, this can cause problems if the URL
      # contains commas. This line detects URLs with commas and
      # replaces them
      tx <- gsub(",(?=.*html?,)", "%%%COMMA%%%", tx, perl=T)

      # check if hits are numbered in the very first column
      # (which means that there's another column before the
      # metadata column)
      if(length(unlist(strsplit(tx[1], "\t"))) > 2) {
        mt <- lapply(1:length(tx),
                     function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], ",")))
      } else {
        mt <- lapply(1:length(tx),
                     function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[1], ",")))
      }


      # get maximal length
      mtMax <- max(sapply(1:length(mt), function(i) length(mt[[i]])))

      # if necessary, add more elements to mt (= vector for metadata)
      for(i in 1:length(mt)) {
        if(length(mt[[i]])<mtMax) {
          mt[[i]][(length(mt[[i]])+1):(mtMax)] <- NA
        }
      }

      # transform to df
      kwic <- as.data.frame(matrix(ncol=mtMax, nrow=length(mt)))
      for(i in 1:nrow(kwic)) {
        kwic[i,] <- mt[[i]]
      }

      # add colnames
      colnames(kwic) <- paste("Metatag", 1:mtMax, sep="")

      # transform %%%COMMA%%% in URLs back to real commas
      kwic$Metatag1 <- gsub("%%%COMMA%%%", "", kwic$Metatag1)

      # add Left, Key, Right
      l <- length(kwic)
      kwic[,((l+1):(l+3))] <- NA
      colnames(kwic)[(l+1):(l+3)] <- c("Left", "Key", "Right")


      # check if there are < or > in the data that don't belong to
      # the keyword
      tx <- gsub(">(?=<)", "RIGHTARROW", tx, perl = T)
      tx <- gsub("<(?=>)", "LEFTARROW", tx, perl = T)


      # get left, key, right from concordance

      # check again if hits are numbered in the very first column
      # (which means that there's another column before the
      # metadata column)
      if(length(unlist(strsplit(tx[1], "\t"))) > 2) {
        n <- 3
      } else {
        n <- 2
      }

      kwic$Left <- sapply(1:length(tx),
                          function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[n], "<|>"))[1])
      kwic$Key <- sapply(1:length(tx),
                         function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[n], "<|>"))[2])
      kwic$Right <- sapply(1:length(tx),
                           function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[n], "<|>"))[3])


    } else {

      left <- sapply(1:length(tx),
                     function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[1])

      key <- sapply(1:length(tx),
                    function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[2])

      right <- sapply(1:length(tx),
                      function(i) unlist(strsplit(unlist(strsplit(tx[i], "\t"))[2], "<|>"))[3])

      kwic <- data.frame(Left = left,
                         Key = key,
                         Right = right
      )

    }


  }


  # check if there are tags, if not specified in arguments

  if(missing(tags)) {
    # check if there are tags in the keyword column
    if(all(grepl("/", kwic$Key))) {
      tags = TRUE
    } else {
      tags = FALSE
    }
  }

  # check if there are tags in the context columns
  if(missing(context_tags)) {
    if(all(grepl("/", grep("[[:alnum:]]", kwic$Left, value = T))) | all(grepl("/", grep("[[:alnum:]]", kwic$Right, value = T)))) {
      context_tags = TRUE
    } else {
      context_tags = FALSE
    }
  }



  if(tags) {

    # create _with_anno column
    kwic$Key_with_anno   <- kwic$Key

    # function for getting annotations
    # (the lookaround assertions make sure that slashes
    # that belong to the original text are NOT replaced)
    get_anno <- function(myTx) {
      w <- .splitter(trimws(gsub("[[:space:]]+/(?![[:space:]])", "/", myTx, perl = T)), " ")
      w <- w[which(w!="")] # remove empty vector elements
      w <- lapply(1:length(w), function(i) .splitter(w[i], "(?<!^)/", perl = T))

      return(w)
    }

    # how many annotation tags?
    w <- get_anno(kwic$Key[1])
    la <- max(sapply(1:length(w), function(i) length(w[[i]])))

    # function for pasting annotations

    paste_anno <- function(myTx, k) {
      w <- get_anno(myTx)
      w <- paste0(sapply(1:length(w), function(i) w[[i]][k]), collapse = " ")
      return(w)

    }

    # add columns
    l <- length(kwic)
    kwic[, (l+1):(l+la)] <- NA
    colnames(kwic)[(l+1):(l+la)] <- paste0("Tag", 1:la, "_Key", sep = "")

    # status update
    if(verbose) {
      cat("Processing tags in the keyword column ... \n")
    }

    # fill columns
    for(j in 1:la) {
      kwic[, l+j] <- sapply(1:nrow(kwic), function(i) paste_anno(kwic$Key[i], j))
    }


    # strip tags from Key column
    kwic$Key <- kwic$Tag1_Key

  }


  # if there are tags, make columns with tags
  # and strip them from the original columns

  if(context_tags) {

    # create _with_anno columns
    kwic$Left_with_anno  <- kwic$Left
    kwic$Right_with_anno <- kwic$Right

    # how many annotation tags?
    w <- get_anno(kwic$Left[1])
    la <- max(sapply(1:length(w), function(i) length(w[[i]])))

    # add columns: left
    l <- length(kwic)
    kwic[, (l+1):(l+la)] <- NA
    colnames(kwic)[(l+1):(l+la)] <- paste0("Tag", 1:la, "_Left", sep = "")

    # fill columns: left
    for(j in 1:(la)) {
      kwic[, l+j] <- sapply(1:nrow(kwic), function(i) paste_anno(kwic$Left[i], j))
    }

    # strip tags from Left column
    kwic$Left <- kwic$Tag1_Left


    # add columns: right
    l <- length(kwic)
    kwic[, (l+1):(l+la)] <- NA
    colnames(kwic)[(l+1):(l+la)] <- paste0("Tag", 1:la, "_Right", sep = "")

    # status update
    if(verbose) {
      cat("processing tags in the context columns ... \n")
    }

    # fill columns: right
    for(j in 1:la) {
      kwic[, l+j] <- sapply(1:nrow(kwic), function(i) paste_anno(kwic$Right[i], j))
    }

    # strip tags from Left column
    kwic$Right <- kwic$Tag2_Key


    # reorder columns
    lf  <- grep("Tag.*_Left", colnames(kwic))
    ky  <- grep("Tag.*_Key", colnames(kwic))
    rgt <- grep("Tag.*_Right", colnames(kwic))
    lwa <- which(colnames(kwic)=="Left_with_anno")
    rwa <- which(colnames(kwic)=="Right_with_anno")
    kwa <- which(colnames(kwic)=="Key_with_anno")

    kwic <- kwic[,c(1:(min(c(lf, ky, rgt, lwa, rwa, kwa))-1), lwa, kwa, rwa, lf, ky, rgt)]

    if(verbose) {
      cat("\n ... done. \n")
    }







  }

  return(kwic)


}

