#' @export getCWB
#' @import data.table
#' @title Read in concordances created with the CWB Corpus Query Processor (CQP).
#' @description This function reads in export files created with the Corpus Query Processor (CQP) of the Corpus Workbench (CWB) as dataframes. Alternatively, it offers the option to convert them to tab-separated text files on the fly.
#' @param filename The name of the file you read in.
#' @param dt If TRUE (the default), the results will be returned as a data.table (see ?'data.table-package' for further information). If FALSE, they will be returned as a standard data frame.
#' @examples
#' ~~ getCWB(myfile) # do not run


getCWB <- function(filename, dt = TRUE) {
  # read file
  tx <- fread(filename, fill = T, sep="\n", encoding = "UTF-8", header = F, stringsAsFactors = F, quote = "")

  # replace non-valid UTF-8
  tx[, V1 := sapply(tx[,1], function(m) iconv(m, "UTF-8", "UTF-8", sub=''))]

  # check if there are any metadata (using sample of firt 20 lines)
  n <- ifelse(nrow(tx)>=20, 20, nrow(tx))
  if(length(grep(">:", tx[1:n,V1]))>=n) {
    # find lines with multiple ": <" (i.e. a : before the keyword) to avoid errors
    rpl <- grep("\\: \\<(?=.*\\: \\<)", tx[,V1], perl = T)

    if(length(rpl>0)) {
      for(r in rpl) {
        tx[r, V1 := paste(paste(.splitter(tx[r,V1], ": <")[1:2], collapse=": <", sep=""),
                          .splitter(tx[r,V1], ": <")[3], sep = "INSERTCOLONHERE <", collapse = "")]
      }
    }


    # replace < and >
    tx[,V1 := gsub(": <|><|>:", "SPLITHERE", as.character(V1))]
    x <- data.table::setDT(data.table::tstrsplit(as.character(tx[,V1]), "SPLITHERE", fixed = T))

    # get metadata names
    mn <- as.character(x[1,])
    mn <- sapply(2:(length(mn)-1), function(i) .splitter(mn[i], " ")[1])

    # add metadata names
    colnames(x) <- c("No", mn, "Text")

    # omit metadata names from metadata values
    for(k in 1:length(mn)) {
      x[, mn[k] := trimws(gsub(mn[k], "", as.character(as.matrix(x[,mn[k], with = FALSE]))))]
    }


  } else {

    x <- tx

    # numbers
    x[, No := gsub(":.*", "", V1)]
    x[, V1 := trimws(gsub("^[0-9]*:", "", V1))]
    data.table::setcolorder(x, c("No", "V1"))
    data.table::setnames(x, "V1", "Text")
  }

  # check if left and right context are present
  if(length(grep("^<|>$", x[,Text]))<nrow(x)) {

    cat("splitting concordance up into columns...")

    # split up into Left, Right, Key
    x[, Left := trimws(gsub("<.*", "", Text))]
    x[, Key := trimws(gsub(".*<|>.*", "", Text))]
    x[, Right := trimws(gsub(".*>", "", Text))]
    x[, Text := NULL]
  } else {
    x[, Text := trimws(gsub("<|>", "", Text))]
    setnames(x, "Text", "Key")
  }


  # get tags from Key column

  # find out number of tags
  for(i in 1:n) {
    if(i==1) some_tokens <- list(n)
    some_tokens[[i]] <- .splitter(x[i,Key], " ")
  }

  # get length of / that are not at the beginning
  l <- unlist(sapply(1:n,
                     function(j) sapply(1:length(some_tokens[[j]]),
                                        function(i) length(unlist(strsplit(some_tokens[[j]][i],
                                                                           "(?<!^)/", perl = T))))))

  # print warning if tokens differ in the number of tags
  if(length(unique(as.vector(l)))>1) {
    warning("Number of tags may be inaccurate. Please check if slashes (/) occur in annotations, e.g. by searching for pos=\"/\" in CQP")
  }

  # maximum of l becomes number of columns
  l <- max(l)

  if(l > 1) {
    # add new columns to data table
    for(i in 1:l) {
      x[, paste("tag", formatC(i, digits = 3, flag = "0"), sep="") := character(.N)]
    }

    # warning message
    if(nrow(x)>10000) {
      cat("Collecting tags. This can take a while, please be patient.")
    }

    # create progress bar
    pb <- utils::txtProgressBar(min = 1, max = nrow(x), style = 3)

    # fill the new columns
    for(j in 1:nrow(x)) {
      utils::setTxtProgressBar(pb, j)
      for(t in 1:length(grep("tag", colnames(x)))) {
        helper <- lapply(1:l, function(z) paste(lapply(.splitter(x[j,Key], " "),
                                                       function(i) unlist(strsplit(i, "(?<!^)/", perl=T))[z]), collapse=" "))[t]

        x[j, paste("tag", formatC(t, digits = 3, flag = "0"), sep="") := helper]
      }



    }

    # close progress bar
    close(pb)

    # print better-grab-a-coffee message for large files
    if(nrow(x)>10000) {
      cat("Stripping tags. This can take a while, please be patient.")
    }

    # strip tags from left and right context, if present
    if(is.element("Left", colnames(x))) {
      x[, Left := sapply(1:nrow(x), function(i) gsub("SSSLASSSH", "/", gsub("(/)(.*?)(?= )|/.*$", "", gsub(" //", " SSSLASSSH/", x[i,Left]), perl=T)))]
    }

    if(is.element("Right", colnames(x))) {
      x[, Right := sapply(1:nrow(x), function(i) gsub("SSSLASSSH", "/", gsub("(/)(.*?)(?= )|/.*$", "", gsub(" //", " SSSLASSSH/", x[i,Right]), perl=T)))]
    }

    # update column names
    colnames(x)[which(colnames(x)=="Key")] <- "Key_with_anno"
    colnames(x)[which(colnames(x)=="tag0001")] <- "Key"

    # update column order
    new_order <- gsub("Keyword", "Key", gsub("Key$", "Key_with_anno", gsub("Key_with_anno", "Keyword", colnames(x))))
    data.table::setcolorder(x, new_order)

    # replace INSERTCOLONHERE
    if(length(which(colnames(x)=="Left"))>0) {
      x[, Left := gsub("INSERTCOLONHERE", ":", Left)]
    }



  }

  if(!dt) {
    return(as.data.frame(x))
  } else {
    return(x)
  }

}
