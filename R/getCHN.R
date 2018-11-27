#' @export getCHN
#' @title Read in concordances from the Corpus Hedendaags Nederlands
#' @description This function allows for reading in concordances exported
#' from the free web interface for searching the Corpus Hedendaags Nederlands.
#' (You need a CLARIN account for using it, which many universities have.)
#' NOTE: There is currently no option to export the concordances themselves.
#' Just save the page with the query results in your browser and then read
#' in the HTML file.
#' @param filename The name of the file you read in. You can use file.choose() to choose a file interactively.


getCHN <- function(filename) {
  # read file
  f <- readLines(filename, warn = F)

  # find title rows
  t <- grep("<tr class=\"titlerow\">", f)
  t2 <- grep("^<a class=\"text-error\"", f) # finds the line that actually contains the reference
  t2 <- sapply(1:length(t),
               function(i)
                 t2[which(t2>t[i])][1]) # just in case there's more than 1 <a class="text error" etc.
  # after one of the <titlerow> tags

  # find concordance rows
  cr <- grep("<td class=\"tbl_conc_left\">", f)

  # function for splitting up concordance rows
  .get_conc <- function(l) {
    l <- trimws(gsub("<.*?>", "", unlist(strsplit(l, "</td>"))))
    l <- data.frame(Left =  l[1],
                    Key  =  l[2],
                    Right = l[3],
                    Lemma = l[4],
                    POS   = l[5],
                    stringsAsFactors = F)
    return(l)

  }


  # getting the sources
  titles <- data.frame(No    = t,
                       Title = gsub("<.*?>", "", f[t2]),
                       stringsAsFactors = F)
  titles$Year <- gsub(".*\\(|\\)", "", titles$Title)

  # create progress bar
  pb <- utils::txtProgressBar(min = 1, max = length(cr), style = 3)

  # get KWIC concordance
  for(i in 1:length(cr)) {
    utils::setTxtProgressBar(pb, i)
    c_cur <- .get_conc(f[cr[i]])
    t_cur <- max(which(t < cr[i]))
    t_cur <- t[t_cur]
    c_cur$Title <- titles[which(titles$No==t_cur),]$Title
    c_cur$Year  <- titles[which(titles$No==t_cur),]$Year

    if(i == 1) {
      c_all <- c_cur
    } else {
      c_all <- rbind(c_all, c_cur)
    }

  }

  close(pb)
  return(c_all)

}


