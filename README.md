
<!-- README.md is generated from README.Rmd. Please edit that file -->
concordances
============

Corpus export files often come in formats that require certain modifications if you want to import them into a spreadsheet program or if you want to read them into R as a data frame. The aim of *concordances* is to automatize this process. All you need is a corpus export file, and *concordances* will (try to) convert it for you. Currently it can handle export files from

-   the corpus workbench / CQP (should be largely compatible with CQPweb as well): **getCWB**
-   NoSketchEngine (in particular, the NSE implementation of the COW corpora): **getNSE**
-   the NoSketchEngine implementation of WaCkY: **getWACKY**
-   COSMAS2web, the online system for querying the German Reference Corpus (DeReKo): **getCOSMAS**. (getWACKY will sooner or later be merged with getNSE.)

getCWB depends on the package data.table, which speeds up handling of large files considerably. By default, getCWB therefore returns data.table objects, unless you set dt = FALSE, in which case it returns an ordinary R data frame. All other functions return R data frames.

Installation
------------

You can install *concordances* from github with:

``` r
# install.packages("devtools")
devtools::install_github("hartmast/concordances")
```

Usage
-----

The functions currently differ considerably in their arguments, the way they work, and also with regard to their reliability. I'll try to optimize them in the near future. In principle, however, all functions require only one obligatory argument: the path to the file that you want to read in.

``` r
library(concordances)
getCWB("path/to/file.txt") # do not run
```

If you want to open the resulting dataframes in a spreadsheet, e.g. for annotating them, you can easily export them using write.table:

``` r
myText <- getCWB("path/to/file.txt")
write.table(myText, "myText.txt", sep = "\t", row.names = F, quote = F, 
            fileEncoding = "UTF-8")
```

Caveats
-------

The format of corpus export files can change at any time, especially in the case of online services like COSMAS II. Please let me know if one of the functions doesn't work properly any more, I'll do my best to take care of it!
