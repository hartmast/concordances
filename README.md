
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/hartmast/concordances.svg?branch=master)](https://travis-ci.org/hartmast/concordances) [![codecov](https://codecov.io/gh/hartmast/concordances/branch/master/graph/badge.svg)](https://codecov.io/gh/hartmast/concordances)

concordances
============

Corpus export files often come in formats that require certain modifications if you want to import them into a spreadsheet program or if you want to read them into R as a data frame. The aim of *concordances* is to automatize this process. All you need is a corpus export file, and *concordances* will (try to) convert it for you. Currently it can handle export files from

-   the corpus workbench / CQP (if you use CQPweb, you don't need the package: just use CQPweb's KWIC export function): **getCWB**
-   NoSketchEngine (in particular, the NSE implementation of the COW and WaCkY corpora): **getNSE**
-   COSMAS2web, the online system for querying the German Reference Corpus (DeReKo): **getCOSMAS**,
-   the Corpus Hedendaags Nederlands (this one does not offer export files but you can just save the page with the query results in your browser and use the saved HTML file as input for the function): **getCHN**.
-   (DEPRECATED: getWACKY - for the NoSketchEngine implementation of the Wacky corpora. Use getNSE instead.)

In addition, the functions **last\_left** and **first\_right** provide the option to get the last n words from the left context and the first n words from the right context. The function **export** provides a convenient wrapper for write.table, exporting concordances as tab-separated UTF-8 files (without text qualifiers). In some cases, this is the most desirable option for KWIC concordance files as they can contain unmatched scarequotes, which can lead to parsing errors when using the typical CSV export settings. Tabs, by contrast, are rare (though not unheard of) and most of the functions in this package try to get rid of them.

getCWB depends on the package data.table, which speeds up handling of large files considerably. By default, getCWB therefore returns data.table objects, unless you set dt = FALSE, in which case it returns an ordinary R data frame. All other functions return R data frames.

Installation
------------

You can install *concordances* from github with:

``` r
if(!is.element("devtools", installed.packages())) {
  install.packages("devtools")
}

devtools::install_github("hartmast/concordances")
```

Usage
-----

The functions currently differ considerably in their arguments, the way they work, and also with regard to their reliability. I'll try to optimize them in the near future. In principle, however, all functions require only one obligatory argument: the path to the file that you want to read in.

``` r
library(concordances)
getCWB("path/to/file.txt") # do not run
```

Note that on Windows machines, you usually have to use double backslashes in file paths, e.g.

``` r
getCWB("path\\to\\file.txt") # do not run
```

If you want to open the resulting dataframes in a spreadsheet, e.g. for annotating them, you can easily export them using *export()* or *write.table()*:

``` r
# read in text
myText <- getCWB("path/to/file.txt")

# export text
export(myText)

# export(myText) is equivalent to:
write.table(myText, "myText.tsv", sep = "\t", row.names = F, quote = F, 
            fileEncoding = "UTF-8")
```

Caveats
-------

The format of corpus export files can change at any time, especially in the case of online services like COSMAS II. Please let me know if one of the functions doesn't work properly any more, I'll do my best to take care of it!
