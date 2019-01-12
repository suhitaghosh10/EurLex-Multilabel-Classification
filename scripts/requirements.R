## The script installs the necessary packages if not already installed, and then loads them

packages <- c(
  "textstem",
  "stringi",
  "dplyr",
  "data.table",
  "tm",
  "textclean",
  "XML",
  "RWeka",
  "tidyr",
  "tidytext",
  "parallel",
  "mldr",
  "utiml",
  "tidytext",
  "ggplot2",
  "RColorBrewer",
  "udpipe",
  "kknn",
  "xgboost",
  "randomForest",
  "matrixStats",
  "rmarkdown",
  "wordcloud",
  "ggraph",
  "igraph",
  "tidyverse",
  "gridExtra",
  "kableExtra",
  "knitr"
)

verify.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = 
                       TRUE)
  sapply(pkg, library, character.only = TRUE)
}

verify.packages(packages)
