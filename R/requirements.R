## The script installs the necessary packages if not already installed, and then loadd them
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
  "parallel",
  "mldr",
  "utiml",
  "ggplot2",
  "RColorBrewer",
  "udpipe"
)


verify.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, library, character.only = TRUE)
}


verify.packages(packages)
