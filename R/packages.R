pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("lubridate") # Handle different date time format
# also used to get the day number in a year, e.g. January 1st is day 1
# https://stackoverflow.com/questions/25463523/convert-variable-with-mixed-date-formats-to-one-format

pkgTest("vegan");pkgTest("ade4") # PCA

pkgTest("reshape2") # To 'unmelt' data

pkgTest("ggplot2")

source("R/getpath4data.R")
