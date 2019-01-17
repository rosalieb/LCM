pkgTest <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkgTest("lubridate") # Handle different date time format
# https://stackoverflow.com/questions/25463523/convert-variable-with-mixed-date-formats-to-one-format

pkgTest("vegan");pkgTest("ade4") # PCA

pkgTest("reshape2") # To 'unmelt' data
