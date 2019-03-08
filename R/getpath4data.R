#function which user
getpath4data <- function() {
  if(Sys.getenv("USER")=="Rosalie") return("/Volumes/-/Script R/LCM_GitHub_Data_LCM/")
  if(Sys.getenv("USER")=="alexnaccarato") return("~/Desktop/Food-Web 2018-2019/LCM_GitHub_Data/")
  if(Sys.getenv("USER")!="Rosalie"|Sys.getenv("USER")!="alexnaccarato") stop("You need to download the data from the VTDEC website and place them in a folder we can reach")
}

myconstant <- function() {
  if(Sys.getenv("USER")=="Rosalie") return(16)
  if(Sys.getenv("USER")=="alexnaccarato") return(21)
  if(Sys.getenv("USER")!="Rosalie"|Sys.getenv("USER")!="alexnaccarato") stop("You need to download the data from the VTDEC website and place them in a folder we can reach")
}

