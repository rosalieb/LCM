### Read database mdb
library(Hmisc)
source("R/getpath4data.R")
getpath4data()

mdb.get(paste0(getpath4data(), "LCM_bio_PeteStangel/Zoo_2013.mdb"), tables="PlanktonData")
#, tables=NULL, lowernames=FALSE, allow=NULL,
#        dateformat='%m/%d/%y', mdbexportArgs='-b strip')


library(RODBC)
conn <- odbcConnect(path.expand(paste0(getpath4data(), "LCM_bio_PeteStangel/Zoo_2013.mdb"))) 
subset(sqlTables(conn), TABLE_TYPE == "TABLE") 
df <- sqlFetch(conn, "Table1") 
close(conn)


library(ImportExport)
x<-access_import(file=paste0(getpath4data(), "LCM_bio_PeteStangel/Zoo_2013.mdb"),
                 )

