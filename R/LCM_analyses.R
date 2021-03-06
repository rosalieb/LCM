# Load packages
# Give the path of a specific script I want to load
source(paste(getwd(),"/R/packages.R", sep=""))

# Attempt to read data from online databases ####
# Is there a way in R to download data from online databases? Any blogpost, stackoverflow discussion I missed? #rstats

# Load data ####
# There should be 15 monitoring points on the lake
(Sys.glob(paste0(getpath4data(),"LakeMonitoringPoints/","*.txt")))
filenames <- list.files(paste0(getpath4data(),"LakeMonitoringPoints"), pattern="*.txt", full.names=TRUE)
ldf <- lapply(filenames, read.csv, header=T, sep="\t")
res <- lapply(ldf, summary)
# The function below substr() will allow us to extract the number of the lake
#    monitoring site in order to rename our new dataframes
# The way data are formated in the folder, the site code is the two first characters
#    of the file name
# Because path changes for different users, we'll access it by getting the length
#    of the path, and then adding 2 (to get pass the change in folder to the first
#    character)
location_name <- nchar(paste0(getpath4data(),"LakeMonitoringPoints"))+myconstant()

names(res) <- paste0("LCM",substr(filenames, location_name, location_name+1))
for (i in 1:length(ldf))
  assign(paste("LCM",substr(filenames, location_name, location_name+1), sep="")[i], ldf[[i]]) # Create individual dataframes

# Clean data - the date-time format varies ####
for (i in 1:length(ldf)) {
  temporary <- ldf[[i]] 
  temporary$VisitDate <- parse_date_time(x = temporary$VisitDate, orders = c("m/d/y", "m/d/Y"))
  assign(paste("LCM",substr(filenames, location_name, location_name+1), sep="")[i], temporary)
  ldf[[i]] <- temporary
  rm(temporary)
}
names(ldf) <- names(res)

# Plot, e.g., phosphorus ####
mydata <- LCM50
plot(mydata$VisitDate[mydata$Test=="Total Phosphorus"], mydata$Result[mydata$Test=="Total Phosphorus"], type="l", ylim=c(0,200))

for (i in 1:length(ldf)) {
  temporary <- ldf[[i]] 
  lines(temporary$VisitDate[grep("Total Phosphorus", temporary$Test)], temporary$Result[grep("Total Phosphorus", temporary$Test)])
  rm(temporary)
}

# Plot mean variable per year ####
# Question you need to ask yourself: do we want an average, a sum? In most cases average makes
# sense, but sometimes (e.g. for precipitations), it is more relevant to look at the sum.

# Here, I wrote a loop that gives us the parameter we want at the end.
# You can add in this vector the different parameters, e.g., "Temperature".
# It has to match one of the parameter measured as part of the analysis
unique(LCM02$Test) # Check which parameters exist
myparameterslist <- c("Temperature", "Total Phosphorus", "Total Nitrogen", "Chlorophyll-a", "Net phytoplankton, total biovolume")
mydatalist <- names(res) # Here we could also select some sites

# Actual loop.
# It will create in your Output/Figures/2-Inter-annual_variations folder one pdf/site.
# Note that it could make sense to gather all the trends on one plot, let's discuss that later.

# But first, create a matrix (dataframe) we will automatically fill with out outputs.
# You can add output to this first vector, which_output
which_output <- c("min_year", "max_year", "min_value", "max_value", "mean","SD","R2")
output_trends <- matrix(rep(NA, length(myparameterslist)*length(mydatalist)*(length(which_output)+2)), ncol=length(which_output)+2)
colnames(output_trends) <- c("parameter","site",which_output)
# Add two first column with site+ parameter
for (p in seq_along(myparameterslist)) { # seq_along is just another way of writing 1:length(myparameterslist)
  myparameter <- myparameterslist[p]
  # I want to save in independent pdf files the different graphs.
  pdf(paste(getwd(),"/Output/Figures/2-Inter-annual_variations/",myparameter,".pdf",sep=""),width = 5, height = 4,family = "Helvetica")
  for (s in seq_along(mydatalist)) {
    mydata <- ldf[[which(sapply(names(res)==mydatalist[s],isTRUE))]]
    mydata <- aggregate(x = mydata$Result[mydata$Test==myparameter],
                        by = list(year = substr(mydata$VisitDate[mydata$Test==myparameter], 1, 4)),
                        FUN = mean)
    mydata$year <- as.numeric(mydata$year)
    if(myparameter=="Net phytoplankton, total biovolume") mydata$x <- mydata$x/1000000
    plot(mydata, type='l', ylab=myparameter,col=adjustcolor("black", alpha.f = .7),lwd=2)
    points(mydata,col=adjustcolor("black", alpha.f = 1), pch=20)
    abline(lm(x ~ year, data = mydata), col=adjustcolor("black", alpha.f = .3))
    text(x=max(mydata$year), y=min(mydata$x),
         labels = paste("R2= ",round(as.numeric(summary(lm(x ~ year, data =mydata))[8]),2),sep=""),
         cex=.9, pos = 2)
    mtext(mydatalist[s], side = 3, line = .5, adj = 0, font = 2)
    # Save output in matrix
    # The order must match the order determined in the which_output list
    output_trends[length(mydatalist)*(p-1)+(s),1] <- myparameterslist[p]
    output_trends[length(mydatalist)*(p-1)+(s),2] <- mydatalist[s]
    output_trends[length(mydatalist)*(p-1)+(s),3] <- mydata$year[mydata$x==min(mydata$x, na.rm=T)]
    output_trends[length(mydatalist)*(p-1)+(s),4] <- mydata$year[mydata$x==max(mydata$x, na.rm=T)]
    output_trends[length(mydatalist)*(p-1)+(s),5] <- min(mydata$x, na.rm=T)
    output_trends[length(mydatalist)*(p-1)+(s),6] <- max(mydata$x, na.rm=T)
    output_trends[length(mydatalist)*(p-1)+(s),7] <- mean(mydata$x, na.rm=T)
    output_trends[length(mydatalist)*(p-1)+(s),8] <- sd(mydata$x, na.rm=T)
    output_trends[length(mydatalist)*(p-1)+(s),9] <- round(as.numeric(summary(lm(x ~ year, data =mydata))[8]),2)
  }
  dev.off()
}
View(output_trends)

# Create one global data set ####
total <- NULL
for (i in 1:length(ldf)) {
  total <- rbind(total,ldf[[i]])
}
summary(total)

# transform the data to average per year
total_year <- total
total_year$VisitDate <- year(total_year$VisitDate )

# transform the datasets
total <- dcast(data = total,formula = VisitDate + StationID~Test,value.var = "Result",fun.aggregate = mean, na.rm = TRUE)
total_year <- dcast(data = total_year,formula = VisitDate + StationID~Test,value.var = "Result",fun.aggregate = mean, na.rm = TRUE)
dim(total)
head(total)
dim(total_year)
head(total_year)

# Want to create a column with the day number in the year
# e.g. January 1st is day 1
DOY <- yday(total$VisitDate) - 1
YEAR <- year(total$VisitDate)
plot(DOY[total$StationID==2],YEAR[total$StationID==2],
     pch=20, xlab="DOY", ylab="Year")
points(DOY[total$StationID==34],YEAR[total$StationID==34],pch=20,col="red")

# Some correlations
cor(total$`Total Phosphorus`,total$`Chlorophyll-a`, use = 'complete.obs')
cor(total$`Total Phosphorus`,total$`Dissolved Oxygen`, use = 'complete.obs')
cor(total$`Chlorophyll-a`,total$`Dissolved Oxygen`, use = 'complete.obs')

ggplot(data = total, aes(x=`Total Phosphorus`,y=`Chlorophyll-a`)) + geom_point() + stat_smooth(method=lm)
plot(total$`Total Phosphorus`, total$`Chlorophyll-a`, pch=20)

# PCA ####
# NAs cannot be handled by the PCA. 
# I'm replacing the NAs by the column average, so they won't get any weight in the analysis
total_PCA <- total_year
for (i in 3:ncol(total_PCA)) {
  total_PCA[is.na(total_PCA[,i]),i] <- mean(total_PCA[,i], na.rm=T)
}

acpR<-dudi.pca(total_PCA[,-c(1,2)])
5
summary(acpR)
s.corcircle(acpR$co, clab=0.8)
s.label(acpR$l1, clabel=0.6, label = paste(total_year$VisitDate,total_year$StationID))
s.arrow(acpR$co, add.p=TRUE)


total2 <- NULL
for (i in 1:length(ldf)) {
  temporary <- ldf[[i]] 
  head(temporary)
  temporary <- dcast(data = temporary,formula = VisitDate~Test,fun.aggregate = sum,value.var = "Result")
  temporary <- cbind("StationID" = rep(ldf[[i]]$StationID[1], nrow(temporary)),temporary)
  total2 <- merge(total2,temporary, by = intersect(names(total2), names(temporary)), all.y = T, all.x = T)
  rm(temporary)
}
summary(total2)
dim(total2)
total3 <- total2[ , colSums(is.na(total2)) == 0]

acpR<-dudi.pca(total3[,-c(1,2)])
5
summary(acpR)
s.corcircle(acpR$co, clab=0.8)
s.label(acpR$l1, clabel=0.6)
s.arrow(acpR$co, add.p=TRUE)


# Changes in habitats 
# pick dataset
mdata <- LCM21
mmonth <- 7
mdata <- mdata[month(mdata$VisitDate)==mmonth,]
dim(mdata)

# Remove all depth that are not numeric ==> find a solution later for this
mdata <- mdata[!mdata$Depth=="COM",]
mdata <- mdata[as.numeric(mdata$Depth)>0,]

# Keep only Temperature => find other solution later
mdata <- mdata[mdata$Test=="Temperature",]

samplingdate <- unique(mdata$VisitDate)
#for (i in 1:length(samplingdate))
plot(as.numeric(mdata$Depth[mdata$VisitDate==samplingdate[i]]), as.numeric(mdata$Result[mdata$VisitDate==samplingdate[i]]))
plot(mdata$VisitDate, as.numeric(mdata$Result), type="l")

str(mdata$Depth[mdata$VisitDate==samplingdate[i]])
tail(mdata[mdata$Test=="Temperature",],10)

# MFA ####
#### Create the data frame for MFA ####
total_MFA <- total_year[order(total_year$VisitDate, total_year$StationID),]
head(total_MFA)

GroupLakesF <- NULL
for (i in 1:length(GroupLakesnames)) {
  if (i==1) {
    GroupLakesF <- as.data.frame(get(GroupLakesnames[i]))
    rownames(GroupLakesF) <- GroupLakesF[,1]
    #LakeTemporary <-  get(lakes[i])[,which(as.vector(sapply(get(lakes[i]), function(x) sum(x != 0)))>nbsp)]
    colnames(GroupLakesF) <- paste(lakes[i],colnames(GroupLakesF), sep="_")
    GroupLakesF <- GroupLakesF[,-c(which(colnames(GroupLakesF)==paste(lakes[i],"Year", sep="_")))]
  } else {
    GroupLakesF_2 <- as.data.frame(get(GroupLakesnames[i]))
    rownames(GroupLakesF_2) <- GroupLakesF_2[,1]
    #LakeTemporary <-  get(lakes[i])[,which(as.vector(sapply(get(lakes[i]), function(x) sum(x != 0)))>nbsp)]
    colnames(GroupLakesF_2) <- paste(lakes[i],colnames(GroupLakesF_2), sep="_")
    GroupLakesF_2 <- GroupLakesF_2[,-c(which(colnames(GroupLakesF_2)==paste(lakes[i],"Year", sep="_")))]
    GroupLakesF <- merge(GroupLakesF, GroupLakesF_2, by="row.names", all=TRUE)
    rownames(GroupLakesF) <- GroupLakesF[,which(colnames(GroupLakesF)=="Row.names")]
    GroupLakesF <- GroupLakesF[,-c(which(colnames(GroupLakesF)=="Row.names"))]
  }
}

# GroupLakesF <- t(GroupLakesF)
# for(c in 1:ncol(GroupLakesF)){
#   GroupLakesF[is.na(GroupLakesF[,c]), c] <- mean(GroupLakesF[,c], na.rm = TRUE)
# }
# GroupLakesF <- t(GroupLakesF)
d <- as.data.frame(GroupLakesF)

if (grouping == "Y") {
  GroupLakesF <- GroupLakesF[,which(colMeans(GroupLakesF,na.rm = F)>0)]
}
#### MFA all lakes ####
colnames(GroupLakesF)
rownames(GroupLakesF)

dim(GroupLakesF)
GroupLakesF <- GroupLakesF[,which(colMeans(GroupLakesF,na.rm = F)>0)]
dim(GroupLakesF)

groups_MFA <- NULL
for (i in 1:length(lakes)) {
  groups_MFA <- c(groups_MFA,length(c(grep(lakes[i], colnames(GroupLakesF), value=TRUE))))
}
groups_MFA2 <- groups_MFA[which(groups_MFA>0)]

if (grouping == "Y") { #if by group
  resR <- MFA(GroupLakesF, group=groups_MFA2, name.group = lakes[which(groups_MFA>0)], graph = TRUE)
  resR_out[[r]] <- resR
}

MFAout_axis2[r,(lakes %in% names(resR$group$contrib[,2]))] <- resR$group$contrib[,2]
MFAout_axis1[r,(lakes %in% names(resR$group$contrib[,1]))] <- resR$group$contrib[,1]
MFAout_devepl_axis1 <- c(MFAout_devepl_axis1, resR$eig[1,2])
MFAout_devepl_axis2 <- c(MFAout_devepl_axis2,resR$eig[2,2])

