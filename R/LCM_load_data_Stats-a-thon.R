##GREG: getpath4data() is a function I use with the student I work with that locate where we keep our data on our laptop.
# Mine is: "/Volumes/-/Script R/LCM_GitHub_Data_LCM/"
# So you should assign your own path to use this script.
# getpath4data() <- "/your path/"

## ddply because it's data frame to data frame
#ldply from list to data frame
#dlply from data frame to list
#llply from list to list
library(plyr)
#package changepoint for changepoint analysis
#method chosen: AMOC (at most one change) as we want to detect the thermocline
library(changepoint)
#I'm using reshape at the end because I'm not sure I can use ddply for the last step.
library(reshape2)
#to get julian days
library(lubridate)

# Data work ####
#Read data
dat <- read.csv(paste0(getpath4data(),"Lake_Champlain_long-term_monitoring_1992_2016.csv"), sep=";")
head(dat)
names(dat)

#Transform into list to work on parameters with multi-measurements
test <- dlply(dat, .(StationID, VisitDate, Test))#, function(x) median(x$Results))
head(test)
len<-sapply(test,nrow)
#E.g.,
test[len == 10][[1]]

#Just checking which parameters have been measured at multiple depths
replicates <- test[len > 4] #5 measurements or more
head(names(replicates))
te <- names(sapply(replicates,nrow))
#Most measurements at different depths are for temperature and oxygen
te[!seq_along(te) %in% grep("Temperature|Oxygen", te)]
#e.g., There's only 10 days with repeated Phosphorus measurements, between 1996 and 1999
replicates$`4.04/06/1997.Total Phosphorus`

test1 <- replicates$`4.02/08/2000.Temperature`
# For parameters that are replicated, we want to create rules on what to keep
# All documented in the loop, to the best of my extent at the moment
unique <- test[len == 1]
replicates <- test[len > 1]
for (i in 1:length(replicates)) {
  test1 <- replicates[[i]]
  label <- names(replicates[i])
  # A) Duplicates
  if(nrow(test1)==2) {
    # First case (1/2), there are several times when two measurements were carried out at the same depth - duplicates
    # If taken at the same depth, just do an average of the two values
    # If meaurements are taken at less than 2m difference, I'm also doing an average
    # There's lot of instance where depths documented are 2 and 1.8 for instance, they are basically the same sample
    # When only one out of two depths are given, it's impossible to know for sure whether we're looking at duplicates or different layers. Right now I'm calculating an average.
    if(test1$Depth[1]==test1$Depth[2] || "COM" %in% test1$Depth || any(nchar(paste(test1$Depth ))<1)  || abs(as.numeric(as.character(test1$Depth[1]))-as.numeric(as.character(test1$Depth[2])))<2) {
      test2 <- test1[1,]
      test2$Result <- mean(test1$Result, na.rm=T)
      test2$Depth <- "COM"
      test2$Stratum <- "duplicates"
      if(!identical(test1$Lab[1],test1$Lab[2])) test2$Lab <- "VT/NY"
      
      if(nrow(test2)>1) stop(paste0("problem in the loop to transform row ",i," of 'replicates' dataframe. parameter: ",label))
      # unique <- c(unique, test2) # Done!
      unique[[label]] <- test2 # Done!
    } else {
      # Second case (2/2), only two measurements, taken at two depths
      if(any(!is.na(as.numeric(as.character(test1$Depth))))) test2 <- test1[order(as.numeric(as.character(test1$Depth)), decreasing = F),]
      if(length(grep("E|H", test1$Stratum))>0)               test2 <- test1[order(test1$Stratum, decreasing = F),]
        
      # Use the index E or H for Epi or Hypolimnion
      test2$Test <- paste(test2$Test,c("E","H"), sep="_")
      test2$Stratum <- c("E_infered","H_infered")
      
      test2 <- dlply(test2, .(StationID, VisitDate, Test))
      
      unique <- c(unique, test2) # Done!
    }
  } else {
    # B) More than two values
    # Here we have again two big categories:
    # (1/2) replicates for one or two depths 
    # (2/2) profile (e.g. for temperature)
    
    # 1) Replicates for one or two depths
    if(length(unique(test1$Depth))<=2) {
      # If depth is not COM or given, order the dataset
      options(warn=-1)
      if(any(!is.na(as.numeric(as.character(test1$Depth))))) test1 <- test1[order(as.numeric(as.character(test1$Depth)), decreasing = F),]
      options(warn=0)
      test2 <- test1[1,]
      test2$Result <- mean(test1$Result[test1$Depth==unique(test1$Depth)[1]], na.rm=T)
      test2$Depth <- "COM"
      test2$Stratum <- "U_replicates"
      if(length(unique(test1$Lab))>1) test2$Lab <- "VT/NY"
      
      if (length(unique(test1$Depth))==2) {
        test3 <- test1[1,]
        test3$Result <- mean(test1$Result[test1$Depth==unique(test1$Depth)[2]], na.rm=T)
        test3$Depth <- "COM"
        if(length(unique(test1$Lab))>1) test2$Lab <- "VT/NY"
        test2 <- rbind(test2, test3)
        test2$Test <-  paste(test2$Test,c("E","H"), sep="_")
        test2$Stratum <- c("E_replicates", "H_replicates")
      }
      test2 <- dlply(test2, .(StationID, VisitDate, Test))
      
      unique <- c(unique, test2) # Done!
    } else 
      # 2) With more than 3 depths, we'll assess whether there's a significant changepoint
      # If there's a changepoint: we'll create 2 values, otherwise just one.
    {
      options(warn=-1)
      if(any(!is.na(as.numeric(as.character(test1$Depth))))) test1 <- test1[order(as.numeric(as.character(test1$Depth)), decreasing = F),]
      options(warn=0)
      mcpt <- cpts(cpt.mean(test1$Result, method="AMOC", class=T, param.estimates=T))
      # No changepoints detected, mcpt=numeric(0)
      if(identical(mcpt, numeric(0))) {
        test2 <- test1[1,]
        test2$Result <- median(test1$Result, na.rm=T)
        test2$Depth <- "COM"
        test2$FieldID <- median(test1$FieldID, na.rm=T)
        test2$Stratum <- c("U_changepoints")
        
        if(nrow(test2)>1) stop(paste0("problem in the loop to transform row ",i," of 'replicates' dataframe. parameter: ",label))
        # unique <- c(unique, test2) # Done!
        unique[[label]] <- test2 # Done!
      } else
        # A changepoint was detected
        # Here we'll take the median value for the layer
      { 
        test2 <- test1[1:2,]
        test2$Depth <- "COM"
        if(length(unique(test1$Lab))>1) test2$Lab <- "VT/NY"
        
        test2$Result[1] <- median(test1$Result[1:mcpt], na.rm=T)
        test2$Result[2] <- median(test1$Result[(mcpt+1):nrow(test1)], na.rm=T)
        
        test2$FieldID[1] <- median(test1$FieldID[1:mcpt], na.rm=T)
        test2$FieldID[1] <- median(test1$FieldID[(mcpt+1):nrow(test1)], na.rm=T)
        
        test2$Test <-  paste(test2$Test,c("E","H"), sep="_")
        test2$Stratum <- c("E_changepoints", "H_changepoints")
        
        test2 <- dlply(test2, .(StationID, VisitDate, Test, Stratum))
        
        if(any(sapply(test2,nrow)>1)) stop(paste0("problem in the loop to transform row ",i," of 'replicates' dataframe. parameter: ",label))
        unique <- c(unique, test2) # Done!
      }
    }
  }
  if(sapply(unique[length(unique)], nrow)>1) stop(paste0("problem in the loop to transform row ",i," of 'replicates' dataframe. parameter: ",label))
  if (i==length(replicates)) cat("All done!\n")
}

length(unique)
len<-sapply(unique,nrow)
max(unlist(len))
plot(unlist(len))
# max should be 1 if it worked.
unique[[1]]
# do.call() is very slow, but using unlist() I was losing some properties of the table
unique_dat <- do.call(rbind.data.frame, unique)
# Save the data in that format
if(file.exists(paste0(getpath4data(),"LCM_unique_param_step1.txt"))){
  message("\nA file already exist in the folder. Do you want to replace it? (Y/N)")
  answer <- readline()
  if(answer=="Y") write.table(unique_dat, file = paste0(getpath4data(),"LCM_unique_param_step1.txt"), col.names = T, row.names = F, sep="\t")
} else {write.table(unique_dat, file = paste0(getpath4data(),"LCM_unique_param_step1.txt"), col.names = T, row.names = F, sep="\t")}

unique_dat$Result <- as.numeric(as.character(unique_dat$Result))

names(unique_dat)
unique_dat_vertical <- dcast(data = unique_dat,formula = StationID + VisitDate~Test,value.var = "Result",fun.aggregate = mean, na.rm = TRUE)
unique_dat_vertical <- unique_dat_vertical[ , c(1,2,order(names(unique_dat_vertical)[3:ncol(unique_dat_vertical)])+2)]
head(unique_dat_vertical)
names(unique_dat_vertical)
# We have up to 3 variables dpending on where the measurement was taken.
#e.g.,
head(unique_dat_vertical[,grep("Temperature", names(unique_dat_vertical))])
# Participants can always use grep() function if they want to use any variable with this format?

# Save the data in that format
if(file.exists(paste0(getpath4data(),"LCM_unique_param_step2.txt"))){
  message("\nA file already exist in the folder. Do you want to replace it? (Y/N)")
  answer <- readline()
  if(answer=="Y") write.table(unique_dat_vertical, file = paste0(getpath4data(),"LCM_unique_param_step2.txt"), col.names = T, row.names = F, sep="\t")
} else {write.table(unique_dat_vertical, file = paste0(getpath4data(),"LCM_unique_param_step2.txt"), col.names = T, row.names = F, sep="\t")}

# We have one issue left: when measurements have been taken in an interval or 2-4 days, we want to assume they are the same data.
# Create an additional column with year+julian day.
unique_dat_vertical$Day <- as.numeric(paste0(
  year(as.Date(unique_dat_vertical$VisitDate, format="%d/%m/%Y")),
  yday(as.Date(unique_dat_vertical$VisitDate, format="%d/%m/%Y"))))

unique(unique_dat_vertical$StationID)
# For some reason, I have on StationID saying "E_changepoints"
# Note: not an issue anymore when re-running the script on 2019-08-14
# I don't where it comes from at the moment, it correspond to the last row of the data frame
# Getting read of that one...
if(length(which(unique_dat_vertical$StationID=="E_changepoints"))>0)
  unique_dat_vertical <- unique_dat_vertical[-which(unique_dat_vertical$StationID=="E_changepoints"),]
unique_dat_vertical$StationID <- as.numeric(as.character(unique_dat_vertical$StationID))

# Back to doing an average if two measurements were taken close one to each other.
mythreshold <- 4 #setting here my threshold. If two measurements have been carried out within 4 days e.g. Monday and Thursday, or Friday and Monday, they are considered as the same date.

df <- unique_dat_vertical[order(unique_dat_vertical$StationID, decreasing = F),]
myvec=NULL
mysite=NULL
for (i in unique(df$StationID)) {
  test4 <- df[df$StationID==i,]
  test4 <- test4[order(test4$Day, decreasing = F),] 
  dif <- abs(diff(test4$Day))
  myvec <- c(myvec, dif)
  mysite <- c(mysite, rep(i, length(myvec)))
  which_dif <- which(dif<=mythreshold)
  test5 <- test4[-c(which_dif,which_dif+1),]
  for (j in seq_along(which_dif)) {
    temporary <- colMeans(test4[which_dif[j]:c(which_dif[j]+1),-c(1,2)], na.rm = T)
    # For the Day, I'm not doing an average. I'm using the first day (when sampling was initially planned)
    # I'm not doing that for any particular reason, just don't want to start doing some average of dates
    temporary <- cbind(test4[which_dif[j],c(1,2)], matrix(as.vector(temporary), nrow=1))
    names(temporary) <- names(test4)
    test5 <- rbind(test5, temporary)
  }
  test5 <- test5[order(test5$Day, decreasing = F),]
  cat(paste0(nrow(test4)-nrow(test5)," days (out of ",nrow(test4), ") were merged for station ", i,", using ",mythreshold," days as a threshold for merging.\n"))
  if (i==unique(df$StationID)[1]) df_output <- test5 else df_output <- rbind(df_output, test5)
}
myvec = myvec[myvec<50]  
mysite=mysite[myvec<50]  
abline(h=10)
plot(order(myvec), pch=mysite[order(myvec)])
nrow(df_output)
summary(df_output)

# Save the data in that format
if(file.exists(paste0(getpath4data(),"LCM_unique_param_step3.txt"))){
  message("\nA file already exist in the folder. Do you want to replace it? (Y/N)")
  answer <- readline()
  if(answer=="Y") write.table(df_output, file = paste0(getpath4data(),"LCM_unique_param_step3.txt"), col.names = T, row.names = F, sep="\t")
} else {write.table(df_output, file = paste0(getpath4data(),"LCM_unique_param_step3.txt"), col.names = T, row.names = F, sep="\t")}


# Assign the same value to E and H when no distinction is done ####
df_lc <- df_output
grep("_E",x = names(df_lc))
gsub("_E","", names(df_lc)[grep("_E",x = names(df_lc))])
for (i in 1:length(grep("_E",x = names(df_lc)))) {
  df_lc[is.na(df_lc[,grep("_E",x = names(df_lc))[i]]), grep("_E",x = names(df_lc))[i]] <- 
    df_lc[is.na(df_lc[,grep("_E",x = names(df_lc))[i]]), gsub("_E","", names(df_lc)[grep("_E",x = names(df_lc))[i]])]
}
for (i in 1:length(grep("_H",x = names(df_lc)))) {
  df_lc[is.na(df_lc[,grep("_H",x = names(df_lc))[i]]), grep("_H",x = names(df_lc))[i]] <- 
    df_lc[is.na(df_lc[,grep("_H",x = names(df_lc))[i]]), gsub("_H","", names(df_lc)[grep("_H",x = names(df_lc))[i]])]
  # remove the column with integrated value
  df_lc <- df_lc[,-which(names(df_lc)==gsub("_H","", names(df_lc)[grep("_H",x = names(df_lc))[i]]))]
}
dim(df_output);dim(df_lc)

#Save output
if(file.exists(paste0(getpath4data(),"LCM_unique_param_step4.txt"))){
  message("\nA file already exist in the folder. Do you want to replace it? (Y/N)")
  answer <- readline()
  if(answer=="Y") write.table(df_lc, file = paste0(getpath4data(),"LCM_unique_param_step4.txt"), col.names = T, row.names = F, sep="\t")
} else write.table(df_lc, file = paste0(getpath4data(),"LCM_unique_param_step5.txt"), col.names = T, row.names = F, sep="\t")

# Create a dataframe without all biological data  ####
df_lc2 <- df_lc
head(df_lc2)
bloom_thresh <- exp(5) # Set here our threshold for bloom
df_lc2$Bloom <- ifelse(is.na(df_lc2$`Net phytoplankton, Cyanobacteria biovolume`),
                      NA,ifelse(df_lc2$`Net phytoplankton, Cyanobacteria biovolume`>=bloom_thresh, 1,0))
summary(df_lc2$Bloom)
hist(log(df_lc2$`Net phytoplankton, Cyanobacteria biovolume`+.1), main="", xlab="log(Net phytoplankton, Cyanobacteria biovolume)")

df_lc_june <- df_lc2[as.numeric(substr(df_lc2$VisitDate,4,5))==6,]
likelihood_bloom_june <-
  as.data.frame(with(df_lc_june, tapply(Bloom,list("Year"=as.factor(substr(df_lc_june$VisitDate,7,10)), "StationID"=StationID), sum, na.rm=T)))/
  as.data.frame(with(df_lc_june, tapply(rep(1,nrow(df_lc_june)),list("Year"=as.factor(substr(df_lc_june$VisitDate,7,10)), "StationID"=StationID), sum, na.rm=T)))
likelihood_bloom_june <- likelihood_bloom_june[as.numeric(rownames(likelihood_bloom_june))>=2006,]  
barplot(colSums(likelihood_bloom_june)/nrow(likelihood_bloom_june))

names(df_lc2)
todelete <- grep("phytoplankton|Day|Chlorophyll-a", names(df_lc2))
if(length(todelete)>0) df_lc2 <- df_lc2[,-todelete]
dim(df_lc);dim(df_lc2)

# Save output
if(file.exists(paste0(getpath4data(),"LCM_unique_param_step5.txt"))){
  message("\nA file already exist in the folder. Do you want to replace it? (Y/N)")
  answer <- readline()
  if(answer=="Y") write.table(df_lc2, file = paste0(getpath4data(),"LCM_unique_param_step5.txt"), col.names = T, row.names = F, sep="\t")
} else write.table(df_lc2, file = paste0(getpath4data(),"LCM_unique_param_step5.txt"), col.names = T, row.names = F, sep="\t")



# Attemp model ####
temporary <- gsub("_E|_H","",names(df_lc))
for (i in 3:length(unique(temporary))) {
  if(i==3) myvec = NULL
  if(length(which(temporary %in% unique(temporary)[i]))>1) myvec <- c(myvec,which(rowSums(df_lc[, temporary==unique(temporary)[i]], na.rm = T)>0)) else 
    myvec <- c(myvec,which(!is.na(df_lc[, temporary==unique(temporary)[i]])))
}
myvec <- unique(myvec)
# There's always at least one data missing, so I don't really know how to create models with the same number of observation at the moment

library(nlme)
names(df_lc2) <- gsub(" ",".",names(df_lc2))
df_lc2$StationID <- as.factor(df_lc2$StationID)
df_lc2$Day <- as.numeric(paste0(year(as.Date(df_lc2$VisitDate,format="%d/%m/%Y")),yday(as.Date(df_lc2$VisitDate,format="%d/%m/%Y"))))

M0 <- gls(Bloom ~ Temperature_E, data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Bloom),])
summary(M0)
M1 <- gls(Bloom ~ Temperature_E + Total.Phosphorus_E, data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Bloom),])
summary(M1)
M2 <- gls(Bloom ~ Temperature_E + Total.Phosphorus_E + Total.Nitrogen_E, data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Total.Nitrogen_E)&!is.na(df_lc2$Bloom),])
summary(M2)
AIC(M0,M1,M2) # but models are not fitted to the same number of observations...

library(mgcv)
M3 <- gamm(Bloom ~ s(Temperature_E), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Bloom),])
summary(M3$lme)
M4 <- gamm(Bloom ~ s(Temperature_E) + s(Total.Phosphorus_E), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Bloom),])
summary(M4$lme)
par(mfrow=c(1,2));plot(M4$gam);par(mfrow=c(1,1))
M5 <- gamm(Bloom ~ s(Temperature_E) + s(Total.Phosphorus_E) + s(Total.Nitrogen_E), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Total.Nitrogen_E)&!is.na(df_lc2$Bloom),])
summary(M5$lme)
par(mfrow=c(1,3));plot(M5$gam);par(mfrow=c(1,1))
M6 <- gamm(Bloom ~ s(Temperature_E) + s(Total.Phosphorus_E) + s(Total.Nitrogen_E) + factor(StationID), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Total.Nitrogen_E)&!is.na(df_lc2$Bloom),])
summary(M6$lme)
par(mfrow=c(1,3));plot(M6$gam);par(mfrow=c(1,1))
# M7 <- gamm(Bloom ~ s(Temperature_E) + s(Total.Phosphorus_E) + s(Total.Nitrogen_E), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Total.Nitrogen_E)&!is.na(df_lc2$Bloom),],correlation = corAR1(form=~Day|StationID))
# summary(M7$lme)
# M8 <- gamm(Bloom ~ s(Temperature_E) + s(Total.Phosphorus_E) + s(Total.Nitrogen_E) + factor(StationID), data=df_lc2[!is.na(df_lc2$Temperature_E)&!is.na(df_lc2$Total.Phosphorus_E)&!is.na(df_lc2$Total.Nitrogen_E)&!is.na(df_lc2$Bloom),],correlation = corAR1(form=~Day))
# summary(M8$lme)

logLik(M0)
logLik(M1)
logLik(M2)
logLik(M3$lme)
logLik(M4$lme)
logLik(M5$lme)
logLik(M6$lme)

## Look at correlation between variables ####
cor(df_lc2$Total.Phosphorus_E, df_lc2$Total.Nitrogen_E, use = "complete.obs")
cor(df_lc2$Total.Phosphorus_E, df_lc2$Total.Nitrogen_E, use = "pairwise.complete.obs")
cor(df_lc2$Total.Phosphorus_E, df_lc2$Total.Nitrogen_E, use = "na.or.complete")
names(df_lc2)
r <- as.data.frame(matrix(rep(NA,(ncol(df_lc2)-3)^2), ncol = (ncol(df_lc2)-3)))
colnames(r) <- names(df_lc2)[3:(ncol(df_lc2)-1)]
rownames(r) <- names(df_lc2)[3:(ncol(df_lc2)-1)]
p <- r
n <- r

for (i in 3:(ncol(df_lc2)-1)) {
  for (j in 3:(ncol(df_lc2)-1)) {
    z <- !is.na(df_lc2[!is.na(df_lc2[,j]),i])
    z <- length(z[z==TRUE])
    n[j-2,i-2] <- z
    if(z>5) {
      res <- cor.test(df_lc2[,i], df_lc2[,j])#, use = "na.or.complete")
      r[j-2,i-2] <- res$estimate
      p[j-2,i-2] <- res$p.value
    }
  }
}
res <- list(r=r, P=p, n=n)

# Correlation matrix
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
library(Hmisc)
flattenCorrMatrix(res$r, res$P)

library(corrplot)
res$r2 <- res$r
res$r2[is.na(res$r2)] <- 0

# rename to inprove visibility
colnames(res$r2) <-c("Alk_E","Alk_H","Ca_E","Ca_H", "Chl_E","Chl_H","Cond_E","Cond_H","DIC_E", "DIC_H","DOC_E","DOC_H","DO_E","DO_H","DP_E","DP_H","DSi_E" ,
                     "DSi_H","Fe_E","Fe_H","Pb_E","Pb_H","Mg_E","Mg_H","OrtP_E","OrtP_H",  "pH_E", "pH_H","Pot_E","Pot_H","Secchi","Na_E",
                     "Na_H","T_E","T_H","NH3_E","NH3_H","TKN_E","TKN_H","NO23_E","NO23_H","N_E", "N_H","TOC_E","TOC_H","TP_E","TP_H","TSS_E","TSS_H","Bloom")
rownames(res$r2) <- colnames(res$r2)
p <- corrplot(as.matrix(res$r2), type="upper", order="hclust", tl.col="black", tl.srt=45)

# Try to do it interactively with ggplotly:
names(df_lc2)
X1 <- rep(4:ncol(df_lc2), times=(ncol(df_lc2)-3))
Y1 <- -rep(3:(ncol(df_lc2)-1), each=(ncol(df_lc2)-3))
correlation <- as.vector(t(df_lc2_correlation))
#color[is.na(color)] <- 0
dat <- data.frame(X1,Y1,correlation)
head(dat)

library(ggplot2)
library(plotly)
c <- dat %>%
  ggplot(aes(X1,Y1, fill = correlation)) + geom_tile() + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
c <- ggplotly(c)
c 



## check the average difference between two measurements days



