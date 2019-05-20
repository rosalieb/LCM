#plankton data
source("R/getpath4data.R")
library(reshape2)

plktn_pre2010  <- read.delim(paste0(getpath4data(),"LCM_bio_PeteStangel/Plankton data pre 2010.txt"))
plktn_post2010 <- read.delim(paste0(getpath4data(),"LCM_bio_PeteStangel/Plankton data after 2010.txt"))
sites_raw      <- read.delim(paste0(getpath4data(),"LCM_bio_PeteStangel/Plankton data stations.txt"))
sites          <- sites_raw[sites_raw$LocationID %in% as.factor(plktn_pre2010$LocationID),]

plktn <- rbind(plktn_pre2010,plktn_post2010)

plktn$VisitDate <- as.Date(plktn$PlanktonData.VisitDate, format="%d-%b-%y")
head(plktn)

names(plktn)
plktn$PlanktonType[plktn$PlanktonType=="phyto"] <- "Phyto"
summary(plktn$PlanktonType)

summary(plktn$ResultType[plktn$PlanktonType=="Phyto"])
summary(plktn$ResultType[plktn$PlanktonType=="Zoo"])
summary(plktn$SampleType[plktn$PlanktonType=="Zoo"])

# Reshape the dataframe with the type of plankton+name as column name
plktn$Type_SpeciesID <- paste(plktn$PlanktonType, plktn$SpeciesID, sep="_")
plktn2 <- dcast(plktn,  VisitDate + LocationID ~ Type_SpeciesID,value.var = "Result",fun.aggregate = sum, na.rm = TRUE )
head( plktn2)
dim(plktn2)
names(plktn2)

phyto <- plktn2[,c(1, 2, grep("Phyto_", names(plktn2)))]
zoo   <- plktn2[,c(1, 2, grep("Zoo_",   names(plktn2)))]

dim(zoo)
dim(phyto)

names(zoo)
rowSums(phyto[,-c(1:2)])

# Often, when we get 0 for zoo, we get value for phyto, and vice versa.
# no overlap of sampling? Are zoo and phyto samples taken on different days?
# There are only 69 days for which we got values both for zoo and phyto.
summary(which(rowSums(zoo[,-c(1,2)])!=0) %in% which(rowSums(phyto[,-c(1,2)])!=0))
summary(which(rowSums(phyto[,-c(1,2)])!=0) %in% which(rowSums(zoo[,-c(1,2)])!=0))

# Group the data by at least genus
names(zoo)
whatsleft <- 
  names(zoo[,-c(grep("aphni",     names(zoo)),
              grep("osmin",     names(zoo)),
              grep("epto",      names(zoo)),
              grep("otifer|Anuraeopsis|Ascomorpha|Asplanchna|Brachi|Collotheca|Conochilus|Cupelopagis|Euchlanis|Filinia|Kellicottia|Keratella|Lecane|Monostyla|Nothalca|Ploesoma|Polyarthra|Synchaeta|Trichocerca",    names(zoo)),
              grep("iaphanoso", names(zoo)),
              grep("yclop",     names(zoo)),
              grep("alanoid|Limnocalanus",   names(zoo)),
              grep("opepo|Epischura|Harpacticoid",names(zoo)))])
zoo$BOSMINA      <- rowSums(zoo[,grep("osmin",names(zoo))])     # small grazer
zoo$DAPHNIA      <- rowSums(zoo[,grep("aphni",names(zoo))])     # medium grazer
zoo$DIAPHANOSOMA <- rowSums(zoo[,grep("iaphanoso",names(zoo))]) # medium grazer
zoo$LEPTO        <- rowSums(zoo[,grep("epto",names(zoo))])      # predator
zoo$CYCLOP       <- rowSums(zoo[,grep("yclop",names(zoo))])     # copepod
zoo$CALANOID     <- rowSums(zoo[,grep("alanoid|Limnocalanus",names(zoo))])   # copepod
zoo$ROTIFER      <- rowSums(zoo[,grep("otifer|Anuraeopsis|Ascomorpha|Asplanchna|Brachi|Collotheca|Conochilus|Cupelopagis|Euchlanis|Filinia|Kellicottia|Keratella|Lecane|Monostyla|Nothalca|Ploesoma|Polyarthra|Synchaeta|Trichocerca",names(zoo))])    # small grazer
zoo$COPEPOD      <- rowSums(zoo[,grep("opepo|Epischura|Harpacticoid",names(zoo))])    # large grazer


# Group by very large groups (i.e. grazers, predators)
zoo$PREDATOR    <- rowSums(zoo[,grep("Polyphemus|LEPTO|Holopedium",names(zoo))])   
zoo$GRAZER_sm   <- rowSums(zoo[,grep("BOSMINA|Sida|Alon|Chyd|ROTIF|leurox|ladocer",names(zoo))])  
zoo$GRAZER_lr   <- rowSums(zoo[,grep("DAPHNIA|DIAPHA|CYCLOP|CALANO|opep|Eurycercus|aupl",names(zoo))])  
zoo$PARASITIC   <- zoo[,grep("Ergasilus",names(zoo))]
  
whatsleft[-c(grep("Polyphemus|LEPTO|Holopedium",whatsleft),
            grep("BOSMINA|Sida|Alon|Chyd|ROTIF|leurox|ladocer",whatsleft),
            grep("DAPHNIA|DIAPHA|CYCLOP|CALANO|COPEPOD|Eurycercus|aupl",whatsleft),
            grep("Ergasilus",whatsleft))]

plot(zoo$VisitDate,zoo$LEPTO, pch=20 )
points(zoo$VisitDate,zoo$DAPHNIA, pch=20, col=adjustcolor("blue", alpha.f = .3) )
points(zoo$VisitDate,zoo$BOSMINA, pch=20, col=adjustcolor("red", alpha.f = .3) )
points(zoo$VisitDate,zoo$PARASITIC, pch=20, col=adjustcolor("green", alpha.f = .3) )

plot(zoo$VisitDate,zoo$PARASITIC, pch=20)

summary(as.factor(plktn2$LocationID))

library(ggplot2)
ggplot(data = zoo, aes(x = VisitDate, y = PREDATOR)) + geom_point() +
  facet_wrap(~as.factor(zoo$LocationID)) + ylim(0,20000)

ggplot(data = zoo, aes(x = VisitDate, y = GRAZER_lr)) + geom_point() +
  facet_wrap(~as.factor(zoo$LocationID)) + ylim(0,100000)

ggplot(data = zoo, aes(x = VisitDate, y = GRAZER_sm)) + geom_point() +
  facet_wrap(~as.factor(zoo$LocationID)) + ylim(0,250000)

sites


# Add explanatory variables to my dataframe
zoo$LocationID2 <- sites[match(zoo$LocationID, sites$LocationID), "StationID"]
zoo$Year        <- as.numeric(format(as.Date(zoo$VisitDate, "%Y-%m-%d"),"%Y"))
zoo$Month        <- as.numeric(format(as.Date(zoo$VisitDate, "%Y-%m-%d"),"%m"))
zoo$yday        <- as.numeric(format(as.Date(zoo$VisitDate, "%Y-%m-%d"),"%j"))

# Model species
library(mgcv)
mod1 <- gamm(GRAZER_sm ~ s(Year, by=as.factor(LocationID2))+s(yday, by=as.factor(LocationID2)), data=zoo)
summary(mod1$gam)
plot(mod1$gam)

#mod2 <- gamm(GRAZER_sm ~ s(GRAZER_lr, by=as.factor(LocationID2)), data=zoo)
mod2 <- gamm(GRAZER_sm ~ s(GRAZER_lr), data=zoo)
mod2 <- gamm(GRAZER_sm ~ s(PREDATOR), data=zoo)
summary(mod2$gam)
plot(mod2$gam)

# Focus on main lake
zoo19 <- zoo[zoo$LocationID2==19,c("VisitDate", "LocationID2", "Year", "Month", "yday",
                                   "PREDATOR", "GRAZER_sm", "GRAZER_lr", "PARASITIC")]

ggplot(data = zoo19, aes(x = VisitDate, y = PARASITIC)) + geom_point()

mod2 <- lm(PREDATOR ~ Year*yday, data=zoo19)
summary(mod2)

plot(zoo19$VisitDate[zoo19$PREDATOR != 0],zoo19$PREDATOR[zoo19$PREDATOR != 0], pch=20, col=adjustcolor("black", alpha.f = .3) )
points(zoo19$VisitDate[zoo19$GRAZER_lr != 0],zoo19$GRAZER_lr[zoo19$GRAZER_lr != 0], pch=20, col=adjustcolor("blue", alpha.f = .3) )
points(zoo19$VisitDate[zoo19$GRAZER_sm != 0],zoo19$GRAZER_sm[zoo19$GRAZER_sm != 0], pch=20, col=adjustcolor("red", alpha.f = .3) )
points(zoo19$VisitDate[zoo19$PARASITIC != 0],zoo19$PARASITIC[zoo19$PARASITIC != 0], pch=20, col=adjustcolor("green", alpha.f = .3) )


head(plktn)
head(zoo19)

zoo19 <- zoo19[rowSums(zoo19[,c("PREDATOR", "GRAZER_sm", "GRAZER_lr", "PARASITIC")])>0,]
zoo19_month <- aggregate(zoo19[,c("PREDATOR", "GRAZER_sm", "GRAZER_lr", "PARASITIC")], list(format(zoo19$VisitDate, "%Y-%m")), mean)
print(zoo19_month)
zoo19_month$Year   <- as.numeric(substr(zoo19_month$Group.1,1,4))
zoo19_month$Month  <- as.numeric(substr(zoo19_month$Group.1,6,7))
zoo19_month_Predator <- dcast(zoo19_month, Year ~ Month, value.var = "PREDATOR", fun.aggregate = mean)
zoo19_month_Grazer_sm <- dcast(zoo19_month, Year ~ Month, value.var = "GRAZER_sm", fun.aggregate = mean)
zoo19_month_Grazer_lr <- dcast(zoo19_month, Year ~ Month, value.var = "GRAZER_lr", fun.aggregate = mean)
