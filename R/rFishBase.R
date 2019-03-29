#rFishBase
#http://www.seascapemodels.org/rstats/2017/03/23/summarize-by-genera.html 

library(rfishbase)
library(dplyr)

getTrophicLevel(fish.data[1:10])

#load Lake Champlain species, as in Marsden and Langdon 2012
fishlc <- read.delim(paste0(getpath4data(),"fish_in_LC.txt"))
head(fishlc)

dim(fishbase)

head(fishbase)
sum(duplicated(fishbase$SpecCode))#All codes are unique
plot(fishbase$SpecCode, pch=20)
sum(fishbase$SpecCode>40000)
sum(fishbase$SpecCode>20000 & fishbase$SpecCode<40000)
sum(fishbase$SpecCode<20000)

predatorslc <- predators(species_list = fishlc$species)
dietlc <- diet(species_list = fishlc$species)
fishlc$species[!fishlc$species %in% dietlc$Species]

library(vioplot)
vioplot(dietlc$Troph)
vioplot(predatorslc$PredatTroph)
vioplot(predatorslc$PredatTroph)
dietlc[dietlc$Species=="Salvelinus namaycush",]
yperch <- dietlc[dietlc$Species=="Perca flavescens",]
yperch <- dietlc[,complete.cases(dietlc)]

vioplot(c_code()$LatDec)
vioplot(c_code()$LongDec)

sum(!is.na(c_code()$LatDec), na.rm = TRUE)
sum(!is.na(c_code()$LongDec), na.rm = TRUE)
sum(!is.na(c_code()$country), na.rm = TRUE) # all country are available
sum(c_code()$country=="USA"|c_code()$country=="Canada")
sum(c_code()$country=="USA")

getTrophicLevel(fishlc$species[1:10], as_table=TRUE)

diet(species_list = c("Oreochromis niloticus"))
diet(species_list = c("Oncorhynchus mykiss"))
diet(species_list = c("Acipenser fulvescens"))

predators(species_list = c("Oncorhynchus mykiss"))
ecosystem(species_list = c("Oncorhynchus mykiss"))
length_length(species_list = c("Oreochromis niloticus"))
grep("Mali",  brains()[,"Locality"], ignore.case = T)
View(brains())
brains()[brains()[,"Locality"]=="Mali",]
brains()[is.na(brains()[,"Locality"]),]
d4 <- as.data.frame(brains()[brains()[,"Locality"]=="France",])

groupers <- fishbase %>% filter(Family == "Salmonidae") %>%
  mutate(gensp = paste(Genus, Species))
dim(groupers)
head(groupers)

grptroph <- ecology(groupers$gensp, fields = c("DietTroph"))
nrow(grptroph)

head(grptroph[!is.na(grptroph),])
summary(grptroph)
summary(grptroph[!is.na(grptroph),])

d2 <- left_join(groupers, grptroph)
nrow(d2)

d3 <- d2 %>% group_by(Genus) %>%
  mutate(mntroph = mean(DietTroph, na.rm = T)) %>%
  ungroup() %>%
  mutate(trophall = ifelse(is.na(DietTroph), mntroph, DietTroph))

d3 %>% select(Genus, Species, DietTroph, trophall) %>%
  data.frame() %>% head(20)

d3 %>% filter(gensp == "Epinephelus malabaricus") %>% select(trophall)


# R code for fitting the von Bertanffy ####
# Fish Base
# https://www.fishbase.in/fishonline/english/FOL_FishBasegoesFishBayes.htm#7.2.2.1.
###Setting your working directory
##Loading available K and Loo estimates from studies included in Fishbase
FishBaseData<-read.table(paste0(getpath4data(),"FishBaseMullusBarbatus.txt"),header=T)
FishBaseData
##Entering mean length at age data from Sieli et al. 2011
Age<-c(1,2,3,4,5,6,7)
Length<-c(10.93,12.1,15.23,17.24,18.36,20.18,19.77)
###### METHOD 1##############
##Fitting the von Bertalanffy growth model with least square non linear regression
##Setting the von Bertalanffy growth model equation
vonBertalanffy<-Length~Loo*(1-exp(-K*(Age-To)))
###Setting initial values for the 3 parameters of the model
InitLoo<-max(Length)
InitK<-mean(FishBaseData$K)
InitTo<-0
##Running the non linear regression model
NLRBertalanffy<-nls(vonBertalanffy,data=list(Age=Age,Length=Length),start=list(Loo=InitLoo,K=InitK,To=InitTo))
# ######METHOD 2 ############
# ##Fitting the von Bertalanffy growth model with Bayesian inference using previous knowledge
# ##Fitting normal distributions to available FishBase data for using them as priors.
# # Log-normal distributions could also be fitted, but let’s stick with the simpler normal distribution for better comprehension.
# library(MASS)
# PriorLoo<-fitdistr(FishBaseData$Loo,"normal")
# PriorK<-fitdistr(FishBaseData$K,"normal")
# ###Printing the fitted distributions 
# PriorLoo 
# PriorK 
# library(R2jags) 
# runif(1) 
# #######Writing the Model in Jags notation
# Model = "model { for( i in 1 :Nobs){ Length[i] ~ dnorm(mu[i],tau) mu[i]<-Loo*(1-exp(-K*(Age[i]-To))) } ####Setting our informative priors for K and Loo as estimated above##### Loo ~ dnorm(24.9,0.055) K ~ dnorm (0.368,22.3) ####Using an uniformative prior for To and variability at the individual level##### tau ~ dgamma(0.001,0.001) To ~ dnorm (0,0.0001)}"
# #Saving the model file in the working directory#
# cat(Model, file="Model.bug") 
# ####Defining data##### 
# DataJ <- list(Age=Age,Length=Length,Nobs=length(Age)) 
# ##Running the model in Jags####### 
# Jags <- jags(model.file="Model.bug", working.directory=NULL, data=DataJ, parameters.to.save=c("K","Loo","To"), n.chains=3,n.thin=10, n.iter=20000, n.burnin=10000) 
# #####Getting results from the jags model###### 
# Jags$BUGSoutput$mean$K 
# #####Getting results from the non linear regression
# summary(NLRBertalanffy) 
# #####Plotting our results
# K_NL<-coef(NLRBertalanffy)[2] 
# K_Jags<-Jags$BUGSoutput$mean$K 
# Loo_NL<-coef(NLRBertalanffy)[1] 
# Loo_Jags<-Jags$BUGSoutput$mean$Loo 
# sd_K_NL<-summary(NLRBertalanffy)$coefficients[2,2] 
# sd_K_Jags<-Jags$BUGSoutput$sd$K 
# sd_Loo_NL<-summary(NLRBertalanffy)$coefficients[1,2] 
# sd_Loo_Jags<-Jags$BUGSoutput$sd$Loo 
# a1<-Loo_NL-3*sd_Loo_NL 
# a2<-Loo_NL+3*sd_Loo_NL 
# b1<-K_NL-3*sd_K_NL 
# b2<-K_NL+3*sd_K_NL 
# xK<-seq(b1,b2,length.out=200) 
# xLoo<-seq(a1,a2,length.out=200) 
# par(mfrow=c(1,2)) 
# ######Plotting posterior distribution of K from Jags
# plot(xK,dnorm(xK,mean=K_Jags,sd=sd_K_Jags)/100,type="l",lwd=1,xlab="K",ylab="") 
# ######Adding distribution of K from non linear regression
# lines(xK,dnorm(xK,K_NL,sd_K_NL)/100,lty=3) 
# ######Plotting posterior distribution of Loo from Jags
# plot(xLoo,dnorm(xLoo,Loo_Jags,sd_Loo_Jags),type="l",lwd=1,xlab="Loo",ylab="") 
# ######Adding distribution of K from non linear regression
# lines(xLoo,dnorm(xLoo,Loo_NL,sd_Loo_NL),lty=3) 
# jpeg("PlotNLvsBayesianGrowth.jpeg") 
# par(mfrow=c(1,2)) 
# ######Plotting posterior distribution of K from Jags
# plot(xK,dnorm(xK,mean=K_Jags,sd=sd_K_Jags)/100,type="l",lwd=1,xlab="K",ylab="") 
# ######Adding distribution of K from non linear regression
# lines(xK,dnorm(xK,K_NL,sd_K_NL)/100,lty=3) 
# ######Plotting posterior distribution of Loo from Jags
# plot(xLoo,dnorm(xLoo,Loo_Jags,sd_Loo_Jags),type="l",lwd=1,xlab="Loo",ylab="") 
# ######Adding distribution of K from non linear regression
# lines(xLoo,dnorm(xLoo,Loo_NL,sd_Loo_NL),lty=3) 
# dev.off()
