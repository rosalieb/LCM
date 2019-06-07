library(ggplot2)

smelt <- read.csv(file = paste0(getpath4data(),"data_from_Ellen/MasterFileSmeltBio1984-2015-2-24-16.csv"))
smelt$Station <- gsub("-"," ",smelt$Station)
head(smelt)

mean(smelt$Weight / smelt$Length, na.rm = T)
smelt[(smelt$Weight / smelt$Length)<0.4,]

plot(smelt$Weight, smelt$Length, pch=20)

ggplot(aes(x=Length, y=Weight), data=smelt )+
  geom_point()+
  #geom_line() +
  geom_smooth(method="gam", formula = y ~ s(x))

ggplot(aes(y=Length, x=Weight), data=smelt )+
  geom_point()+
  #geom_line() +
  geom_smooth(method="gam", formula = y ~ s(x)) +
  facet_wrap(~Station)

#some values of condition >2, one at 49 - check that
ggplot(aes(y=condition, x=Year), data=smelt )+
  ylim(0,6)+
  geom_point()+
  #geom_line() +
  geom_smooth(method="gam", formula = y ~ s(x)) +
  facet_wrap(~Station)


fbb<-read.delim(paste0(getpath4data(),"/data_from_Ellen/Fish_Burlington_Bay_1999_2000.txt"))
class(fbb$rel_ab_Burl_Bay_1999_2000)
fbb$abundance=gsub("<","",fbb$rel_ab_Burl_Bay_1999_2000)
fbb$abundance <- as.numeric(paste(fbb$abundance))
fbb$abundance
dim(fbb)

fbb
library(rfishbase)
dietfbb <- diet(species_list = fbb$species)
dim(dietfbb)
fbb$species[!fbb$species %in% unique(dietfbb$Species)] # all species got diet assigned
fbb$species[fbb$species %in% unique(dietfbb$Species)]
fbb$Troph<-rep(NA,nrow(fbb))
fbb$seTroph <- fbb$Troph
for (i in 1:length(unique(dietfbb$Species))) {
  fbb$Troph[i] <- mean(dietfbb$Troph[dietfbb$Species==unique(dietfbb$Species)[i]],na.rm=T)
  fbb$seTroph[i] <- mean(dietfbb$seTroph[dietfbb$Species==unique(dietfbb$Species)[i]],na.rm=T)
}


