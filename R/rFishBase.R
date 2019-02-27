#rFishBase
#http://www.seascapemodels.org/rstats/2017/03/23/summarize-by-genera.html 

library(rfishbase)
library(dplyr)

#load Lake Champlain species, as in Marsden and Langdon 2012
fishlc <- read.delim(paste0(getpath4data(),"fish_in_LC.txt"))

dim(fishbase)

head(fishbase)

diet(species_list = c("Oreochromis niloticus"))
diet(species_list = c("Oncorhynchus mykiss"))
diet(species_list = c("Acipenser fulvescens"))

predators(species_list = c("Oncorhynchus mykiss"))
ecosystem(species_list = c("Oncorhynchus mykiss"))
length_length(species_list = c("Oreochromis niloticus"))
grep("Mali",  brains()[,"Locality"], ignore.case = T)
View(brains())
brains()[brains()[,"Locality"]=="Mali",]
brains()[brains()[,"Locality"]==NA,]
d4 <- as.data.frame(brains()[brains()[,"Locality"]=="France",])

groupers <- fishbase %>% filter(Family == "Salmonidae") %>%
  mutate(gensp = paste(Genus, Species))
dim(groupers)
head(groupers)

grptroph <- ecology(groupers$gensp, fields = c("DietTroph"))
nrow(grptroph)

head(grptroph)

d2 <- left_join(groupers, grptroph)
nrow(d2)

d3 <- d2 %>% group_by(Genus) %>%
  mutate(mntroph = mean(DietTroph, na.rm = T)) %>%
  ungroup() %>%
  mutate(trophall = ifelse(is.na(DietTroph), mntroph, DietTroph))

d3 %>% select(Genus, Species, DietTroph, trophall) %>%
  data.frame() %>% head(20)

d3 %>% filter(gensp == "Epinephelus malabaricus") %>% select(trophall)


# From paper Boettiger
