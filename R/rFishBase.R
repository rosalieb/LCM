#rFishBase
#http://www.seascapemodels.org/rstats/2017/03/23/summarize-by-genera.html 

library(rfishbase)
library(dplyr)

dim(fishbase)

groupers <- fishbase %>% filter(Family == "Salmonidae") %>%
  mutate(gensp = paste(Genus, Species))
nrow(groupers)
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