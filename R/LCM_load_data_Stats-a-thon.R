lcm <- read.delim(paste0(getpath4data(),"Lake_Champlain_long-term_monitoring.txt"), header=T,sep=",")
lcm2018 <- read.delim(paste0(getpath4data(),"Lake_Champlain_long-term_monitoring_2018.txt"), header=T, sep="\t")
lcm2018 <- read.delim(paste0(getpath4data(),"LakeMonitoringPoints2018/Lake_Champlain_long-term_monitoring_2018.txt"), header=T, sep="\t")

nrow(data.frame(test=unique(lcm$Test)))
nrow(unique(data.frame(test=lcm2018$Test)))

sum(colSums(!is.na(total[,-c(1,2)])))
ncol(total)


sum(!is.na(total$`Net phytoplankton, Cyanobacteria biovolume`))
boxplot(total$`Net phytoplankton, Cyanobacteria biovolume`)

total2 <- total
total2$Year 
write.table(total,file = "Lake_Champlain_long-term_monitoring_1992_2016.txt", row.names = F, sep=",")
head(total$VisitDate)
