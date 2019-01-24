lcm <- read.delim(paste(getwd(),"/Data/Lake_Champlain_long-term_monitoring.txt", sep=""), header=T,sep=",")
lcm2018 <- read.delim(paste(getwd(),"/Data/RE__minutes_from_the_Lake_Champlain_pre-Stats-a-thon_meeting/Lake_Champlain_long-term_monitoring_2018.txt", sep=""), header=T, sep="\t")

View(data.frame(test=unique(lcm$Test)))
View(unique(data.frame(test=lcm2018$Test)))
