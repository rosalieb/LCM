# Created on 2019-05-23 for LTdiet script - delete by 2019-06-23 if no other need ####
my_sizeclass <- c(seq(50,400,50),seq(500,600,100)) # a vector you may edit to create more size class
for (i in seq_along(my_sizeclass)) {
  if (i==1) { # everything smaller than the first value of the my_sizeclass vector
    LTdiet_summary <- colSums(LTdiet[which(LTdiet$Total.Length<=my_sizeclass[i]),21:38], na.rm=T)
    message(paste0(nrow(LTdiet[which(LTdiet$Total.Length<=my_sizeclass[i]),])," individuals are smaller than ",my_sizeclass[i], " mm"))
  }
  else { # everything larger than the last value of the my_sizeclass vector
    if (i == length(my_sizeclass)) { 
      LTdiet_summary <- rbind(LTdiet_summary, colSums(LTdiet[which(LTdiet$Total.Length>my_sizeclass[i]),21:38], na.rm=T))
      message(paste0(nrow(LTdiet[which(LTdiet$Total.Length>my_sizeclass[i]),])," individuals are larger than ",my_sizeclass[i], " mm"))
    }
    else     {# everything else
      LTdiet_summary <- rbind(LTdiet_summary, colSums(LTdiet[which(LTdiet$Total.Length>=my_sizeclass[i-1]&LTdiet$Total.Length<my_sizeclass[i]),21:38], na.rm=T))
      message(paste0(nrow(LTdiet[which(LTdiet$Total.Length>=my_sizeclass[i-1]&LTdiet$Total.Length<my_sizeclass[i]),]), " individuals are in the ", my_sizeclass[i-1], "-", my_sizeclass[i], " mm size-range"))
    }
  }
}

rownames(LTdiet_summary) <- my_sizeclass
### end 'my size class'

# If you had another piece of code here, label it and write when it's ok to delete it