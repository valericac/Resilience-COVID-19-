######CRISIS 
# Merge
# 22.03.2022
# Valerie Hofmann

library(lavaan)
library(readr)
library(tidyverse)
library(cSEM)
library(OpenMx)
library(dplyr)
library(corrplot)
library(foreign)
library(psych)

#load data 
mood <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_fscores.txt")
worries <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Worries_fscores.txt")

mw <- merge(mood, worries, by=c('ID'))
colnames(mw) <- c("ID", "T_T1", "T_T2", "T_T3", "M_T1", "M_T2", "M_T3", "W_T1", "W_T2", "W_T3")
head(mw)

#write txt file with all relevant Info
write.csv(mw, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores.txt", row.names = FALSE, sep = ",")
#write.csv(mw, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_pca.txt", row.names = FALSE, sep = ",")

#check if it worked - load data
#dat <-  readr::read_csv(file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores.txt") 
dat <-  readr::read_csv(file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores.txt") 
dat <- as.data.frame(dat)
head(dat)


###PCA
mood_p <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_pca_score.txt")

