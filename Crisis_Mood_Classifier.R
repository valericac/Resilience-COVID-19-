#Crisis Mood
#Classifier based on Probs 
#25.03.2022
#Author: Valerie Hofmann

#Packages 
library(dplyr)
library(ggplot2)
library(psych)

#load data with 2 class probabilities 
crisis2 <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_2Classes.txt")
head(crisis2)

#prepare
crisis2[, 13:14] <- sapply(crisis2[, 13:14], as.factor)
crisis2[, 13:14] <- sapply(crisis2[, 13:14], as.numeric)

crisis2$class <-ifelse(crisis2$C1prob > crisis2$C2prob, '1',
                ifelse(crisis2$C1prob < crisis2$C2prob, '2', 'NO'))

table(crisis2$class)

#write new file
write.csv(crisis2, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_2Classes_ident.txt", row.names = FALSE)

#reshape 
crisis2_M <- as.data.frame(crisis2[, c(1,5:7,15)])
head(crisis2_M)

#reshape to long
crisis2l <- reshape(crisis2_M, idvar="ID", varying=2:4, timevar="Time",
                    sep="_", direction="long")
head(crisis2l)

#######
#Asign classes 3
#######

#load data with 3 classes 
crisis3 <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_3Classes.txt")
head(crisis3)

#prepare
crisis3[, 15:17] <- sapply(crisis3[, 15:17], as.factor)
crisis3[, 15:17] <- sapply(crisis3[, 15:17], as.numeric)

table(crisis3$class)

crisis3$class <-ifelse(crisis3$C1prob > crisis3$C2prob & crisis3$C1prob >
                         crisis3$C3prob, '1',
                       ifelse(crisis3$C1prob < crisis3$C2prob & crisis3$C2prob
                              > crisis3$C3prob, '2',
                              ifelse(crisis3$C2prob < crisis3$C3prob & 
                                       crisis3$C1prob < crisis3$C3prob,
                                     '3', 'NO')))

table(crisis3$class)

#save
write.csv(crisis3, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_3Classes_ident.txt", row.names = FALSE)

#reshape 
crisis3_M <- as.data.frame(crisis3[, c(1,5:7,16)])
head(crisis3_M)

#reshape to long
crisis3l <- reshape(crisis3_M, idvar="ID", varying=2:4, timevar="Time",
                    sep="_", direction="long")
#export 
write.csv(crisis3l, file="C:/Users/hofma/Desktop/LGC/02_data/
          Crisis_Mood_3C_long.txt", row.names = FALSE)

