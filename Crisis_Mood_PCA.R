## Author: Valerie Hofmann
## Date: 27/02/2022
## CFAs 

library(lavaan)
library(readr)
library(tidyverse)
library(cSEM)
library(dplyr)
library(corrplot)
library(foreign)
library(psych)
install.packages("nFactors")
library(nFactors)

#load data 
crisis_75 <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/crisis_75_sep.csv", col_names = TRUE, col_types = cols(ID = "c"))
colnames(crisis_75)

#load 3 M prior 
crisis_75 <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/02_data/crisis_75_sep.csv", col_names = TRUE, col_types = cols(ID = "c"))
colnames(crisis_75)

#### MOOD FScores 
m3_raw <- readr::read_csv2("/Users/hofma/Desktop/LGC/02_data/Crisis_BL_3M.csv", col_names = TRUE, col_types = cols(ID = "c"))
m3 <- merge(m3_raw, crisis_75, by="ID")
tm3 <-  m3[,3:13]
colnames(tm3)
tm3 <- sapply(tm3, as.numeric)

cormatm3 <- cor(tm3)
round(cormatm3, digits = 2)
#usally seen as problematic if there are little correlations above .30 or higher or may correltations above .90, or if single or various items show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(cormatm3, n=282) 

#Is there a pattern in the correltaion matrix that can be detected via a factor analysis? KMO[]
KMO(tm3)
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(cormatm3) 

#function
c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

#Check for number of components in case of a PCA 
tm3pca <- prcomp(tm3, scale = TRUE, retx=TRUE)
summary(tm3pca)

#Screeplot Items
screeplot(tm3pca, type='lines', col='blue')

#
centeredata_tm3 <- apply(tm3, MARGIN=1, FUN=c.fun, tm3pca$center, tm3pca$scale)

# rotation matrix
center_tm3 <- t(tm3pca$rotation) %*% centeredata_tm3
as.data.frame(center_tm3[1,])
crisis_75$M_TM3 <-(center_tm3[1,])
 
##################
#T1
##################

####PCA

#Check for correlations of the Data 
col_t1 = c('M1_T1', 'M10_T1', 'M11_T1', 'M2_T1', 'M3_T1', 'M4_T1', 'M5_T1', 'M6_T1', 'M7_T1', 'M8_T1', 'M9_T1')
t1 <- as_data_frame(crisis_75[, col_t1])
t1 <-  sapply(t1, as.numeric )
cormat1 <- cor(t1)
round(cormat1, digits = 2)
#usally seen as problematic if there are little correlations above .30 or higher or may correltations above .90, or if single or various items show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(cormat1, n=282) 

#Is there a pattern in the correltaion matrix that can be detected via a factor analysis? KMO[]
KMO(t1)
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(cormat1) 

#Check for number of components in case of a PCA 
t1pca <- prcomp(t1, scale = TRUE, retx=TRUE)
summary(t1pca)
t1pca$rotation

#Screeplot Items
screeplot(t1pca, type='lines', col='blue')

#Factors to extract
parallel <- fa.parallel(t1, fa="fa")

c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

#
centeredata_t1 <- apply(t1, MARGIN=1, FUN=c.fun, t1pca$center, t1pca$scale)

# rotation matrix
center_t1 <- t(t1pca$rotation) %*% centeredata_t1
crisis_75$M_T1 <-(center_t1[1,])


####################
# T2
####################
col_t2 = c('M1_T2', 'M10_T2', 'M11_T2', 'M2_T2', 'M3_T2', 'M4_T2', 'M5_T2', 'M6_T2', 'M7_T2', 'M8_T2', 'M9_T2')
t2 <- as_data_frame(crisis_75[, col_t2])
t2 <-  sapply(t2, as.numeric )

cormat2 <- cor(t2)
round(cormat2, digits = 2)
#usally seen as problematic if there are little correlations above .30 or higher or may correltations above .90, or if single or various items show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(cormat2, n=282) 

#Is there a pattern in the correltaion matrix that can be detected via a factor analysis? KMO[]
KMO(t2)
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(cormat2) 

#Check for number of components in case of a PCA 
t2pca <- prcomp(t2, scale = TRUE, retx=TRUE)
summary(t2pca)

#Screeplot Items
screeplot(t2pca, type='lines', col='blue')

#
centeredata_t2 <- apply(t2, MARGIN=1, FUN=c.fun, t2pca$center, t2pca$scale)

# rotation matrix
center_t2 <- t(t2pca$rotation) %*% centeredata_t2
as.data.frame(center_t2[1,])
crisis_75$M_T2 <-(center_t2[1,])


####################
# T3
####################
col_t3 = c('M1_T3', 'M10_T3', 'M11_T3', 'M2_T3', 'M3_T3', 'M4_T3', 'M5_T3', 'M6_T3', 'M7_T3', 'M8_T3', 'M9_T3')
T3 <- as_data_frame(crisis_75[, col_t3])
T3 <-  sapply(T3, as.numeric )

cormaT3 <- cor(T3)
round(cormaT3, digits = 2)
#usally seen as problematic if there are little correlations above .30 or higher or may correltations above .90, or if single or various items show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(cormaT3, n=282) 

#Is there a pattern in the correltaion matrix that can be detected via a factor analysis? KMO[]
KMO(T3)
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(cormaT3) 

#Check for number of components in case of a PCA 
t3pca <- prcomp(T3, scale = TRUE, retx=TRUE)
summary(t3pca)

#Screeplot Items
screeplot(t3pca, type='lines', col='blue')

#
centeredata_t3 <- apply(T3, MARGIN=1, FUN=c.fun, t3pca$center, t3pca$scale)

# rotation matrix
center_t3 <- t(t3pca$rotation) %*% centeredata_t3
crisis_75$M_T3 <-(center_t3[1,])


#check correlations
T1 <- crisis_75[,c('T_T1')]
T2 <- crisis_75[,c('T_T2')]
T3 <- crisis_75[,c('T_T3')]

#PCA
ID <- crisis_75[, 1]
M_TM3 <- crisis_75[,c('M_TM3')]
M_T1 <- crisis_75[,c('M_T1')]
M_T2 <- crisis_75[,c('M_T2')]
M_T3 <- crisis_75[,c('M_T3')]

p <- cbind(M_TM3, M_T1, M_T2, M_T3)
corp <- cor(p)
round(p, digits = 2)
head(p)
p <- cbind(ID, T1, T2, T3, M_TM3, M_T1, M_T2, M_T3)
#probably due to time variations 

###########
write.csv(p, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_pca_score.txt", row.names = FALSE)
