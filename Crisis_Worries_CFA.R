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
library(reliable)

#load data 
crisis_75 <- readr::read_csv2("C:/Users/hofma/Desktop/LGC/
                              02_data/crisis_75_sep.csv", col_names = TRUE,
                              col_types = cols(ID = "c"))
colnames(crisis_75)

#### worries FScores 

#First reverse Item W6 
#Has the Coronavirus/COVID-19 crisis in your area led to any positive
#changes in your life?
#1-3 None, Only a few, Some

cols <- c('W6_T1', 'W6_T2', 'W6_T3')
crisis_75[,cols] <- crisis_75[,cols]/10
crisis_75[,cols] <- 4-crisis_75[,cols]
head(crisis_75[,cols])
#scores are saved as 10, 20 etc. 
cols_5 <- c('W5_T1', 'W5_T3')
crisis_75[,cols_5] <- crisis_75[,cols_5]/10
head(crisis_75[,cols_5])

##################
#T1
##################

#Check for correlations of the Data 
t1 <- as_data_frame(crisis_75[, 24:29])
t1 <-  sapply(t1, as.numeric )
cormat1 <- cor(t1)
round(cormat1, digits = 2)

cortest.bartlett(cormat1, n=282) 

KMO(t1)

det(cormat1) 

t1pca <- prcomp(t1, scale = TRUE, retx=TRUE)
summary(t1pca)


screeplot(t1pca, type='lines', col='blue')

#Eligible for CFA!
worries_T1 <- "
! Regressions 
Worries_T1=~lambda1*W1_T1
Worries_T1=~lambda2*W2_T1
Worries_T1=~lambda3*W3_T1
Worries_T1=~lambda4*W4_T1
Worries_T1=~lambda5*W5_T1
Worries_T1=~lambda6*W6_T1
! residuals, variances and covariances
W1_T1 ~~ e1*W1_T1
W2_T1 ~~ e2*W2_T1
W3_T1 ~~ e3*W3_T1
W4_T1 ~~ e4*W4_T1
W5_T1 ~~ e5*W5_T1
W6_T1 ~~ e6*W6_T1
Worries_T1 ~~ 1.0*Worries_T1
! means
Worries_T1~mu*1
W1_T1~0*1;
W2_T1~0*1;
W3_T1~0*1;
W4_T1~0*1;
W5_T1~0*1;
W6_T1~0*1;
";
result_worries_T1 <-lavaan(worries_T1, data=t1, fixed.x=FALSE, missing="FIML");
summary(result_worries_T1, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_worries_T1)
T1_w_fscore <- lavPredict(result_worries_T1, method ="Bartlett")
#get H
round(coefficient_H(result_worries_T1),2)

####################
# T2
####################
cols_t2 <- c('W1_T2', 'W2_T2', 'W3_T2', 'W4_T2', 'W5_T2', 'W6_T2')
t2 <- as_data_frame(crisis_75[, cols_t2])
t2 <-  sapply(t2, as.numeric )
head(t2)

#Check
cormat2 <- cor(t2)
round(cormat2, digits = 2)
cortest.bartlett(cormat2, n=282) 
KMO(t2)
det(cormat2) 

#PCA
t2pca <- prcomp(t2, scale = TRUE, retx=TRUE)
summary(t2pca)

#Screeplot Items
screeplot(t2pca, type='lines', col='blue')

worries_T2 <- "
! regressions 
Worries_T2=~lambda1*W1_T2
Worries_T2=~lambda2*W2_T2
Worries_T2=~lambda3*W3_T2
Worries_T2=~lambda4*W4_T2
Worries_T2=~lambda5*W5_T2
Worries_T2=~lambda6*W6_T2
! residuals, variances and covariances
W1_T2 ~~ e1*W1_T2
W2_T2 ~~ e2*W2_T2
W3_T2 ~~ e3*W3_T2
W4_T2 ~~ e4*W4_T2
W5_T2 ~~ e5*W5_T2
W6_T2 ~~ e6*W6_T2
Worries_T2 ~~ 1.0*Worries_T2
! means
Worries_T2~mu*1
W1_T2~0*1;
W2_T2~0*1;
W3_T2~0*1;
W4_T2~0*1;
W5_T2~0*1;
W6_T2~0*1;
";
result_worries_T2 <-lavaan(worries_T2, data=t2, fixed.x=FALSE, missing="FIML");
summary(result_worries_T2, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_worries_T2)
T2_w_fscore <- lavPredict(result_worries_T2, method ="Bartlett")
#get H
round(coefficient_H(result_worries_T2),2)

####################
# T3
####################
cols_t3<- c('W1_T3', 'W2_T3', 'W3_T3', 'W4_T3', 'W5_T3', 'W6_T3')
T3 <- as_data_frame(crisis_75[,cols_t3])
T3 <-  sapply(T3, as.numeric)

cormaT3 <- cor(T3)
round(cormaT3, digits = 2)
cortest.bartlett(cormaT3, n=565) 
KMO(T3)
det(cormaT3) 
T3pca <- prcomp(T3, scale = TRUE, retx=TRUE)
summary(T3pca)

#Screeplot Items
screeplot(T3pca, type='lines', col='blue')

worries_T3 <- "
! regressions 
Worries_T3=~lambda1*W1_T3
Worries_T3=~lambda2*W2_T3
Worries_T3=~lambda3*W3_T3
Worries_T3=~lambda4*W4_T3
Worries_T3=~lambda5*W5_T3
Worries_T3=~lambda6*W6_T3
! residuals, variances and covariances
W1_T3 ~~ e1*W1_T3
W2_T3 ~~ e2*W2_T3
W3_T3 ~~ e3*W3_T3
W4_T3 ~~ e4*W4_T3
W5_T3 ~~ e5*W5_T3
W6_T3 ~~ e6*W6_T3
Worries_T3 ~~ 1.0*Worries_T3
! means
Worries_T3~mu*1
W1_T3~0*1;
W2_T3~0*1;
W3_T3~0*1;
W4_T3~0*1;
W5_T3~0*1;
W6_T3~0*1;
";
result_worries_T3 <-lavaan(worries_T3, data=T3, fixed.x=FALSE, missing="FIML");
summary(result_worries_T3, fit.measures=TRUE, standardized = TRUE)
standardizedSolution(result_worries_T3)
T3_w_fscore <- lavPredict(result_worries_T3, method ="Bartlett")

#get H
round(coefficient_H(result_worries_T2),2)

ID <- crisis_75[, 1]

w <- cbind(T1_fscore, T2_fscore ,T3_fscore)
corf <- cor(w)
round(w, digits = 2)
w <- cbind(ID, T1_w_fscore, T2_w_fscore, T3_w_fscore)
#probably due to time variations 

ICC(w[2:3])
ICC(w[3:4])

###########
#Write DataFrame containing all FScores of Worries and the timestamps 
###########
write.csv(w, file="C:/Users/hofma/Desktop/LGC/02_data/
          Crisis_Worries_fscores.txt", row.names = FALSE)



