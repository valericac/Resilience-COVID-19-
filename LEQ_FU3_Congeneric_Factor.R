###############
#Congeneric Factor Score LEQ FU3
# Valerie Hofmann
# 20/01/22
###############

# 
##
library(lavaan)
library(readr)
library(tidySEM)
library(ggplot2)
library(dplyr)
library(psych)
library(tidyverse)
library(onyxR)
##

#setwd
options(max.print=1000000)
setwd("C:/Users/hofma/Desktop/FRESHMO/02_data")
#

##load dataset
leq_fu3 <- readr::read_csv("LEQ_FU3_rawdata_stripped.csv", col_names=TRUE, col_types = cols(.default = "n", ID = "c"));
#

library(lavaan);
LEQ_FU3 <-"
! regressions 
   Stress_LEQ=~lambda1*leq_01_ever
   Stress_LEQ=~lambda2*leq_02_ever
   Stress_LEQ=~lambda3*leq_03_ever
   Stress_LEQ=~lambda4*leq_04_ever
   Stress_LEQ=~lambda5*leq_05_ever
   Stress_LEQ=~lambda6*leq_06_ever
   Stress_LEQ=~lambda7*leq_07_ever
   Stress_LEQ=~lambda8*leq_08_ever
   Stress_LEQ=~lambda9*leq_09_ever
   Stress_LEQ=~lambda10*leq_10_ever
   Stress_LEQ=~lambda11*leq_11_ever
   Stress_LEQ=~lambda12*leq_12_ever
   Stress_LEQ=~lambda13*leq_13_ever
   Stress_LEQ=~lambda14*leq_14_ever
   Stress_LEQ=~lambda15*leq_15_ever
   Stress_LEQ=~lambda16*leq_16_ever
   Stress_LEQ=~lambda17*leq_17_ever
   Stress_LEQ=~lambda18*leq_18_ever
   Stress_LEQ=~lambda19*leq_19_ever
   Stress_LEQ=~lambda20*leq_20_ever
   Stress_LEQ=~lambda21*leq_21_ever
   Stress_LEQ=~lambda22*leq_22_ever
   Stress_LEQ=~lambda23*leq_23_ever
   Stress_LEQ=~lambda24*leq_24_ever
   Stress_LEQ=~lambda25*leq_25_ever
   Stress_LEQ=~lambda26*leq_26_ever
   Stress_LEQ=~lambda27*leq_27_ever
   Stress_LEQ=~lambda28*leq_28_ever
   Stress_LEQ=~lambda29*leq_29_ever
   Stress_LEQ=~lambda30*leq_30_ever
   Stress_LEQ=~lambda31*leq_31_ever
   Stress_LEQ=~lambda32*leq_32_ever
   Stress_LEQ=~lambda33*leq_33_ever
   Stress_LEQ=~lambda34*leq_34_ever
   Stress_LEQ=~lambda35*leq_35_ever
   Stress_LEQ=~lambda36*leq_36_ever
   Stress_LEQ=~lambda37*leq_37_ever
   Stress_LEQ=~lambda38*leq_38_ever
   Stress_LEQ=~lambda39*leq_39_ever
! residuals, variances and covariances
   leq_01_ever ~~ e1*leq_01_ever
   leq_02_ever ~~ e2*leq_02_ever
   leq_03_ever ~~ e3*leq_03_ever
   leq_04_ever ~~ e4*leq_04_ever
   leq_05_ever ~~ e5*leq_05_ever
   leq_06_ever ~~ e6*leq_06_ever
   leq_07_ever ~~ e7*leq_07_ever
   leq_08_ever ~~ e8*leq_08_ever
   leq_09_ever ~~ e9*leq_09_ever
   leq_10_ever ~~ e10*leq_10_ever
   leq_11_ever ~~ e11*leq_11_ever
   leq_12_ever ~~ e12*leq_12_ever
   leq_13_ever ~~ e13*leq_13_ever
   leq_14_ever ~~ e14*leq_14_ever
   leq_15_ever ~~ e15*leq_15_ever
   leq_16_ever ~~ e16*leq_16_ever
   leq_17_ever ~~ e17*leq_17_ever
   leq_18_ever ~~ e18*leq_18_ever
   leq_19_ever ~~ e19*leq_19_ever
   leq_20_ever ~~ e20*leq_20_ever
   leq_21_ever ~~ e21*leq_21_ever
   leq_22_ever ~~ e22*leq_22_ever
   leq_23_ever ~~ e23*leq_23_ever
   leq_24_ever ~~ e24*leq_24_ever
   leq_25_ever ~~ e25*leq_25_ever
   leq_26_ever ~~ e26*leq_26_ever
   leq_27_ever ~~ e27*leq_27_ever
   leq_28_ever ~~ e28*leq_28_ever
   leq_29_ever ~~ e29*leq_29_ever
   leq_30_ever ~~ e30*leq_30_ever
   leq_31_ever ~~ e31*leq_31_ever
   leq_32_ever ~~ e32*leq_32_ever
   leq_33_ever ~~ e33*leq_33_ever
   leq_34_ever ~~ e34*leq_34_ever
   leq_35_ever ~~ e35*leq_35_ever
   leq_36_ever ~~ e36*leq_36_ever
   leq_37_ever ~~ e37*leq_37_ever
   leq_38_ever ~~ e38*leq_38_ever
   leq_39_ever ~~ e39*leq_39_ever
   Stress_LEQ ~~ 1.0*Stress_LEQ
! means
   Stress_LEQ~mu*1
   leq_01_ever~0*1;
   leq_02_ever~0*1;
   leq_03_ever~0*1;
   leq_04_ever~0*1;
   leq_05_ever~0*1;
   leq_06_ever~0*1;
   leq_07_ever~0*1;
   leq_08_ever~0*1;
   leq_09_ever~0*1;
   leq_10_ever~0*1;
   leq_11_ever~0*1;
   leq_12_ever~0*1;
   leq_13_ever~0*1;
   leq_14_ever~0*1;
   leq_15_ever~0*1;
   leq_16_ever~0*1;
   leq_17_ever~0*1;
   leq_18_ever~0*1;
   leq_19_ever~0*1;
   leq_20_ever~0*1;
   leq_21_ever~0*1;
   leq_22_ever~0*1;
   leq_23_ever~0*1;
   leq_24_ever~0*1;
   leq_25_ever~0*1;
   leq_26_ever~0*1;
   leq_27_ever~0*1;
   leq_28_ever~0*1;
   leq_29_ever~0*1;
   leq_30_ever~0*1;
   leq_31_ever~0*1;
   leq_32_ever~0*1;
   leq_33_ever~0*1;
   leq_34_ever~0*1;
   leq_35_ever~0*1;
   leq_36_ever~0*1;
   leq_37_ever~0*1;
   leq_38_ever~0*1;
   leq_39_ever~0*1;
";
leqfu3_onefactor <-lavaan(LEQ_FU3, data=leq_fu3, fixed.x=FALSE, missing="FIML");
summary(leqfu3_onefactor, fit.measures=TRUE);
#onyx(LEQ_FU3)

#Factor Score
head(lavPredict(leqfu3_onefactor))
head(lavPredict(leqfu3_onefactor, type = "ov"))

## ------------------------------------------
## merge factor scores to original data.frame
## ------------------------------------------
fscores <- as.data.frame(lavPredict(leqfu3_onefactor, method = "regression"))
ID <- as.character(leq_fu3$ID)
leq_fu3_fs <- as.data.frame(cbind(ID, fscores), stringsAsFactors =FALSE)
head(leq_fu3_fs)

# Write CSV file 
write.csv(leq_fu3_fs, file = "LEQ_FU3_onefactor_score.txt", row.names = FALSE)

