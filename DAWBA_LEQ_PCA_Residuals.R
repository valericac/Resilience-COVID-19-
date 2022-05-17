## Author: Valerie Hofmann
## Date: 31/03/2022
## PCA

library(lavaan)
library(readr)
library(tidyverse)
library(dplyr)
library(corrplot)
library(foreign)
library(psych)

# In this Version two subfactors fear and distress + costant 
dawba <- 
  readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_FU3_CFA.csv",
                  col_names=TRUE, col_types = cols(.default = "n", ID = "c"));

# NA 
sum(is.na(dawba))#check for NAs 
#Problem for PTSD there are more Missings than non Missings - however those 
#who provided info 
#are important to consider, imptation of missings above 10% of data is not
#recommended. The status is 
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f',
                'se2g', 'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d',
                'se4e', 'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 
                'se5', 'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dawba[,PTSD_names]
#if first questions is answered with no all others will be NA
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] 
2223-2182 #41 Participants 

#Check if they also filled in the stress score questionnaires 
stress <- 
  readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/stress_score.csv",
                  col_names = TRUE, 
                  col_types = cols(stress_mean = "n", ID = "c"))
ds <- merge(dawba, stress)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 
                'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 
                'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 
                'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- ds[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] 
671-664 #7 

#Check if these continued in IMACOV
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
dsc <- merge(ds, crisis)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g',
                'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 
                'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5',
                'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dsc[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] #none of them continued 
#Exclude PTSD

#Check if these continued in IMACOV
crisis <- 
  readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
dsc <- merge(ds, crisis)
PTSD_names <- c('se1', 'se2a', 'se2b', 'se2c', 'se2d', 'se2e', 'se2f', 'se2g', 
                'se2h', 'se2i', 'se2j', 
                'se2k', 'se3', 'se3a', 'se4a', 'se4b', 'se4c', 'se4d', 'se4e', 
                'se4f', 'se4g', 'se4h',
                'se4i', 'se4j', 'se4k', 'se4l', 'se4m', 'se4n', 'se4o', 'se5', 
                'se6', 'se7', 'se8a',
                'se8b', 'se8c', 'se8d')
PTSD <- dsc[,PTSD_names]
PTSD_NA <- PTSD[rowSums(is.na(PTSD)) > 0, ] #none of them continued 
#Exclude PTSD

#Specific Fear and Phobia 
SPE_names <- c('sb1a' , 'sb1a' , 'sb1b' , 'sb1c' , 'sb1d' , 'sb1e' , 'sb1f' ,
               'sb1g' , 'sb1h' , 'sb1i' , 'sb1j' , 'sb1k' , 'sb1l' , 'sb1m' , 
               'sb2' , 'sb3' ,
               'sb4' , 'sb5' , 'sb6' , 'sb7' , 'sb8' , 'sb9' , 'sb10')
SPE <- dsc[,SPE_names]
SPE_NA <- SPE[rowSums(is.na(SPE)) > 0, ] #3 observations
#exclude Specific Fear and Phobia 

#write frame with all items considered 
dd <- dawba[,!names(dawba) %in% PTSD_names]
d <- dd[,!names(dd) %in% SPE_names]
colnames(d) 
write.csv(d, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_PCA.txt"
          , row.names = FALSE)

#load data 
dawbapca <-
  readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_PCA.txt")
colnames(dawbapca)

####################
#check
#Check for correlations of the Data 

cormat <- cor(dawbapca[, 2:102])
round(cormat, digits = 2)
table(cormat<.30)
#usally seen as problematic if there are little correlations 
#above .30 or higher or may correltations above .90, or if single 
#or various items show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(cormat, n=1315) 
#if significant not a identity matrix which is what we are looking for  
#not significant 

#Is there a pattern in the correltaion matrix that can be detected via a 
#factor analysis? KMO[]
KMO(dawbapca[, 2:102])
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(cormat) #below 

###################################

#0 indicated no burden of the symptom 
dawbapca[is.na(dawbapca)] <- 0 #replace NA with0s 
sum(is.na(dawbapca))#check if it worked

#perform pca
res <-prcomp(dawbapca[,2:102], scale = TRUE, retx=TRUE)
summary(res)
res$rotation

#Get CIs 
#install.packages("eigenprcomp")
library(eigenprcomp)
dpca <- as.matrix(dawbapca[,2:102])
res_boot <- boot_pr_comp(dpca, plot = TRUE)
round(res_boot$proportions_quantiles[c(1,2),1],5)

#get relative importance
comp <- round(res_boot$proportions_used[1,1],5)
comp 

#Screeplot Alternative Res Score 1
screeplot(res, type='lines', col='blue') #6

c.fun<-function(df, center, scale) {
  return((df-center)/scale )
}

centeredData <- apply(dawbapca[,2:102], MARGIN=1, FUN=c.fun, res$center,
                      res$scale)

# rotation matrix 
center <- t(res$rotation) %*% centeredData
as.data.frame(center[1,])

#save 
dawbapca$pca_d <-(center[1,])

#get one component 


############
# LEQ
############
leq <- 
  readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/LEQ_FU3_rawdata_stripped.csv",
                  col_names=TRUE, col_types = cols(.default = "n", ID = "c"));


####################
#Check
####################
#check
#Check for correlations of the Data 

corm <- cor(leq[,3:41])
round(corm, digits = 2)
table(corm<.30)
#usally seen as problematic if there are little correlations above .30 
#or higher or may correltations above .90, or if single or various items 
#show a very low correlation with each other 

#Is the present Correlation Matrix an identity matrix?
cortest.bartlett(corm, n=1338) 
#if significant not a identity matrix which is what we are looking for  
#not significant 

#Is there a pattern in the correltaion matrix that can be detected via a 
#factor analysis? KMO[]
KMO(leq[,3:41])
# Kaiser-Meyer Olkin Maß, the closer it comes to 1 the better 

#Determinate of the Corr Matrix
#should not be lower than 0.0001
det(corm) #above 

################

#############
#PCA
#############
resl <-prcomp(leq[,3:41], scale = TRUE, retx=TRUE)
summary(resl)
resl$rotation

#Get CIs 
lpca <- as.matrix(leq[,3:41])
resl_boot <- boot_pr_comp(lpca, plot = TRUE)
round(resl_boot$proportions_quantiles[c(1,2),1],5)

#get relative importance
comp <- round(resl_boot$proportions_used[1,1],5)
comp 

#Screeplot
screeplot(resl, type='lines', col='blue') #6
centeredatal <- apply(leq[,3:41], MARGIN=1, FUN=c.fun, resl$center, resl$scale)

# rotation matrix
centerl <- t(resl$rotation) %*% centeredatal
as.data.frame(centerl[1,])
leq$pca_l <-(centerl[1,])


#write new file
dl <- merge(dawbapca, leq, by=c('ID'))
colnames(dl)
dlp <- dl[, c(1,103,144)]


##########
#Residual 
##########
#corr test leq and dawba 
cor.test(dlp$pca_d, dlp$pca_l)#r=.094,t = 3.393, df = 1298, p-value < 0.0007111

resil <- lm(formula = pca_d ~ pca_l, data = dlp)
summary(resil)

#for the report
round(0.008793, 3)
round(0.000711,3)
CI_upper <- 0.268150+0.079021 
round(CI_upper,3)
CI_lower <- 0.268150-0.079021
round(CI_lower,3)
round(sqrt(0.008793),3)

d_resil <- as.data.frame(residuals(resil))
colnames(d_resil) <- c("res")
d_resil$Symptom <- "Internalizing Symptoms"
#in Line with FRESHMO invert scores here
d_resil$res <-(d_resil$res*-1)
df <- subset(d_resil, 0 >= res)
#percentage
round((nrow(df))/(nrow(d_resil)),3)

histres <- ggplot(d_resil, aes(x = res, fill = Symptom)) +
  geom_histogram(colour="#000000", alpha = 0.6, bins=30) + 
  scale_fill_manual(values="#0072B2", guide="none")+ 
  theme_classic() +
  labs(x = "Residuals", y = "Count", 
       title ="Residuals Int. Symptoms on Life Events",
       plot.title = element_text(hjust = 0.5))
histres <- histres + coord_cartesian(ylim=c(-40,-10)) + 
  histres <- histres + coord_cartesian(xlim=c(0,650))
histres 


#write frame
RESIL <- as.data.frame(d_resil[,1])
ID <- dlp[,1]
dat <- as.data.frame(cbind(ID, RESIL))
colnames(dat) <- c("ID", "RESIL")
head(dat)

#write dataframe
write.csv(dat, "C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_LEQ_PCA_res.txt", 
          row.names = FALSE)
