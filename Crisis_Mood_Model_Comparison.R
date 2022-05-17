## Author: Valerie Hofmann
## Date: 27/02/2022

#Packages 
library(tidyLPA)
library(lavaan)
library(readr)
library(tidyverse)
library(cSEM)
library(OpenMx)
library(dplyr)
library(corrplot)
library(foreign)
library(psych)
library(lmtest)
#to install onyxR 
#source('https://raw.githubusercontent.com/brandmaier/onyxR/master/tools/install.R')
#to install Onxy itself Java is required - https://onyx-sem.com/
library(onyxR)

#data load
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
crisis[2:12] <- lapply(crisis[2:12], function(x) as.numeric(as.character(x)))
crisis <- as.data.frame(crisis)
head(crisis)

#calculate baseline Latent Growth Curve Model
manifests<-c("M_T1","M_T2","M_T3")
latents<-c("icept","slope")
model_lgcm <- mxModel("Latent Growth Curve Model", 
                 type="RAM",
                 manifestVars = manifests,
                 latentVars = latents,
                 mxPath(from="icept",to=c("M_T1","M_T2","M_T3"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("icept__M_T1","icept__M_T2","icept__M_T3") ),
                 mxPath(from="slope",to=c("M_T2","M_T3"), free=c(FALSE,FALSE), value=c(1.0,2.0) , arrows=1, label=c("slope__M_T2","slope__M_T3") ),
                 mxPath(from="one",to=c("icept","slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__icept","const__slope") ),
                 mxPath(from="icept",to=c("slope","icept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=2, label=c("COV_icept_slope","sigma_i") ),
                 mxPath(from="slope",to=c("slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma_s") ),
                 mxPath(from="M_T1",to=c("M_T1"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                 mxPath(from="M_T2",to=c("M_T2"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                 mxPath(from="M_T3",to=c("M_T3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                 mxPath(from="one",to=c("M_T1","M_T2","M_T3"), free=F, value=0, arrows=1),
                 mxData(crisis, type = "raw")
);
result_lgcm_x <- mxRun(model_lgcm)
summary(result_lgcm_x)
omxGraphviz(result_lgcm_x, "Latent_Growth_Curve.dot")

#Starting Values  
result_lgcm <- mxTryHard(model_lgcm) 
summary(result_lgcm)

#Latent Basis Growth Curve Model, fixed at T1 no defintion Variables but loadings specified
#from B to M_T1 = fixed to 0
#from B to M_T2 = free 
#from B to M_T3 = fixed to 1
manifests<-c("M_T1","M_T2","M_T3")
latents<-c("icept","slope")
model_basis <- mxModel("Mood_LBGC", 
                       type="RAM",
                       manifestVars = manifests,
                       latentVars = latents,
                       mxPath(from="icept",to=c("M_T1","M_T2","M_T3"), free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , arrows=1, label=c("icept__M_T1","icept__M_T2","icept__M_T3") ),
                       mxPath(from="slope",to=c("M_T2","M_T3","M_T1"), free=c(TRUE,FALSE,FALSE), value=c(-8.63,1.0,0.0) , arrows=1, label=c("LamdaT2","slope__M_T3","slope__M_T1") ),
                       mxPath(from="one",to=c("icept","slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=1, label=c("const__icept","const__slope") ),
                       mxPath(from="icept",to=c("slope","icept"), free=c(TRUE,TRUE), value=c(1.0,1.0) , arrows=2, label=c("COV_icept_slope","sigma_i") ),
                       mxPath(from="slope",to=c("slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma_s") ),
                       mxPath(from="M_T1",to=c("M_T1"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                       mxPath(from="M_T2",to=c("M_T2"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                       mxPath(from="M_T3",to=c("M_T3"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("e") ),
                       mxPath(from="one",to=c("M_T1","M_T2","M_T3"), free=F, value=0, arrows=1),
                       mxData(crisis, type = "raw")
);
result_basis_x <- mxRun(model_basis)
summary(result_basis_x)
#onyx(model_basis)

#Starting Values
result_basis <- mxTryHard(model_basis) 
summary(result_basis)


#Model fixed at T1 + Definition Variables Days since handing in the first questionnaire  
manifests<-c("M_T1","M_T2","M_T3")
latents<-c("icept","slope")
model_basis_def <- mxModel("Mood_LBGC_def", 
                           type="RAM",
                           manifestVars = manifests,
                           latentVars = latents,
                           mxPath(from="icept",to=c("M_T1","M_T2","M_T3"), free=c(FALSE,FALSE,FALSE), 
                                  value=c(1.0,1.0,1.0) , arrows=1, label=c("icept__M_T1","icept__M_T2","icept__M_T3") ),
                           mxPath(from="slope",to=c("M_T1", "M_T2","M_T3"), free=c(FALSE, FALSE,FALSE),
                                  arrows=1, label=c("slope__M_T1", "data.D21D", "data.D31D") ),
                           mxPath(from="one",to=c("icept","slope"), free=c(TRUE,TRUE), value=c(1.0,1.0) ,
                                  arrows=1, label=c("const__icept","const__slope") ),
                           mxPath(from="icept",to=c("slope","icept"), free=c(TRUE,TRUE),
                                  value=c(1.0,1.0) , arrows=2, label=c("COV_icept_slope","sigma_i") ),
                           mxPath(from="slope",to=c("slope"), free=c(TRUE), value=c(1.0) , arrows=2, label=c("sigma_s") ),
                           mxPath(from=c("M_T1","M_T2","M_T3"), arrows=2, free=TRUE, values = c(1,1,1), labels=c("e","e","e") ), 
                           mxPath(from="one",to=c("M_T1","M_T2","M_T3"), free=F, value=0, arrows=1), 
                           mxData(crisis, type = "raw")
);
result_basis_def_x <- mxRun(model_basis_def)
summary(result_basis_def_x)

#
result_basis_def <- mxTryHard(model_basis_def) 
summary(result_basis_def)


#####Liklihood Ratio Tests 
mxCompare(result_basis, result_lgcm) 

mxCompare(result_basis_def, result_lgcm) 
#onxy values 
(teststat <- -2 * (2273.997-2278.649))
#df = 8 - 6 = 2
(p.val <- pchisq(teststat, df = 2, lower.tail = FALSE))

#mxCompare(result_basis, result_basis_def) 

################################################################################################

########
#Introduce Classes to Latent Basis Model
########

#based on https://vipbg.vcu.edu/vipbg/OpenMx2/docs//OpenMx/latest/GrowthMixtureModel_Path.html

###One Class
# residual variances
resVars      <- mxPath( from=c("M_T1","M_T2","M_T3"), arrows=2,
                        free=TRUE, values = c(1,1,1),
                        labels=c("e","e","e"))

# latent variances and covariance
latVars      <- mxPath(from=c("icept","slope"), free=TRUE, connect="unique.pairs", value=c(1.0, 0.5, 1.0), 
                       arrows=2, labels=c("sigma_i","COV_icept_slope","sigma_s") )

# intercept loadings
intLoads     <- mxPath(from="icept",to=c("M_T1","M_T2","M_T3"), 
                       free=c(FALSE,FALSE,FALSE), value=c(1.0,1.0,1.0) , 
                       arrows=1, label=c("icept__M_T1","icept__M_T2","icept__M_T3") )
# slope loadings
sloLoads     <- mxPath(from="slope",to=c("M_T1", "M_T2","M_T3"), free=c(FALSE, FALSE,FALSE),
                       arrows=1, label=c("slope__M_T1", "data.D21D", "data.D31D") )

# manifest means
manMeans     <- mxPath(from="one",to=c("M_T1","M_T2","M_T3"), free=F, value=0, arrows=1)

# latent means
latMeans     <- mxPath( from="one", to=c("icept","slope"), arrows=1,
                        free=TRUE,  values=c(1,1), labels=c("const__icept","const__slope") )

funML        <- mxFitFunctionML(vector=TRUE)

class1       <- mxModel("Class1", type="RAM",
                        manifestVars=c("M_T1","M_T2","M_T3"),
                        latentVars=c("icept","slope"),
                        resVars, latVars, intLoads, sloLoads,
                        manMeans, latMeans,
                        funML)


####Adding second class 

# latent variances and covariance
latVars2     <- mxPath( from=c("icept","slope"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,1,1), labels=c("sigma_i2","COV_icept_slope2","sigma_s2") )
# latent means
latMeans2    <- mxPath( from="one", to=c("icept", "slope"), arrows=1,
                        free=TRUE, values=c(0,1), labels=c("const__icept2","const__slope2"))

class2       <- mxModel(class1, name="Class2", latVars2, latMeans2)

dataRaw      <- mxData(observed=crisis, type="raw")

classP <- mxMatrix( type="Full", nrow=2, ncol=1,
                    free=c(TRUE, FALSE), values=1, lbound=0.001,
                    labels=c("p1","p2"), name="weights")

classQ <- mxAlgebra( weights %x% (1/sum(weights)), name="classProbs" )

algFit <- mxAlgebra(-2*sum(log(classProbs[1,1] %x% Class1.fitfunction
                                + classProbs[2,1] %x% Class2.fitfunction)),
                     name="mixtureObj")

fit     <- mxFitFunctionAlgebra("mixtureObj")

lcgc2  <- mxModel("Growth Mixture Model 2",
                  class1, class2, classP, 
                  classQ, 
                  algFit,
                  fit,
                  dataRaw, 
                  mxExpectationMixture(paste0('Class',1:2), scale="softmax"),
                  mxFitFunctionML())

lcgc2Fit_x     <- mxRun(lcgc2, suppressWarnings=TRUE)
summary(lcgc2Fit_x, suppressWarnings=TRUE)


#Starting Values  
lcgc2Fit <- mxTryHard(lcgc2) # Run the model, returning the result into model
summary(lcgc2Fit)

lcgc2Fit$classProbs

##############
#Introducing a third class
##############
latVars3     <- mxPath( from=c("icept","slope"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,.5,1), labels=c("sigma_i3","COV_icept_slope3","sigma_s3") )

# latent means
latMeans3    <- mxPath( from="one", to=c("icept", "slope"), arrows=1,
                        free=TRUE, values=c(0,1), labels=c("const__icept3","const__slope3"))

class3       <- mxModel(class1, name="Class3", latVars3, latMeans3)

classP <- mxMatrix( type="Full", nrow=3, ncol=1,
                     free=c(TRUE, TRUE, FALSE), values=1, lbound=0.001,
                     labels=c("p1","p2","p3"), name="weights" )

algFit3 <- mxAlgebra( -2*sum(log( weights[1,1] %x% weights
                                  + weights[2,1] %x% weights
                                  + weights[3,1] %x% weights)),
                      name="mixtureObj3")

fit3 <- mxFitFunctionAlgebra("mixtureObj3")

lcgc3        <- mxModel("Growth Mixture Model 3",
                        class1, class2, class3, classP,
                        classQ, 
                        algFit3,
                        fit3,
                        dataRaw,
                        mxExpectationMixture(paste0('Class',1:3), scale="softmax"),
                        mxFitFunctionML())

lcgc3Fit_x     <- mxRun(lcgc3, suppressWarnings=TRUE)
summary(lcgc3Fit_x, suppressWarnings=TRUE)

#Starting Values  
lcgc3Fit <- mxTryHard(lcgc3) # Run the model, returning the result into model
summary(lcgc3Fit)

#class probs 
lcgc3Fit$classProbs


######
#Introducing a 4th Class
#####
latVars4     <- mxPath( from=c("icept","slope"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,.5,1), labels=c("sigma_i4","COV_icept_slope4","sigma_s4") )
# latent means
latMeans4    <- mxPath( from="one", to=c("icept", "slope"), arrows=1,
                        free=TRUE, values=c(0,1), labels=c("const__icept4","const__slope4"))

class4       <- mxModel(class1, name="Class4", latVars4, latMeans4)

classP <- mxMatrix( type="Full", nrow=4, ncol=1,
                     free=c(TRUE, TRUE, TRUE, FALSE), values=1, lbound=0.001,
                     labels=c("p1","p2","p3","p4"), name="weights" )

algFit4 <- mxAlgebra( -2*sum(log(weights[1,1] %x% Class1.fitfunction
                                 + weights[2,1] %x% Class2.fitfunction
                                 + weights[3,1] %x% Class3.fitfunction
                                 + weights[4,1] %x% Class4.fitfunction)),
                      name="mixtureObj4")

fit4 <- mxFitFunctionAlgebra("mixtureObj4")

lcgc4        <- mxModel("Growth Mixture Model 4",
                        class1, class2, class3, class4, dataRaw, 
                        classQ, 
                        classP, 
                        algFit4,
                        fit4,
                        mxExpectationMixture(paste0('Class',1:4), scale="softmax"),
                        mxFitFunctionML())

lcgc4Fit_x     <- mxRun(lcgc4, suppressWarnings=TRUE)
summary(lcgc4Fit_x, suppressWarnings=TRUE)

#Starting Values  
lcgc4Fit <- mxTryHard(lcgc4) 
summary(lcgc4Fit)

lcgc4Fit$classProbs


################
#Introducing a 5th class
###############
latVars5     <- mxPath( from=c("icept","slope"), arrows=2, connect="unique.pairs",
                        free=TRUE, values=c(1,.5,1), labels=c("sigma_i5","COV_icept_slope5","sigma_s5") )
# latent means
latMeans5    <- mxPath( from="one", to=c("icept", "slope"), arrows=1,
                        free=TRUE, values=c(0,1), labels=c("const__icept5","const__slope5"))

class5       <- mxModel(class1, name="Class5", latVars5, latMeans5)

classP <- mxMatrix( type="Full", nrow=5, ncol=1,
                     free=c(TRUE, TRUE, TRUE, TRUE, FALSE), values=1, lbound=0.001,
                     labels=c("p1","p2","p3","p4","p5"), name="weights" )

algFit5 <- mxAlgebra( -2*sum(log(weights[1,1] %x% Class1.fitfunction
                                 + weights[2,1] %x% Class2.fitfunction
                                 + weights[3,1] %x% Class3.fitfunction
                                 + weights[4,1] %x% Class4.fitfunction
                                 + weights[5,1] %x% Class5.fitfunction)),
                      name="mixtureObj5")

fit5 <- mxFitFunctionAlgebra("mixtureObj5")

lcgc5        <- mxModel("Growth Mixture Model 5",
                        class1, class2, class3, class4, class5, dataRaw, 
                        classP, classQ, algFit5,
                        mxExpectationMixture(paste0('Class',1:5), scale="softmax"),
                        mxFitFunctionML())

lcgc5Fit     <- mxRun(lcgc5, suppressWarnings=TRUE)
summary(lcgc5Fit, suppressWarnings=TRUE)

#Starting Values  
lcgc5Fit <- mxTryHard(lcgc5) #does not get a solution
summary(lcgc5Fit)

lcgc5Fit$classProbs5


################
#Test Models 
################
#1 vs. 2
#def2 <- mxCompare(lcgc2Fit, result_basis_def, boot=TRUE) #yes
#def2
#calc_lrt(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes)
#calc_lrt(282, 2273.997, 6, 1, 2261.684, 12, 2) #this says no though? 

#3 vs. 2
#def3 <- mxCompare(lcgc3Fit, lcgc2Fit, boot=TRUE) #no
#def3

#calc_lrt(n, null_ll, null_param, null_classes, alt_ll, alt_param, alt_classes)
#calc_lrt(282, 2261.684, 12, 2, 2285.985, 18, 3)

#4 vs. 3
#def4 <- mxCompare(lcgc4Fit, lcgc3Fit, boot=TRUE) #no
#def4

#5 vs. 4
#mxCompare(lcgc5Fit, lcgc4Fit, boot=TRUE) #no

################
#Obtain Class Probabilities
################
#https://openmx.ssri.psu.edu/thread/717 
# view the class probabilities
lcgc2Fit$classProbs

indClassProbs <- function(model, classProbs, round=NA){
  # this function takes a mixture model in OpenMx
  # and returns the posterior class probabilities
  # using Bayes rule, individual person-class likelihoods
  # and the model class probability matrix, as described in
  # Ramaswamy, Desarbo, Reibstein, and Robinson, 1993
  if (missing(model) || !(isS4(model) && is(model, "MxModel"))) {
    stop("'model' argument must be an MxModel object")
  }
  if (missing(classProbs) || !(is.character(classProbs) && length(classProbs) == 1)) {
    stop("'classProbs' argument must be a character string")
  }
  cp <- eval(substitute(mxEval(x, model), list(x = as.symbol(classProbs))))
  cp2 <- as.vector(cp)
  cps <- diag(length(cp2))
  diag(cps) <- cp2
  subs <- model@submodels
  if(min(dim(cp))!=1)stop("Class probabilities matrix must be a row or column vector.")
  if(max(dim(cp))==1)stop("Class probabilities matrix must contain two or more classes.")
  of <- function(num) {
    return(mxEval(objective, subs[[num]]))
  }
  rl <- sapply(1:length(names(subs)), of)
  raw <- (rl %*% cps)
  tot <- 1 / apply(raw, 1, sum)
  div <- matrix(rep(tot, length(cp2)), ncol=length(cp2))
  icp <- raw * div
  if (is.numeric(round)){icp <- round(icp, round)}
  return(icp)
}

#############
#Getting the Entropy 
############
entropy <- function(classProbs){
  # this function takes a matrix of class probabilities from a
  # mixture model with people in rows and classes in columns
  # and returns the entropy statistic, as given by
  # Ramaswamy, Desarbo, Reibstein, and Robinson 1993
  n <- dim(classProbs)[1]
  k <- dim(classProbs)[2]
  e <- 1-sum(-classProbs*log(classProbs))/(n*log(k))
  return(e)
}

###########
# 2 Classes 
###########
icp_2 <- indClassProbs(lcgc2Fit, "classProbs")
print(icp_2)
entropy(icp_2)
#the higher the better
hist(icp_2[,1], main="Class Probabilities", xlab="P(Class==1)", ylab="Count", breaks=20)
hist(icp_2[,2], main="Class Probabilities", xlab="P(Class==2)", ylab="Count", breaks=20)

class <- for(i in 1:nrow(icp_2)){
  if (icp_2[,1] > icp_2[,2]) {
    print("1")
  }
  else (icp_2[,1] < icp_2[,2]) {
    return("2")
  }
  }
x <- class(icp_2)
x <- sapply(icp_2, class)

###########
# 3 Classes 
##########
#Entropy must be calculated via the last prob as it's not free
icp_3 <- indClassProbs(lcgc3Fit, "classProbs")
print(icp_3)
entropy(icp_3)
#hitting a boundary condition (probabilities of 1 or 0 are bad
hist(icp_3[,1], main="Class Probabilities", xlab="P(Class==1)", ylab="Count", breaks=20)
hist(icp_3[,2], main="Class Probabilities", xlab="P(Class==2)", ylab="Count", breaks=20)
hist(icp_3[,3], main="Class Probabilities", xlab="P(Class==3)", ylab="Count", breaks=20)

#Entropy must be calculated via the last prob as it's not free
icp_4 <- indClassProbs(lcgc4Fit, "classProbs")
print(icp_4)
entropy(icp_4)

#Two Class and Three Class Solution yields best results 
#Write file containing probabilities 
#Invert so that higher percentile = higher Mood
crisis2 <- crisis
crisis2$C1prob <- icp_2[,1]
crisis2$C2prob <- icp_2[,2]

crisis3 <- crisis
crisis3$C1prob <- icp_3[,1]
crisis3$C2prob <- icp_3[,2]
crisis3$C3prob <- icp_3[,3]

#new file 
write.csv(crisis2, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_2Classes.txt", row.names = FALSE)
write.csv(crisis3, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_3Classes.txt", row.names = FALSE)

