#Multivar Reg 
#Valerie Hofmann
#26.03.2022

library(lavaan)
library(readr)
library(tidyr)
library(ggplot2)
library(lme4)
library(corrplot)
library(dplyr)

#load and extract demo
demo <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_bl_demo.txt")
#load all other sets 
res <- readr::read_csv("C:/Users/hofma/Desktop/FRESHMO/02_data/DAWBA_LEQ_PCA_res.txt")
crisis <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt")
crisis_class <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D_3Classes_ident.txt")
m3 <- readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_M3_fscores.txt")


#merge to one
ddd <- merge(crisis, demo, by="ID")
dd <- merge(ddd, res[1:2], by="ID")
d <- merge(dd, m3, by="ID" )
  
head(d)
colnames(d)


#men to woman 
table(d$gender)
101
round(177/278,4)#76

#large city
table(d$Urbanicity)
#living in a small town or rual area
#living in a bigger city or in the suburbs of a bigger city = 1
72-278
round(206/278,4) #0.34

#psych health 
table(d$Psy_Health)
##fair and poor = 2, excelent to good = 1
#having fair to poor psychological health increased the by 
237-41
round(237/278,4)

#phys health 
table(d$Phy_Health)
#fair and poor health = 2, excelent to good = 1
#having fair to poor physical health increased the by 
268-10
round(268/278,4)

#household size 
table(d$Household)
#living in a household of more than two people increased 
#--> 1=2 people, 2 = more

#employment
table(d$Employment)
#being unemployed = 2
#being working or on leave, payed or unpayed or a student

#Origin
#en = 1, fr=2, de=3
table(d$Origin)


#write new frame
write.csv(d, "C:/Users/hofma/Desktop/LGC/02_data/Crisis_Reg_Res.txt", 
          row.names = FALSE)

#with class 
ccc <- merge(crisis_class, res[1:2], by="ID")
cc <- merge(ccc, demo, by="ID")
cx <- merge(cc, m3, by="ID" )

table(cx$class)

cx[2:33] <- lapply(cx[2:26], function(x) as.numeric(as.character(x)))
cx <- as.data.frame(cx)
head(cx)
colnames(cx)

#select only relevant predictors and class
c <- cx[13:25]
head(c)

#see resil 
summary(c$RESIL)
range(c$RESIL)
c$RESIL <- round(c$RESIL)
range(c$RESIL)

#order by class 
c <- c[order(c$class),]
head(c)

#get posterior probs 
one <- filter(c, class == "1")
prob1 <- as.data.frame(one$C1prob)
colnames(prob1) <- c("prob")
two <- filter(c, class == "2")
prob2 <- as.data.frame(two$C2prob)
colnames(prob2) <- c("prob")
three <- filter(c, class == "3")
prob3 <- as.data.frame(three$C3prob)
colnames(prob3) <- c("prob")

pp <- rbind(prob1, prob2)
p <- rbind(pp, prob3)


#without the probs 
#without origin
c <- c[c(4,5,7:13)]
colnames(c)


#Multinominal regression 

#Splitting the data using a function from dplyr package
library(caret)
c$class <- as.factor(c$class)
levels(c$class)
table(c$class)

#just gender and resil 
gres <- c[c(1,2,9)]
colnames(gres)

#
index <- createDataPartition(gres$class, p = .70, list = FALSE)
train <- as.data.frame(gres[index,])
test <- as.data.frame(gres[-index,])

# Setting the reference
train$class <- relevel(train$class, ref = "1")
require(nnet)

# Training the multinomial model
multinom_model_red <- multinom(class ~ ., data = gres)
# Checking the model
summary(multinom_model_red)

#odds
exp(coef(multinom_model_red))

#CI
exp(Confint(multinom_model_red))

#predicted values 
head(round(fitted(multinom_model_red),2))

#get std. 
std(summary(multinom_model_red))

#get p values 
z_red <- (summary(multinom_model_red)$coefficients)/
  (summary(multinom_model_red))$ standard.errors
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z_red), 0, 1)) * 2
round(p,3)

#Multinominal regression 

#Splitting the data using a function from dplyr package
library(caret)
c$class <- as.factor(c$class)
levels(c$class)
table(c$class)

index <- createDataPartition(c$class, p = .70, list = FALSE)
train <- as.data.frame(c[index,])
test <- as.data.frame(c[-index,])

# Setting the reference
train$class <- relevel(train$class, ref = "1")
require(nnet)

# Training the multinomial model
multinom_model <- multinom(class ~ ., data = c)
# Checking the model
summary(multinom_model)

#odds
exp(coef(multinom_model))

#Confidence Intervals 
exp(confint(multinom_model))

#predicted values 
head(round(fitted(multinom_model),2))

#get p values 
std <- str(summary(multinom_model)) 

z <- (summary(multinom_model)$coefficients)/
  (summary(multinom_model)$ standard.errors)
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

hist(c$Phy_Health)
table(c$Phy_Health)
table(c$Psy_Health)
10/278

#####
# Predicting the values for train dataset
train$ClassPredicted <- predict(multinom_model, newdata=train, "class")

# Building classification table
tab <- table(train$class, train$ClassPredicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(tab))/sum(tab))*100,2)

# Predicting the class for test dataset
test$ClassPredicted <- predict(multinom_model, newdata = test, "class")

# Building classification table
tab <- table(test$Class, test$ClassPredicted)

###############################################
######class 2 as ref

#Splitting the data using a function from dplyr package
library(caret)
c$class <- as.factor(c$class)
levels(c$class)
table(c$class)

index <- createDataPartition(c$class, p = .70, list = FALSE)
train <- as.data.frame(c[index,])
test <- as.data.frame(c[-index,])

# Setting the reference
train$class <- relevel(train$class, ref = "2")
require(nnet)

c$class <- relevel(c$class, ref = "2")
require(nnet)

# Training the multinomial model
multinom_model_2 <- multinom(class ~ ., data = c)
# Checking the model
summary(multinom_model_2)

#odds
exp(coef(multinom_model_2))

#Confidence Intervals 
exp(confint(multinom_model_2))

#predicted values 
head(round(fitted(multinom_model_2),2))

#get p values 
std <- str(summary(multinom_model_2)) 

z <- (summary(multinom_model)$coefficients)/
  (summary(multinom_model)$ standard.errors)
# 2-tailed Wald z tests to test significance of coefficients
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
