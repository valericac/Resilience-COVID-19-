######CRISIS 
# Inspection 
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
dat <-  readr::read_csv(file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores.txt") 
summary(dat)
#extract mood
mood <- as.data.frame(dat[, c(1,5:7)])
pairs.panels(mood[-1])
#reshape to long
moodl <- reshape(mood, idvar="ID", varying=2:4, timevar="Time", sep="_", direction="long")

#extract worries
worries <- as.data.frame(dat[, c(1, 8:10)])
pairs.panels(worries[-1])
#reshape to long
worriesl <- reshape(worries, idvar="ID", varying=2:4, timevar="Time", sep="_", direction="long")


#time 
age <- as.data.frame(dat[,1:4])
age[,2] <- (age[,2])/365
age[,3:4] <- (age[,3:4])/3650
head(age)
agel <- reshape(age, idvar="ID", timevar="Time", sep="_", varying=2:4, direction="long")

#Calculate individual differences 
D21Dx <- (dat[,3]/10)-dat[,2]
D21Dx <- as.data.frame(D21Dx)
head(D21Dx)
max(D21Dx)
min(D21Dx)
D21D <- D21Dx/241
head(D21D)

#dat$D32D <- (dat[,4]/10)-(dat[,3]/10)
D31Dx <- (dat[,4]/10)-dat[,2]
D31Dx <- as.data.frame(D31Dx)
max(D31Dx$T_T3)#241
min(D31Dx)
D31D <- (D31Dx/241)
max(D31D$T_T3)#241

#make long 
#reshape to long
#write 0 column 
a <- matrix(0, 282, 1)
a <- as.data.frame(a)
colnames(a) <- c("T_T1")
tt <- cbind(dat[1], a, D21Dx, D31Dx)
tt <- reshape(tt, idvar="ID", timevar="Time", sep="_", varying=2:4, direction="long")


#Translate to Months 
#Months between April to August 
m1 <- (30+31+30+31+31)/5
#dat$D21M <- dat$D21D/m1
#Months between August to December
#m2 <- (30+31+30+31)/4
#dat$D32M <- dat$D32D/m2
#Months between April to December 
m3 <- (30+31+30+31+31+30+31+30+31)/9
#dat$D31M <- dat$D31D/m3

dat <- cbind(dat, D21D, D31D)
colnames(dat)
colnames(dat) <- c("ID", "T_T1", "T_T2", "T_T3", "M_T1", "M_T2", "M_T3", "W_T1", "W_T2", "W_T3", "D21D", "D31D")
head(dat)

#save
write.csv(dat, file="C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_Worries_fscores_D.txt", row.names = FALSE)
#dat <- as.list(dat)

histD21D <- ggplot(D21Dx, aes(x = T_T2)) + geom_histogram(colour="#000000", alpha = 0.6, bins=30) + 
  scale_fill_manual(values="#0072B2") +
  theme_classic() +
  labs(x = "Days since T2", y = "Count", title =  "Defintion Variable at T3", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=c(14,20,30,40,50,60,70,80,90,100))
histD21D

histD31D <- ggplot(D31Dx, aes(x = T_T3)) + geom_histogram(colour="#000000", alpha = 0.6, bins=30) + 
  scale_fill_manual(values="#0072B2") +
  theme_classic() +
  labs(x = "Days since T2", y = "Count", title =  "Defintion Variable at T4", plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=c(140, 180, 200, 220, 241))
histD31D

###########
#Plots
###########


######
#Mood
#######

### plot means per measurement occasion
plot <- ggplot(moodl, aes(x=Time, y=M)) 
plot <- plot + stat_summary(fun="mean", geom="point", size=2, color="red")
plot


### now adjust y-axis
plot <- ggplot(moodl, aes(x=Time, y=M)) 
plot <- plot + coord_cartesian(ylim=c(0,1,7))
plot <- plot + stat_summary(fun="mean", geom="point", size=2, color="red")
plot

### include more information
plot <- ggplot(moodl, aes(x=Time, y=M)) 
plot <- plot + geom_violin() + geom_point(position = position_jitter(height = .05, width=.2))
plot <- plot + coord_cartesian(ylim=c(-0.5,7)) 
plot <- plot + stat_summary(fun="mean", geom="point", size=4, color="red") 
plot

### make it more pretty-ish
plot <- ggplot(moodl, aes(x=Time, y=M)) 
plot <- plot + geom_violin(fill="grey90") + geom_point(position = position_jitter(height = .05, width=.2))
plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + stat_summary(fun="mean", geom="point", size=2, color="red") 
plot <- plot + scale_x_discrete(name="Timepoint", labels=c("T2", "T3", "T4"))
plot <- plot + scale_y_continuous(name="Mood States", breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot


### plot individual trajectories
plot <- ggplot(moodl, aes(x=Time, y=M, group=ID)) 
plot <- plot + geom_point(color="grey60", alpha=.2) + geom_line(color="grey60", alpha=.2)
plot <- plot + stat_summary(fun="mean", geom="line", size=.6, color="#D55E00", aes(group=1))
plot <- plot + stat_summary(fun="mean", geom="point", size=2, color="#D55E00", aes(group=1))
plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + scale_x_discrete(name="Timepoint", labels=c("T2", "T3", "T4"))
plot <- plot + scale_y_continuous(name="Mood States", breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot
ggsave("C:/Users/hofma/Desktop/LGC/03_outputs/trajectory.png", width = 15, height = 10, units = "cm", device = "png")

tm <- merge(tt, moodl)
am <- merge(agel, moodl)
am$T <- round(am$T, digits = 3)
min(am$T)
max(am$T)

mean_tm<- tm %>% 
  group_by(Time) %>%
  summarise(M = mean(M),
            T = mean(T))

###With Means and Time Distance 
# Colour in Timeframe?
head(tm)
plot <- ggplot(tm, aes(x=T, y=M, group=ID)) 
plot <- plot + geom_point(color="grey60", alpha=.2) +  geom_line(color="grey60",
                                                                 alpha=.2) +
  annotate("point", x = 0, y = 2.58, colour = "#0072B2", size=3) +
  annotate("text", x = 0, y = 2.58, label = "Mean T2", colour = "#0072B2", 
           vjust = -0.8, size = 3)+
  annotate("point", x = 42.3, y = 2.40, colour = "#0072B2", size=3) +
  annotate("text", x = 42.3, y = 2.40, label = "Mean T3", colour = "#0072B2", 
           vjust = -0.8, size = 3)+
  annotate("point", x = 199, y = 2.60, colour = "#0072B2", size=3) +
  annotate("text", x = 199, y = 2.60, label = "Mean T4", colour = "#0072B2", 
           vjust = -0.8, size = 3)+
  annotate("segment", x = 0, xend = 42.3, y = 2.58, yend = 2.40,
           colour = "#0072B2", size=.8)+ #add lines between means
  annotate("segment", x = 42.3, xend = 199, y = 2.40, yend = 2.60,
           colour = "#0072B2", size=.8)+ #lines between means
  #Annotate T3 
  annotate("segment", x = 14, xend = 100, y = 0, yend = 0, 
           colour = "gray60") +
  annotate("text", x = 95, y = 0, label = "Period T3", vjust = -0.8, size = 3,
           colour = "gray60")+
  #Annotate T4
  annotate("segment", x = 140, xend = 241, y = 0, yend = 0, 
           colour = "gray60") +
  annotate("text", x = 235, y = 0, label = "Period T4", vjust = -0.8, size = 3,
           colour = "gray60")
plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + coord_cartesian(xlim=c(0,241))
plot <- plot + scale_x_continuous(name="Days since T2",
                                  breaks=c(0,50,100,150,200,241))
plot <- plot + scale_y_continuous(name="Mood States", 
                                  breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot
ggsave("C:/Users/hofma/Desktop/LGC/03_outputs/trajectory_days.png", 
       width = 20, height = 10, units = "cm", device = "png")

#####Add classes 
c3 <- 
  readr::read_csv("C:/Users/hofma/Desktop/LGC/02_data/Crisis_Mood_3C_long.txt")

head(c3)
tc3 <- merge(tt, c3)
head(tc3)


table(tc3$class)/3
round(72/282,3)
round(50/282,3)
round(160/282,3)

#Get means and sds
library(apaTables)
?apa.1way.table
apa.1way.table(iv = Time, dv = M, data = tc3, 
               filename = "Table5_APA.doc", 
               table.number = 5)


#get class means
#T2
tc3_2 <- filter(tc3, Time=="T1")
tc3_t2 <- tc3_2 %>%  group_by(class) 
summarise(tc3_t2, M = mean(M))
summarise(tc3_t2, sd=sd(M))

#T3
tc3_3 <- filter(tc3, Time=="T2")
tc3_t3 <- tc3_3 %>%  group_by(class)
summarise(tc3_t3, M = mean(M))
summarise(tc3_t3, sd=sd(M))

#T4
tc3_4 <- filter(tc3, Time=="T3")
tc3_t4 <- tc3_4 %>%  group_by(class) 
summarise(tc3_t4, M = mean(M))
summarise(tc3_t4, sd=sd(M))

#
#Assign means
tc3$mean <-
  ifelse(tc3$Time=="T1" & tc3$class=="1", '3.23',
  ifelse(tc3$Time=="T1" & tc3$class=="2", '1.55',
  ifelse(tc3$Time=="T1" & tc3$class=="3", '2.62',
  ifelse(tc3$Time=="T2" & tc3$class=="1", '3.19',
  ifelse(tc3$Time=="T2" & tc3$class=="2", '1.29', 
  ifelse(tc3$Time=="T2" & tc3$class=="3", '2.39',  
  ifelse(tc3$Time=="T3" & tc3$class=="1", '4.04',
  ifelse(tc3$Time=="T3" & tc3$class=="2", '1.18',
  ifelse(tc3$Time=="T3" & tc3$class=="3", '2.40', 'NO')))))))))

table(tc3$mean)
head(tc3)
tc3[3:6] <- lapply(tc3[3:6], function(x) as.numeric(as.character(x)))
tc3 <- as.data.frame(tc3)
head(tc3)

#plot
plot <- ggplot(tc3, aes(x=T, group=ID, color=factor(class))) 
plot <- plot + geom_point(alpha=.2, aes(y=M)) +  geom_line(alpha=.2, aes(y=M))
plot <- plot + stat_summary(fun="mean", geom="point", size=.6, alpha=.4, 
                            aes(y=mean, x=Time, group=ID, color=factor(class)))
plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + coord_cartesian(xlim=c(0,241))
plot <- plot + scale_x_continuous(name="Days since T2", 
                                  breaks=c(0,50,100,150,200,241))
plot <- plot + scale_y_continuous(name="Mood States", 
                                  breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot <- plot + scale_colour_manual(values=c("#D55E00", "#56B4E9", "#009E73"))
plot

#annotated version
plot <- ggplot(tc3, aes(x=T, group=ID, color=factor(class))) 
plot <- plot + geom_point(alpha=.1, aes(y=M)) +  geom_line(alpha=.1, aes(y=M))+
  #class 1
  annotate("point", x = 0, y = 3.23, colour = "#D55E00", size=3) +
  annotate("point", x = 42.3, y = 3.19, colour = "#D55E00", size=3) +
  annotate("point", x = 199, y = 4.04, colour = "#D55E00", size=3) +
  annotate("text", x = 199, y = 4.04, label = "Means Class 1",
           colour = "#D55E00", 
           vjust = -0.8, size = 4)+
  annotate("segment", x =0 , xend = 42.3, y = 3.23, yend = 3.19,
           colour = "#D55E00", size=.8)+
  annotate("segment", x = 42.3, xend = 199, y = 3.19, yend = 4.04,
           colour = "#D55E00", size=.8)+
  #class 2
  annotate("point", x = 0, y = 1.55, colour = "#56B4E9", size=3) +
  annotate("point", x = 42.3, y = 1.29, colour = "#56B4E9", size=3) +
  annotate("point", x = 199, y = 1.18, colour = "#56B4E9", size=3) +
  annotate("text", x = 199, y = 1.18, label = "Means Class 2",
           colour = "#56B4E9", 
           vjust = -0.8, size = 4)+
  annotate("segment", x =0 , xend = 42.3, y = 1.55, yend = 1.29,
           colour = "#56B4E9", size=.8)+
  annotate("segment", x = 42.3, xend = 199, y = 1.29, yend = 1.18,
           colour = "#56B4E9", size=.8)+
  #class 3
  annotate("point", x = 0, y = 2.62, colour = "#009E73", size=3) +
  annotate("point", x = 42.3, y = 2.39, colour = "#009E73", size=3) +
  annotate("point", x = 199, y = 2.40, colour = "#009E73", size=3) +
  annotate("text", x = 199, y = 2.40, label = "Means Class 3", 
           colour = "#009E73", 
           vjust = -0.8, size = 4)+
  annotate("segment", x =0 , xend = 42.3, y = 2.62, yend = 2.39,
           colour = "#009E73", size=.8)+
  annotate("segment", x = 42.3, xend = 199, y = 2.39, yend = 2.40,
           colour = "#009E73", size=.8)+
  #Annotate T3 
  annotate("segment", x = 14, xend = 100, y = 0, yend = 0, 
           colour = "gray60") +
  annotate("text", x = 95, y = 0, label = "Period T3", vjust = -0.8, size = 3,
           colour = "gray60")+
  #Annotate T4
  annotate("segment", x = 140, xend = 241, y = 0, yend = 0, 
         colour = "gray60") +
  annotate("text", x = 235, y = 0, label = "Period T4", vjust = -0.8, size = 3,
           colour = "gray60")

plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + coord_cartesian(xlim=c(0,241))
plot <- plot + scale_x_continuous(name="Days since T2",
                                  breaks=c(0,50,100,150,200,241))
plot <- plot + scale_y_continuous(name="Mood States", breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot <- plot + scale_colour_manual(values=c("#D55E00", "#56B4E9", "#009E73"),
                                   guide="none")
plot
ggsave("C:/Users/hofma/Desktop/LGC/03_outputs/trajectory_classes.png",
       width = 20, height = 10, units = "cm", device = "png")


#Check age 
plot <- ggplot(am, aes(x=T, y=M, group=ID)) 
plot <- plot + geom_point(color="grey60", alpha=.2) + 
  geom_line(color="grey60", alpha=.2)
plot <- plot + coord_cartesian(ylim=c(0,7)) 
plot <- plot + coord_cartesian(xlim=c(23,28)) 
plot <- plot + scale_x_continuous(name="Age")
plot <- plot + scale_y_continuous(name="Mood States", breaks=c(0,1,2,3,4,5,6,7))
plot <- plot + theme_classic()
plot



#### inspect and plot: wide data ####
plot <- ggplot(mood, aes(x=M_T1, y=M_T2, group=ID)) 
plot <- plot + geom_point()
plot

### add jitter
plot <- ggplot(mood, aes(x=M_T1, y=M_T2, group=ID))
plot <- plot + geom_point(position=position_jitter())
plot

### add regression line
plot <- ggplot(mood, aes(x=M_T1, y=M_T2, group=ID)) 
plot <- plot + geom_point(position=position_jitter())
plot <- plot + geom_smooth(method="lm")
plot


### make it more pretty-ish
plot <- ggplot(mood, aes(x=M_T1, y=M_T2, group=ID)) 
plot <- plot + geom_point(position=position_jitter())
plot <- plot + geom_smooth(method="lm", color="red", fill="red")
plot <- plot + coord_cartesian(ylim=c(0,4), xlim=c(1,3)) 
plot <- plot + scale_x_continuous(name="Mood States T1", breaks=c(0,1,2,3,4))
plot <- plot + scale_y_continuous(name="Mood States T2", breaks=c(0,1,2,3,4))
plot <- plot + theme_bw()
plot


### Create a scatterplot matrix
library(car)
## function adjusted from here:
#http://www.sthda.com/english/wiki/scatter-plot-matrices-r-base-graphs

panel.cor <- function(x, y, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), 3)
  txt <- paste("r = ", r, sep = "")
  text(0.5, 0.6, txt)
  
  p <- round(cor.test(x, y)$p.value, 3)
  txt2 <- paste("p = ", p, sep = "")
  if(p<0.001) txt2 <- paste("p ", "< 0.001", sep = "")
  text(0.5, 0.4, txt2)
}

scatterplotMatrix(~ M_T1 + M_T1 + M_T3, data=mood)
scatterplotMatrix(~ M_T1 + M_T1 + M_T3, data=mood, smooth=F,
                  lower.panel=panel.cor, xlim=c(0,4), ylim=c(0,4),
                  var.labels=c("Mood T1", "Mood T2", "Mood T3"),
                  col = "grey",
                  diagonal=list(method="boxplot"))

#################
# Worries
################

#### Balkendiagramme
plot <- ggplot(worriesl, aes(x=Time, y=W))
plot <- plot + coord_cartesian(ylim=c(0,2)) 
plot <- plot + stat_summary(fun.data="mean_cl_boot",
                            width=.3, color="red") 
plot <- plot + stat_summary(fun="mean", geom="bar", width=.5, fill="cyan") 
plot <- plot + scale_x_discrete(name="Timepoint", 
                                labels=c("Baseline", "Post", "Follow-up"))
plot <- plot + theme_bw()
plot

### plot individual trajectories
plot <- ggplot(worriesl, aes(x=Time, y=W, group=ID)) 
plot <- plot + geom_point(color="grey60", alpha=.4) + 
  geom_line(color="grey60", alpha=.4)
plot <- plot + stat_summary(fun="mean", geom="line", size=1,
                            color="red", aes(group=1))
plot <- plot + stat_summary(fun="mean", geom="point", size=2,
                            color="red", aes(group=q))
plot <- plot + coord_cartesian(ylim=c(0,11)) 
plot <- plot + scale_x_discrete(name="Timepoint", labels=c("T1", "T2", "T3"))
plot <- plot + scale_y_continuous(name="Corona Worries",
                                  breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14))
plot <- plot + theme_bw()
plot

scatterplotMatrix(~ M_T1 + M_T2 + M_T3 + W_T1 + W_T2 + W_T3, data=dat, smooth=F,
                  lower.panel=panel.cor, xlim=c(0,4), ylim=c(0,4),
                  col = "grey",
                  diagonal=list(method="boxplot"))





