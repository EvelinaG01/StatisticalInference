# set the path in R
setwd("C:../StatisticalInference")
# Data Insertion
rm(list=ls())
Data<-read.table(file="data.txt" ,header=T ,na.strings=c("uns")) 

# Delete the observations with NA
Data <- na.omit(Data)

#Boxplot & numeric methods of Damage
par(mfrow=c(1,1))
boxplot(Data[,1],ylab="Damage Value",main="Boxplot of Damage")
fivenum(Data[,1])

# Normality Assumption
par(mfrow=c(1,2))
hist(Data[,1],xlab="Damage Value",ylab="Frequency",main="Histogram of Damage")
qqnorm(Data[,1],main="Normal Q-Q Plot of damage",ylab="Sample Quantiles",xlab="Theoretical Quantiles")
qqline(Data[,1])

# Shapiro Test & t-test
shapiro.test(Data[,1])
t.test(Data[,1], mu = 120, alternative = "greater")


# Categorical Variable Woman
x <- which(Data$sex=="woman")
Woman <- c(rep(0,length(Data[,2])))
Woman[x] <- 1
Woman <- factor(Woman)
levels(Woman)
Data <- transform(Data, woman=Woman)
Testing_woman <- subset(Data,Woman==1)
Testing_notWoman <- subset(Data,Woman==0)

#Histogram & Boxplot of women Damage
par(mfrow=c(1,2))
hist(Testing_woman[,1],ylab="Frequency",xlab="Damage Value", main = "Histogram of women Damage")
boxplot(Testing_woman[,1],ylab="Damage Value",main="Boxplot of women Damage")
fivenum(Testing_woman[,1])
summary(Testing_woman[,1])
var(Testing_woman[,1])
sd(Testing_woman[,1])
IQR <- quantile(Testing_woman[,1],0.75)- quantile(Testing_woman[,1],0.25)

#Histogram & Boxplot of Others' Damage
hist(Testing_notWoman[,1],ylab="Frequency",xlab="Damage Value", main = "Histogram of NoWomen Damage")
boxplot(Testing_notWoman[,1],ylab="Damage Value",main="Boxplot of NoWomen Damage")
fivenum(Testing_notWoman[,1])
summary(Testing_notWoman[,1])
var(Testing_notWoman[,1])
sd(Testing_notWoman[,1])
IQR <- quantile(Testing_notWoman[,1],0.75)- quantile(Testing_notWoman[,1],0.25)



# Indepedance between variables
x <- which(Data$faction=="elf")
elf_cat <- rep(0, length(Data[,3]))
elf_cat[x] <- 1
elf_cat <-  factor(elf_cat)

x <- which(Data[,1]<400)
damage_cat <- rep("high", length(Data[,1]))
damage_cat[x] <- "low"
damage_cat <- factor(damage_cat)

table(elf_cat,damage_cat)

chisq.test(table(elf_cat,damage_cat))




