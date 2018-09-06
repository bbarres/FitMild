###############################################################################
###############################################################################
#S34L mutations and effect on fitness parameters
###############################################################################
###############################################################################

#loading the needed packages
require(lme4)
require(psych)

#loading the dataset
indiv<-read.table(file="FitMild.data",header=TRUE,sep="\t")
#see the structure of the file
head(indiv)


###############################################################################
#S34L mutations and effect on fitness parameters
###############################################################################

pairs(indiv[,c(5:8)])
pairs(indiv[,c(5:8)],col=c("red","blue")[indiv$genoR],pch=19)
pairs.panels(indiv[,c(5:8)],bg=c("red","blue")[indiv$genoR],pch=21)

attach(indiv)

mod_lat<-lmer(latence~genoR + (1|ID) + (1|date/rep))
summary(mod_lat)
stripchart(latence~genoR,vertical=TRUE,method="jitter",
           pch=19,col=c("red","blue"))
boxplot(latence~genoR)

mod_spo<-lmer(spore~genoR + (1|ID) + (1|date/rep))
summary(mod_spo)
stripchart(spore~genoR,vertical=TRUE,method="jitter",
           pch=19,col=c("red","blue"))
boxplot(spore~genoR)

mod_inf<-lmer(infection~genoR + (1|ID) + (1|date/rep))
summary(mod_inf)
stripchart(infection~genoR,vertical=TRUE,method="jitter",
           pch=19,col=c("red","blue"))
boxplot(infection~genoR)

mod_indF<-lmer(indFitt~genoR + (1|ID) + (1|date/rep))
summary(mod_indF)
stripchart(indFitt~genoR,vertical=TRUE,method="jitter",
           pch=19,col=c("red","blue"))

#no significant differences between L(resistant) and S(sensitive) genotypes 
#for all the tested fitness measures. 