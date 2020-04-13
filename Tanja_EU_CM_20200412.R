# title: "Conditioned microbiome data visualization - Europe"
# author: "Tatjana Zivkovic"
# date: "03-03-2020"

library(tidyverse)
library(car)
library(rcompanion)
library(FSA)
library(agricolae)
library(reshape2)
library(readxl)
library(gridExtra)

#import phenotype data into R
CM <- read.delim("EU_data_all.txt")

#calculate Growth rate (cm/day) - lenght of the incubation 28 days
CM$Growth.rate.cmperday <- (CM$Week4-CM$Week0)/28
head(CM)

#visualization of both (Iceland and France/Sweden) experiments by Chamber and Microbiome
CM.sum<-CM %>%
  group_by(Moss, Chamber, Microbiome, Condition) %>%
  summarise(N = length(Growth.rate.cmperday),
             mean = mean(Growth.rate.cmperday),
             sd = sd(Growth.rate.cmperday),
             se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=Chamber, y=mean, fill=Chamber))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=10),
                                                                axis.text.x = element_text(angle = 90, vjust = 0.5),
                                                                axis.title=element_text(size=16,face="bold"),
                                                                legend.text=element_text(size=16))+
  facet_grid(Moss~Microbiome,scales="free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 

CM.sum

#calculate Tmean - Torigin (Torigin are not field data for al sites yet...)
CM$Tmean.Torigin <- (CM$Tmean-CM$Torigin)

#building a figure that shows the difference between T max (largest temp in the exp chamber) and T origin in relation to Growth rate in different microbiomes only for Fallax
CM.MBMoss<-subset(CM,  Moss == "Fallax") %>%
  
  group_by(MBMoss, Tmean.Torigin, Condition) %>%
  summarise(N = length(Growth.rate.cmperday),
            mean = mean(Growth.rate.cmperday),
            sd = sd(Growth.rate.cmperday),
            se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=Tmean.Torigin, y=mean, fill=Condition))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Tmax-Torigin (C)") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                                      axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                                      axis.title=element_text(size=16,face="bold"),
                                                                      legend.text=element_text(size=16))+
  facet_grid(rows = vars(MBMoss), scales = "free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75))
CM.MBMoss

#Visualizing only Fallax and grouping data by countries
CM.countryfallax<-subset(CM,  Moss == "Fallax") %>%
  
  group_by(Country, Chamber, Microbiome, Condition) %>%
  summarise(N = length(Growth.rate.cmperday),
            mean = mean(Growth.rate.cmperday),
            sd = sd(Growth.rate.cmperday),
            se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=Chamber, y=mean, fill=Chamber))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                               axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                               axis.title=element_text(size=16,face="bold"),
                                                               legend.text=element_text(size=16))+
  facet_grid(Country~Microbiome,scales="free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 

CM.countryfallax

#Visualizing only Magellanicum and grouping data by countries
CM.countrymag<-subset(CM,  Moss == "Magellanicum") %>%
  
  group_by(Country, Chamber, Microbiome, Condition) %>%
  summarise(N = length(Growth.rate.cmperday),
            mean = mean(Growth.rate.cmperday),
            sd = sd(Growth.rate.cmperday),
            se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=Chamber, y=mean, fill=Chamber))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                               axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                               axis.title=element_text(size=16,face="bold"),
                                                               legend.text=element_text(size=16))+
  facet_grid(Country~Microbiome,scales="free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 
CM.countrymag


#Visualizing only Fallax and grouping data by countries
CM <- read.delim("EU_data_all_v1.txt")
CM$Growth.rate.cmperday <- (CM$Week4-CM$Week0)/28
CM$TCHAMBER = factor(CM$Tmean)
CM.country_SWE<-subset(CM,  Country == "Sweden") %>%
  
  group_by(Country, Condition, TCHAMBER) %>%
  summarise(N = length(Growth.rate.cmperday),
            mean = mean(Growth.rate.cmperday),
            sd = sd(Growth.rate.cmperday),
            se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=TCHAMBER, y=mean, fill=Condition))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                               axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                               axis.title=element_text(size=16,face="bold"),
                                                               legend.text=element_text(size=16))+
  facet_grid(rows = vars(Country), scales = "free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 
  
CM.country_FRA<-subset(CM,  Country == "France") %>%
  group_by(Country, Condition, TCHAMBER) %>%
    summarise(N = length(Growth.rate.cmperday),
              mean = mean(Growth.rate.cmperday),
              sd = sd(Growth.rate.cmperday),
              se = sd/sqrt(N)
    )%>%
    ggplot(aes(x=TCHAMBER, y=mean, fill=Condition))+
    geom_bar(position=position_dodge(), stat = "identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
    xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                                 axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                                 axis.title=element_text(size=16,face="bold"),
                                                                 legend.text=element_text(size=16))+
    facet_grid(rows = vars(Country), scales = "free")+  
    theme(strip.background = element_rect(fill="black"),
          strip.text = element_text(color="white",size=16))+
    guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 

CM.country_ICE<-subset(CM,  Country == "Iceland") %>%
  group_by(Country, Condition, TCHAMBER) %>%
  summarise(N = length(Growth.rate.cmperday),
            mean = mean(Growth.rate.cmperday),
            sd = sd(Growth.rate.cmperday),
            se = sd/sqrt(N)
  )%>%
  ggplot(aes(x=TCHAMBER, y=mean, fill=Condition))+
  geom_bar(position=position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.1, position=position_dodge(.9)) +
  xlab("Growth chamber") +ylab("Growth rate (cm/day)") + theme(axis.text=element_text(size=12),
                                                               axis.text.x = element_text(angle = 0, vjust = 0.5),
                                                               axis.title=element_text(size=16,face="bold"),
                                                               legend.text=element_text(size=16))+
  facet_grid(rows = vars(Country), scales = "free")+  
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(color="white",size=16))+
  guides(fill=guide_legend(keywidth = 0.75,keyheight = 0.75)) 

grid.arrange(CM.country_SWE,CM.country_FRA,CM.country_ICE, nrow=3)

#Statistical analyses (One-way ANOVA to compare Control-Ambient-Warm in each growth chamber)
#these are only for Fallax across all sites and probably there is a more elegant way to do this...
CM.SWE<- CM %>%
  filter(Country=="Sweden" & TCHAMBER =='16.75')
CM.SWE.rank <- rank(CM.SWE$Growth.rate.cmperday)
CM.SWEDEN <- cbind ( CM.SWE, CM.SWE.rank)
ranked.CM.SWE.aov <- aov(CM.SWE.rank ~ Condition, data = CM.SWEDEN)
Anova(ranked.CM.SWE.aov, type = 3)
TukeyHSD(ranked.CM.SWE.aov)

CM.SWE<- CM %>%
  filter(Country=="Sweden" & TCHAMBER =='19')
CM.SWE.rank <- rank(CM.SWE$Growth.rate.cmperday)
CM.SWEDEN <- cbind ( CM.SWE, CM.SWE.rank)
ranked.CM.SWE.aov <- aov(CM.SWE.rank ~ Condition, data = CM.SWEDEN)
Anova(ranked.CM.SWE.aov, type = 3)
TukeyHSD(ranked.CM.SWE.aov)

CM.SWE<- CM %>%
  filter(Country=="Sweden" & TCHAMBER =='21.25')
CM.SWE.rank <- rank(CM.SWE$Growth.rate.cmperday)
CM.SWEDEN <- cbind ( CM.SWE, CM.SWE.rank)
ranked.CM.SWE.aov <- aov(CM.SWE.rank ~ Condition, data = CM.SWEDEN)
Anova(ranked.CM.SWE.aov, type = 3)
TukeyHSD(ranked.CM.SWE.aov)

CM.SWE<- CM %>%
  filter(Country=="Sweden" & TCHAMBER =='25.75')
CM.SWE.rank <- rank(CM.SWE$Growth.rate.cmperday)
CM.SWEDEN <- cbind ( CM.SWE, CM.SWE.rank)
ranked.CM.SWE.aov <- aov(CM.SWE.rank ~ Condition, data = CM.SWEDEN)
Anova(ranked.CM.SWE.aov, type = 3)
TukeyHSD(ranked.CM.SWE.aov)

CM.FRA<- CM %>%
  filter(Country=="France" & TCHAMBER =='16.75')
CM.FRA.rank <- rank(CM.FRA$Growth.rate.cmperday)
CM.FRANCE <- cbind ( CM.FRA, CM.FRA.rank)
ranked.CM.FRA.aov <- aov(CM.FRA.rank ~ Condition, data = CM.FRANCE)
Anova(ranked.CM.FRA.aov, type = 3)
TukeyHSD(ranked.CM.FRA.aov)

CM.FRA<- CM %>%
  filter(Country=="France" & TCHAMBER =='19')
CM.FRA.rank <- rank(CM.FRA$Growth.rate.cmperday)
CM.FRANCE <- cbind ( CM.FRA, CM.FRA.rank)
ranked.CM.FRA.aov <- aov(CM.FRA.rank ~ Condition, data = CM.FRANCE)
Anova(ranked.CM.FRA.aov, type = 3)
TukeyHSD(ranked.CM.FRA.aov)

CM.FRA<- CM %>%
  filter(Country=="France" & TCHAMBER =='21.25')
CM.FRA.rank <- rank(CM.FRA$Growth.rate.cmperday)
CM.FRANCE <- cbind ( CM.FRA, CM.FRA.rank)
ranked.CM.FRA.aov <- aov(CM.FRA.rank ~ Condition, data = CM.FRANCE)
Anova(ranked.CM.FRA.aov, type = 3)
TukeyHSD(ranked.CM.FRA.aov)

CM.FRA<- CM %>%
  filter(Country=="France" & TCHAMBER =='25.75')
CM.FRA.rank <- rank(CM.FRA$Growth.rate.cmperday)
CM.FRANCE <- cbind ( CM.FRA, CM.FRA.rank)
ranked.CM.FRA.aov <- aov(CM.FRA.rank ~ Condition, data = CM.FRANCE)
Anova(ranked.CM.FRA.aov, type = 3)
TukeyHSD(ranked.CM.FRA.aov)

CM.ICE<- CM %>%
  filter(Country=="Iceland" & TCHAMBER =='8.5')
CM.ICE.rank <- rank(CM.ICE$Growth.rate.cmperday)
CM.ICELAND <- cbind ( CM.ICE, CM.ICE.rank)
ranked.CM.ICE.aov <- aov(CM.ICE.rank ~ Condition, data = CM.ICELAND)
Anova(ranked.CM.ICE.aov, type = 3)
TukeyHSD(ranked.CM.ICE.aov)

CM.ICE<- CM %>%
  filter(Country=="Iceland" & TCHAMBER =='15')
CM.ICE.rank <- rank(CM.ICE$Growth.rate.cmperday)
CM.ICELAND <- cbind ( CM.ICE, CM.ICE.rank)
ranked.CM.ICE.aov <- aov(CM.ICE.rank ~ Condition, data = CM.ICELAND)
Anova(ranked.CM.ICE.aov, type = 3)
TukeyHSD(ranked.CM.ICE.aov)

CM.ICE<- CM %>%
  filter(Country=="Iceland" & TCHAMBER =='21.5')
CM.ICE.rank <- rank(CM.ICE$Growth.rate.cmperday)
CM.ICELAND <- cbind ( CM.ICE, CM.ICE.rank)
ranked.CM.ICE.aov <- aov(CM.ICE.rank ~ Condition, data = CM.ICELAND)
Anova(ranked.CM.ICE.aov, type = 3)
TukeyHSD(ranked.CM.ICE.aov)

CM.ICE<- CM %>%
  filter(Country=="Iceland" & TCHAMBER =='26.5')
CM.ICE.rank <- rank(CM.ICE$Growth.rate.cmperday)
CM.ICELAND <- cbind ( CM.ICE, CM.ICE.rank)
ranked.CM.ICE.aov <- aov(CM.ICE.rank ~ Condition, data = CM.ICELAND)
Anova(ranked.CM.ICE.aov, type = 3)
TukeyHSD(ranked.CM.ICE.aov)
