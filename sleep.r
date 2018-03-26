# Analysis of the physical variables that may predict sleep in animals
# by Brendan Wakefield (SFU ID: 301306925), March 21, 2018
# adapted from bloodpressure.r by C. S. Schwartz

# Avalanche-03 PC:
setwd("C:/Users/bwakefie/sfuvault/04 2018 Spring/STAT650/Asst6/")
# Brendan's Macbook Pro:
setwd("/Users/BREN/sfuvault/04 2018 Spring/STAT650/Asst6/")

options(useFancyQuotes=FALSE) # renders summary output corrects

library(car)
library(GGally)
library(ggplot2)
library(ggfortify)
library(emmeans)

sleep.data <- read.csv('sleepRAW.csv', header=TRUE, na.strings = "NA")
#names(sleep.data) <- tolower(names(sleep.data))
sleep.data[1:10,]

# names(sleep.data) <- c("Species", "logBodyMass", "BrainMass", "Lifespan", "GestationTime",
#                           "PredIndex", "SEIndex", "TotalSleep")
sleep.data$BodyMass <- log(sleep.data$BodyMass)
sleep.data$BrainMass <- log(sleep.data$BrainMass)
colnames(sleep.data)[3:4] <- c("logBodyMass", "logBrainMass")

# make the casement plot
sleep.splot <- GGally::ggpairs(sleep.data[, c("logBodyMass", "logBrainMass",
                                        "Lifespan", "GestationTime",
                                        "PredIndex", "SEIndex", "TotalSleep")])
sleep.splot
ggsave(sleep.splot, file='sleep.ScatterPlot.png',h=6, w=6, units="in", dpi=300)

############################# Look for Colinearity #############################
sink('sleep.VIF.txt', split = TRUE)
car::vif(lm(TotalSleep ~ logBodyMass + logBrainMass, data=sleep.data))
car::vif(lm(TotalSleep ~ GestationTime + Lifespan, data=sleep.data))
car::vif(lm(TotalSleep ~ logBrainMass + Lifespan, data=sleep.data))
car::vif(lm(TotalSleep ~ GestationTime + logBrainMass, data=sleep.data))
car::vif(lm(TotalSleep ~ PredIndex + SEIndex, data=sleep.data))
car::vif(lm(TotalSleep ~ logBodyMass + logBrainMass + PredIndex + SEIndex +
                         GestationTime + Lifespan, data=sleep.data))
car::vif(lm(TotalSleep ~ logBodyMass + logBrainMass + PredIndex + SEIndex + GestationTime +
                         Lifespan, data=sleep.data))
sink()

######################### Simple Regression Comparisons ########################
logBrainMass.fit <- lm(TotalSleep ~ logBrainMass, data=sleep.data)
summary(logBrainMass.fit)$coefficients
summary(logBrainMass.fit)
confint(logBrainMass.fit, level=0.95)

Gest.fit <- lm(TotalSleep ~ GestationTime, data=sleep.data)
summary(Gest.fit)$coefficients
confint(Gest.fit, level=0.95)

Life.fit <- lm(TotalSleep ~ Lifespan, data=sleep.data)
summary(Life.fit)
confint(Life.fit, level=0.95)

######################### Multiple Regression Analysis #########################
mr.fit <- lm(TotalSleep ~ logBrainMass + Lifespan + GestationTime + SEIndex +
               PredIndex, data=sleep.data)

sink('sleep.model.estimates.txt', split=TRUE)
summary(mr.fit)
confint(mr.fit, levels=0.95)
car::Anova(mr.fit, type='III')
sink()

diagplot.mr <- autoplot(mr.fit)
diagplot.mr
ggsave(diagplot.mr, file='sleep.model.DiagPlot.png', h=6, w=6, units="in",
       dpi=300)

############################### Leverage Plots #################################
levplot.mr <- car::leveragePlots(mr.fit, terms = ~.)
levplot.mr
ggsave(levplot.mr, file='sleep.model.LeveragePlot.png', h=6, w=6, units="in",
       dpi=300)