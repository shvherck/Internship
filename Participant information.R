# Load packages 
library(mlmRev)
library(reshape)
library(lattice)
library(car)
library(lme4)
library(leaps)
library(MASS)
library(gdata)
  # to read xlsx files

# Read data
rm(list=ls())
D <- read.xls("/Users/Shauni/Desktop/2e master 2017 - 2018/Stage/3. Data/Data_SRS_AQ.xlsx")
head(D)

names(D) <- c("Subject","Group","SRS", "AQ", "IQ", "Age", "ADOS")
head(D)

D

# Separate Groups 1 and 2
D_TD <- subset(D, Group == 1)
D_ASD <- subset(D, Group == 2)

################
# SRS analyses #
################
SRS_TD <- D_TD$SRS
SRS_ASD <- D_ASD$SRS

mean(SRS_ASD)
sd(SRS_ASD)

mean(SRS_TD)
sd(SRS_TD)

  # testing for equal variances
  var.test(SRS_TD, SRS_ASD)
    # p-value not significant -> variances are equal
  
  # t-test
  t.test(SRS_TD, SRS_ASD, var.equal = TRUE, paired = FALSE)
    # p = 0.0006194 -> TD score significantly lower than ASD

###############
# AQ analyses #
###############
AQ_TD <- D_TD$AQ
AQ_ASD <- D_ASD$AQ

mean(AQ_ASD)
sd(AQ_ASD)

mean(AQ_TD)
sd(AQ_TD)

  # testing for equal variances
  var.test(AQ_TD, AQ_ASD)
    # p-value not significant -> variances are equal
  
  # t-test
  t.test(AQ_TD, AQ_ASD, var.equal = TRUE, paired = FALSE)
    # p = 0.0001252 -> TD score significantly lower than ASD

###############
# IQ analyses #
###############
IQ_TD <- D_TD$IQ
IQ_ASD <- D_ASD$IQ

mean(IQ_ASD)
sd(IQ_ASD)

mean(IQ_TD)
sd(IQ_TD)

  # testing for equal variances
  var.test(IQ_TD, IQ_ASD)
    # p-value not significant -> variances are equal
  
  # t-test
  t.test(IQ_TD, IQ_ASD, var.equal = TRUE, paired = FALSE)
  
  ################
  # Age analyses #
  ################
  Age_TD <- D_TD$Age
  Age_ASD <- D_ASD$Age
  
  mean(Age_TD)
  sd(Age_TD)
  
  mean(Age_ASD)
  sd(Age_ASD)
  
  # testing for equal variances
  var.test(Age_TD, Age_ASD)
  # p-value not significant -> variances are equal
  
  # t-test
  t.test(Age_TD, Age_ASD, var.equal = TRUE, paired = FALSE)
  

  #################
  #     ADOS      #
  #################
ADOS_ASD <- D_ASD$ADOS
ADOS_ASD

mean(ADOS_ASD)
sd(ADOS_ASD)