################################################################################
# Title: Merged_Zip_in_Msaname_ZL
# Author: Ziqi Li
# Data: 01/11/2021
# Description: Two tables of list ranking based on 2019 Avg PRR and SD (in 105 MSA name)
# Two figures

################################################################################


setwd('Y:/2021 Spring Cornell/Project/Data/Code')
getwd()

library(statsr)
library(dplyr)
library(ggplot2)
library(stringr)

ziprent = read.csv("Rent_Zip_ZORI_AllHomesPlusMultifamily_SSA.csv")
zipprice = read.csv("Price_Zip_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_mon.csv")

# Delete columns from 1996-01 to 2013-12 in Zip Price dataset(column 10-225)
zipprice<-zipprice[,-c(10:225)]

# Merge the Zip Price and Rent dataset by "RegionName", name the new dataset Merged_Zip_ZL
Merged_Zip_ZL<-merge(zipprice,ziprent,by="RegionName")

# Select useful columns
PRR_2019 <- Merged_Zip_ZL[,c(1:8,70:81,93:95,156:167)]

# Calulate PRR for each month
PRR_2019[paste('2019PRR',1:12,sep="-")]<-PRR_2019[9:20]/(12*PRR_2019[24:35])


# Calculate Avg PRR of 2019 across zip codes
PRR_2019<-PRR_2019%>%
  mutate(Avg_PRR=rowMeans(PRR_2019[,36:47],na.rm=TRUE))


# Table of PRR
Mean_PRR_2019<-PRR_2019%>%
  group_by(MsaName)%>%
  summarise(Mean_PRR=mean(Avg_PRR,na.rm=TRUE),SD_PRR=sd(Avg_PRR,na.rm=TRUE),
            Min_PRR=min(Avg_PRR,na.rm=TRUE),Max_PRR=max(Avg_PRR,na.rm=TRUE))

Mean_PRR_2019<-arrange(Mean_PRR_2019,desc(Mean_PRR))

# Table of ranking by SD of PRR  
SD_PRR_2019<-PRR_2019%>%
  group_by(MsaName)%>%
  summarise(Mean_PRR=mean(Avg_PRR,na.rm=TRUE),SD_PRR=sd(Avg_PRR,na.rm=TRUE),
            Min_PRR=min(Avg_PRR,na.rm=TRUE),Max_PRR=max(Avg_PRR,na.rm=TRUE))
SD_PRR_2019<-arrange(Mean_PRR_2019,desc(SD_PRR))
write.csv(SD_PRR_2019,"Y:/2021 Spring Cornell/Project/Data/Code/SD PRR 2019.csv")
write.csv(Mean_PRR_2019,"Y:/2021 Spring Cornell/Project/Data/Code/Mean PRR 2019.csv")            
