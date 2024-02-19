setwd("/Users/emilyhuang/econ452/econ452p2/")
# setwd("~/Desktop/REE")

library(haven)
library(sjlabelled)
library(dplyr)
library(countrycode)
library(ggplot2)
library(ggthemes)
library(plm)
library(stargazer)

IVSvars <- readRDS("IVS_names_years.RDS")
ivs <- readRDS("ivs.RDS")

# Immigration attitude variables
# Want to make these into an index: a single variable denoting attitude towards immigrants

table(ivs$A124_06)
attributes(ivs$A124_06)
#Value of 1 means do not want immigrants as neighbors

table(ivs$C002)
attributes(ivs$C002)
# 1 = citizens should have job priority over immigrants
# 2 = no job priority
# 3 = no opinion

table(ivs$E143)
attributes(ivs$E143)
# Increasing in strictness of immigration policy

table(ivs$G038)
attributes(ivs$G038)
# Increasing in disagreement: "immigrants take away jobs"

table(ivs$G040)
attributes(ivs$G040)
# Increasing in disagreement: "immigrants make crime worse"

table(ivs$G041)
attributes(ivs$G041)
# Increasing in disagreement: "immigrants are a strain on the welfare system"

table(ivs$G052)
attributes(ivs$G052)
# Increasing in positive impact immigrants have on country

###
# Explanatory Variables

table(ivs$G026)
# 1 = mother is immigrant

table(ivs$G027)
# 1 = father is immigrant

table(ivs$G027A)
# 1 = respondent is immigrant

get_labels(ivs$F028)
# attendance -- needs to be converted

attributes(ivs$F025)
# religion

attributes(ivs$F034)
# religious person

attributes(ivs$C006)
# increasing in satisfaction with household's financial situation

# Select the desired columns from the 'ivs' data
ivsSmall <- ivs[,c("S003", "S020", "A124_06","C002","E143","G038","G040", "G041", "G052")]
colnames(ivsSmall)<-c("code", "year","nbr","jobPri","policy","lessJobs","crime", "lessWelfare", "posImpact")

# Set negative values in 'ivs2' to NA
ivsSmall[ivsSmall<0]<-NA

# Convert 'code' to 3-digit country codes using countrycode
ivsSmall$code <- countrycode(ivsSmall$code, "iso3n", "iso3c")

# choose not to include those who are indifferent to having immigrants as nbrs
# ivsSmall$nbr[ivsSmall$nbr == 3] <- NA

# Convert 'nbr' to numeric variable indicating that they want immigrants as neighbors (1) or not (0)
# ivsSmall$nbr <- as.numeric(ivsSmall$nbr == 1)

# Convert 'year' to numeric variable
ivsSmall$year <- as.numeric(ivsSmall$year)

countrydata <- ivsSmall %>% group_by(year, code) %>% 
  summarise(attend = mean(nbr, na.rm = TRUE),
            hell = mean(jobPri, na.rm = TRUE),
            work = mean(policy, na.rm = TRUE),
            lessJobs = mean(lessJobs, na.rm = TRUE),
            crime = mean(crime, na.rm = TRUE),
            lessWelfare = mean(lessWelfare, na.rm = TRUE),
            posImpact = mean(posImpact, na.rm = TRUE),
  )

# Replace "NaN" values with NA in 'countrydata'
countrydata[countrydata == "NaN"] <- NA

length(unique(countrydata$code))

macrodata<-readRDS("macrodata.RDS")

countrydata <- merge(countrydata, macrodata)
