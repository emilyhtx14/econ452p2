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
colnames(ivsSmall)<-c("code", "year","nbr","jobPri","policy","lessJobs","crime", "welfareStrain", "posImpact")

# Remap variables to increase with pro-immigration attitudes

# 0 do not want immigrants as neighbors
ivsSmall <- ivsSmall %>% mutate(nbr = case_match(nbr, 1~0))

# 0 = citizens should have job priority over immigrants
# 1 = no job priority
# 0.5 = no opinion
ivsSmall <- ivsSmall %>% mutate(jobPri = case_match(nbr, 1~0, 2~1, 3~0.5))

ivsSmall <- ivsSmall %>% mutate(policy = case_match(nbr, 1~1, 2~0.66, 3~0.33, 4~0))

# adjust scale to be max 1
ivsSmall <- ivsSmall %>% mutate(lessJobs = case_match(lessJobs, 1~0, 2~0.1, 3~0.2, 4~0.3, 5~0.4, 6~0.5, 7~0.6, 8~0.7, 9~0.8, 10~1))
ivsSmall <- ivsSmall %>% mutate(crime = case_match(crime, 1~0, 2~0.1, 3~0.2, 4~0.3, 5~0.4, 6~0.5, 7~0.6, 8~0.7, 9~0.8, 10~1))
ivsSmall <- ivsSmall %>% mutate(welfareStrain = case_match(welfareStrain, 1~0, 2~0.1, 3~0.2, 4~0.3, 5~0.4, 6~0.5, 7~0.6, 8~0.7, 9~0.8, 10~1))
ivsSmall <- ivsSmall %>% mutate(posImpact = case_match(posImpact, 1~0, 2~0.4, 3~0.6, 4~0.8, 5~1))

# Set negative values in 'ivsSmall' to NA
ivsSmall[ivsSmall<0]<-NA

# Convert 'code' to 3-digit country codes using countrycode
ivsSmall$code <- countrycode(ivsSmall$code, "iso3n", "iso3c")

# Convert 'year' to numeric variable
ivsSmall$year <- as.numeric(ivsSmall$year)

# this represents an index of immigration attitudes
countryImmViews <- ivsSmall %>% group_by(year, code) %>% 
  summarise(nbr = mean(nbr, na.rm = TRUE),
            jobPri = mean(jobPri, na.rm = TRUE),
            policy = mean(policy, na.rm = TRUE),
            lessJobs = mean(lessJobs, na.rm = TRUE),
            crime = mean(crime, na.rm = TRUE),
            welfareStrain = mean(welfareStrain, na.rm = TRUE),
            posImpact = mean(posImpact, na.rm = TRUE),
  )

# Replace "NaN" values with NA in 'countrydata'
countryImmViews[countryImmViews == "NaN"] <- NA

length(unique(countryImmViews$code))

macrodata<-readRDS("macrodata.RDS")

countryImmViews <- merge(countryImmViews, macrodata)

