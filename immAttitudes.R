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
attr(ivs$G038, "labels")
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

attributes(ivs$F028)
table(ivs$F028)
# attendance -- needs to be converted

attributes(ivs$F025)
# religion

attributes(ivs$F034)
table(ivs$F034)
# religious person

table(ivs$unemployed)
ivs$unemployed <- ifelse(ivs$X028 == 7, 1, 0)

attributes(ivs$C006)
# increasing in satisfaction with household's financial situation

# Select the desired columns from the 'ivs' data
ivsSmall <- ivs[,c("S003", "S020", "A124_06","C002","E143","G038","G040", "G041", "G052", "F034", "F028", "C006", "unemployed")]
colnames(ivsSmall)<-c("code", "year","nbr","jobPri","policy","lessJobs","crime", "welfareStrain", "posImpact", "reliPerson", "attend", "satisfied", "unemployed")

# religious person variable
ivsSmall$binaryReliPerson <- ifelse(ivsSmall$reliPerson == 1, 1, 0)

# attend
ivsSmall <- ivsSmall %>% mutate(attend = case_match(attend,
                                            1~100,
                                            2~52,
                                            3~12,
                                            4~4,
                                            5~2,
                                            6~1,
                                            7~0.5,
                                            8~0))

# Remap variables to increase with pro-immigration attitudes

# 0 do not want immigrants as neighbors
ivsSmall <- ivsSmall %>% mutate(nbr = case_match(nbr, 0~1, 1~0))

# 0 = citizens should have job priority over immigrants
# 1 = no job priority
# 0.5 = no opinion
ivsSmall <- ivsSmall %>% mutate(jobPri = case_when(jobPri == 1 ~ 0,
                                                   jobPri == 2 ~ 1,
                                                   jobPri == 3 ~ 0.5,
                                                   TRUE ~ jobPri))  

ivsSmall <- ivsSmall %>% mutate(policy = case_match(policy, 1~1, 2~0.66, 3~0.33, 4~0))

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
table(ivsSmall$year)

# this represents an index of immigration attitudes

countryImmViews <- ivsSmall %>% group_by(code) %>% 
  summarise(nbr = mean(nbr, na.rm = TRUE),
            jobPri = mean(jobPri, na.rm = TRUE),
            policy = mean(policy, na.rm = TRUE),
            lessJobs = mean(lessJobs, na.rm = TRUE),
            crime = mean(crime, na.rm = TRUE),
            welfareStrain = mean(welfareStrain, na.rm = TRUE),
            posImpact = mean(posImpact, na.rm = TRUE),
            binaryReliPerson = mean(binaryReliPerson, na.rm = TRUE),
            attend = mean(attend, na.rm = TRUE),
            satisfied = mean(satisfied, na.rm = TRUE),
            unemployed = mean(unemployed, na.rm = TRUE)
  )

countryImmViews <- countryImmViews %>%
  mutate(overallAttitude = rowMeans(select(., nbr, jobPri, policy, lessJobs, crime, welfareStrain, posImpact), na.rm = TRUE))

# Replace "NaN" values with NA in 'countrydata'
countryImmViews[countryImmViews == "NaN"] <- NA

length(unique(countryImmViews$code))

macrodata<-readRDS("macrodata.RDS")

countryImmViews <- merge(countryImmViews, macrodata)
countryImmViews2022 <- countryImmViews[countryImmViews$year == 2022, ]

theme_set(theme_tufte())

# Overall Attitude by country
ggplot(countryImmViews2022, aes(x = code, y = overallAttitude, label=code)) +
  theme(axis.ticks.x=element_blank(),axis.text.x=element_blank())+
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(y = "Overall Attitude", x = "")

# attitude vs. log gdp per capita
ggplot(countryImmViews2022, aes(x = lgdppc, y = overallAttitude, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = "Log GDP per Capita", y = "Overall Attitude")+
  geom_smooth(method = "lm")

# religious person
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = overallAttitude, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = '% IDing as "religious person"', y = "Overall Attitude")+
  geom_smooth(method = "lm")

# attend
ggplot(countryImmViews2022, aes(x = attend, y = overallAttitude, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Overall Attitude")+
  geom_smooth(method = "lm")

# satisfied
ggplot(countryImmViews2022, aes(x = satisfied, y = overallAttitude, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Satisfaction with Financial Situation', y = "Overall Attitude")+
  geom_smooth(method = "lm")

# unemployed
ggplot(countryImmViews2022, aes(x = unemployed, y = overallAttitude, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Unemployed', y = "Overall Attitude")+
  geom_smooth(method = "lm")

### plot individual variables in index ###

# plot attendance 

# more attendance, more job priority for citizens
ggplot(countryImmViews2022, aes(x = attend, y = jobPri, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Job Equality")+
  geom_smooth(method = "lm")

# more attendance, slightly less acceptance of immigrant neighbors
ggplot(countryImmViews2022, aes(x = attend, y = nbr, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Acceptance of Immigrant Neighbors")+
  geom_smooth(method = "lm")

# straight line
ggplot(countryImmViews2022, aes(x = attend, y = policy, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Flexibility of Immigration Policy")+
  geom_smooth(method = "lm")

# more attendance, more belief in immigrants affecting crime
ggplot(countryImmViews2022, aes(x = attend, y = crime, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Belief in Immigrants Not Increasing Crime")+
  geom_smooth(method = "lm")

# more attendance, more belief in immigrants taking away jobs
ggplot(countryImmViews2022, aes(x = attend, y = lessJobs, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Immigrants Do Not Take Away Jobs")+
  geom_smooth(method = "lm")

# more attendance, more belief in immigrants being a welfare strain
ggplot(countryImmViews2022, aes(x = attend, y = welfareStrain, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Immigrants are not a welfare strain")+
  geom_smooth(method = "lm")

# more attendance, less belief in immigrants having a positive impact 
ggplot(countryImmViews2022, aes(x = attend, y = posImpact, label=code)) +
  geom_point(col="blue",alpha=0.5)+geom_text(hjust=-0.1,vjust=-0.1)+
  labs(x = 'Religious Attendance', y = "Immigrants have a positive impact")+
  geom_smooth(method = "lm")


# gdp per capita

# increase in gdp per capita, increase in belief that there should be no job priority
ggplot(countryImmViews2022, aes(x = lgdppc, y = jobPri, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Job Equality") +
  geom_smooth(method = "lm")

# increase in gdp per capita, increase in acceptance of immigrant neighbors
ggplot(countryImmViews2022, aes(x = lgdppc, y = nbr, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Acceptance of Immigrant Neighbors") +
  geom_smooth(method = "lm")

# increase in gdp per capita, decrease in flexibility of immigration policy
ggplot(countryImmViews2022, aes(x = lgdppc, y = policy, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Flexibility of Immigration Policy") +
  geom_smooth(method = "lm")

# increase in gdp per capita, increase in belief of immigrants causing crime
ggplot(countryImmViews2022, aes(x = lgdppc, y = crime, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Belief in Immigrants Not Increasing Crime") +
  geom_smooth(method = "lm")

# increase in gdp per capita, immigrants don't take away jobs
ggplot(countryImmViews2022, aes(x = lgdppc, y = lessJobs, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Immigrants Do Not Take Away Jobs") +
  geom_smooth(method = "lm")

# increase in gdp per capita, belief in immigrants being a welfare strain
ggplot(countryImmViews2022, aes(x = lgdppc, y = welfareStrain, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Immigrants are not a welfare strain") +
  geom_smooth(method = "lm")

# increase in gdp per capita, increase in belief of immigrants with a positive impact
ggplot(countryImmViews2022, aes(x = lgdppc, y = posImpact, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'GDP per Capita', y = "Immigrants have a positive impact") +
  geom_smooth(method = "lm")

# religious person
# more religious, more job priority
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = jobPri, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Job Equality") +
  geom_smooth(method = "lm")

# no correlation
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = nbr, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Acceptance of Immigrant Neighbors") +
  geom_smooth(method = "lm")

# no correlation
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = policy, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Flexibility of Immigration Policy") +
  geom_smooth(method = "lm")

# more religious, more belief in immigrants not increasing crime
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = crime, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Belief in Immigrants Not Increasing Crime") +
  geom_smooth(method = "lm")

# more religious, more belief in immigrants taking away jobs
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = lessJobs, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Immigrants Do Not Take Away Jobs") +
  geom_smooth(method = "lm")

# more religious, more belief in immigrants not being a welfare strain
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = welfareStrain, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Immigrants are not a welfare strain") +
  geom_smooth(method = "lm")

# more religious, less belief in immigrants having a positive impact
ggplot(countryImmViews2022, aes(x = binaryReliPerson, y = posImpact, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = '% IDing as "religious person', y = "Immigrants have a positive impact") +
  geom_smooth(method = "lm")


# satisfaction with financial situation

# increase in satisfaction, increase in no job priority
ggplot(countryImmViews2022, aes(x = satisfied, y = jobPri, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Job Equality") +
  geom_smooth(method = "lm")

# not much correlation
ggplot(countryImmViews2022, aes(x = satisfied, y = nbr, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Acceptance of Immigrant Neighbors") +
  geom_smooth(method = "lm")

# increase in x, associated with decrease in flexibility of policy
ggplot(countryImmViews2022, aes(x = satisfied, y = policy, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Flexibility of Immigration Policy") +
  geom_smooth(method = "lm")

# increase in satisfaction, more belief in immigrants causing crime
ggplot(countryImmViews2022, aes(x = satisfied, y = crime, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Belief in Immigrants Not Increasing Crime") +
  geom_smooth(method = "lm")

# increase in satisfaction, more belief in immigrants not taking away jobs
ggplot(countryImmViews2022, aes(x = satisfied, y = lessJobs, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Immigrants Do Not Take Away Jobs") +
  geom_smooth(method = "lm")

# increase in financial satisfaction, belief immigrants are a welfare strain
ggplot(countryImmViews2022, aes(x = satisfied, y = welfareStrain, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Immigrants are not a welfare strain") +
  geom_smooth(method = "lm")

# increase in financial satisfaction, greater belief immigrants have a positive impact
ggplot(countryImmViews2022, aes(x = satisfied, y = posImpact, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Satisfaction with Financial Situation', y = "Immigrants have a positive impact") +
  geom_smooth(method = "lm")


# unemployment
# increase in job priority beliefs
ggplot(countryImmViews2022, aes(x = unemployed, y = jobPri, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Job Equality") +
  geom_smooth(method = "lm")

# no correlation really
ggplot(countryImmViews2022, aes(x = unemployed, y = nbr, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Acceptance of Immigrant Neighbors") +
  geom_smooth(method = "lm")

# slight increase in flexibility of immigration policy
ggplot(countryImmViews2022, aes(x = unemployed, y = policy, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Flexibility of Immigration Policy") +
  geom_smooth(method = "lm")

# less belief in immigrants increasing crime
ggplot(countryImmViews2022, aes(x = unemployed, y = crime, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Belief in Immigrants Not Increasing Crime") +
  geom_smooth(method = "lm")

# surprisingly flat
ggplot(countryImmViews2022, aes(x = unemployed, y = lessJobs, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Immigrants Do Not Take Away Jobs") +
  geom_smooth(method = "lm")

ggplot(countryImmViews2022, aes(x = unemployed, y = welfareStrain, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Immigrants are not a welfare strain") +
  geom_smooth(method = "lm")

# immigrants are nto a welfare straing
ggplot(countryImmViews2022, aes(x = unemployed, y = posImpact, label=code)) +
  geom_point(col="blue",alpha=0.5) + geom_text(hjust=-0.1,vjust=-0.1) +
  labs(x = 'Unemployment', y = "Immigrants have a positive impact") +
  geom_smooth(method = "lm")

