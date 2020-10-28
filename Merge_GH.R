# Merge MB1T data for analysis including preprocessing
## M. S. Schreiner & M. Lippold 


# Loading required packages

library(dplyr)
library(tidyr)
library(readxl)
library(lme4)
library(readr)
library(langcog)


##Importing data for each lab individually

# Lancaster lab
df_participants <- read_excel("participants//lancslab_MB1_testretest_participants.xlsx",col_types = "text")

df_trial<-read_excel("trial//lancslab_MB1_testretest_trials.xlsx",col_types = "text")

df_all<-left_join(df_trial,df_participants,by=c("subid","test"))
df_all<-df_all[,-1]


df_all<-df_all %>% select(subid,age_days,participant_gender,method,preterm,test,trial_type,
                          trial_num,looking_time,trial_error,session_error,lang1,lab.y) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.y,Method=method,Language=lang1,Session=test)

  df_all<-df_all %>% mutate(LT=as.numeric(LT),trial_num=round(as.numeric(trial_num),digits = 0))
  
  

# Goettingen lab central fixation
df_participants <- read_excel("participants/ManyBabies 1 Individual Lab Participants Data Goettingen_Retest.xlsx")

df_participants<-df_participants %>%group_by(subid) %>%mutate(test=ifelse(age_days==max(age_days),2,1))

df_trial<-read_excel("trial/ManyBabies 1 Individual Lab Trials Data Goettingen_Retest.xlsx")
df_all_g<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_g<-df_all_g %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                          trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test,) %>% 
  mutate(LT=as.numeric(as.character(LT)))
df_all_g<-df_all_g %>% mutate(preterm="term")


# Goettingen lab HPP

df_participants <- read_excel("participants/ManyBabies 1 Individual Participants Data Goettingen HPP_Retest.xlsx")

df_trial<-read_excel("trial/Manybabies 1 Individual Lab trial Data Goettingen HPP_Retest.xlsx")


df_trial<-df_trial %>% rename(test=session) %>% mutate(test=as.numeric(test))
df_participants<-df_participants %>% mutate(test=as.numeric(test))



df_all_g2<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_g2<-df_all_g2 %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                              trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test) %>% 
  mutate(LT=as.numeric(as.character(LT)))

df_all_g2<-df_all_g2 %>% mutate(preterm="N")


## Oslo lab

df_participants <- read_excel("participants/babyling-oslo_MB1_test_retest_participant.xlsx")

df_trial<-read_excel("trial/babyling-oslo_MB1_test_retest_trial.xlsx")
df_all_o<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_o<-df_all_o %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                                trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test) %>% 
  mutate(LT=as.numeric(as.character(LT)))



# Madison lab

df_participants <- read_csv("participants/infantll-madison_MB1_testretest_participants.csv")
df_trial <-read_csv("trial/infantll-madison_MB1_testretest_trials - infantll-madison_MB1_testretest_trials.csv")


df_all_madison<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_madison<-df_all_madison %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                              trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test) %>% 
  mutate(LT=as.numeric(as.character(LT)),LT=ifelse(LT>100,LT/1000,LT))


df_all<-rbind(df_all,df_all_g)
df_all<-rbind(df_all,df_all_g2)
df_all<-rbind(df_all,df_all_o)
df_all<-rbind(df_all,df_all_madison)

# Potsdam lab

##### check if this is the latest data!

df_participants <- read_csv("participants/babylab-potsdam_MB1_testretest_participants - babylab-potsdam_MB1_testretest_participants.csv")

df_participants<-df_participants %>%group_by(subid) %>%mutate(test=ifelse(age_days==max(age_days),2,1))

df_trial<-read_csv("trial/babylab-potsdam_MB1_testretest_trials - babylab-potsdam_MB1_testretest_trials.csv")
df_trial$looking_time <- as.numeric(df_trial$looking_time)
df_all_potsdam<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_potsdam<-df_all_potsdam %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                              trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test,) %>% 
  mutate(LT=as.numeric(as.character(LT)),LT=ifelse(LT>100,LT/1000,LT))
df_all_potsdam<-df_all_potsdam %>% mutate(preterm="term")

df_all<-rbind(df_all,df_all_potsdam)


# British Infant Cog lab

df_participants <- read_excel("participants/InfantCog-UBC_MB1_testrestest_participants.xlsx")

df_participants<-df_participants %>%group_by(subid) %>%mutate(test=ifelse(age_days==max(age_days),2,1))

df_trial<-read_excel("trial/InfantCog-UBC_MB1_testretest_trials.xlsx")
df_trial<-df_trial %>% mutate(trial_type=ifelse(trial_type=="Training","training",trial_type))
df_trial$looking_time <- as.numeric(df_trial$looking_time)
df_all_UCB<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_UCB<-df_all_UCB %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,trial_type,
                                          trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=trial_type,Lab=lab.x,Method=method,Language=lang1,Session=test,) %>% 
  mutate(LT=as.numeric(as.character(LT)),Lab ="InfantCog-UBC")



df_all<-rbind(df_all,df_all_UCB)


# Brookes lab

df_participants <- read_excel("participants/Brookes BabyLab MB1_retest_participants_new.xlsx")


df_trial<-read_delim("trial/Brookes BabyLab MB Test restest data_new.csv",";", escape_double = FALSE, trim_ws = TRUE)



df_trial<-df_trial %>% mutate(subid=gsub("\\D","",subid),
                              subid=paste("MB_",subid,""),
                              subid=gsub(" ", "",subid,fixed = TRUE)) 

df_all_Brookes<-left_join(df_trial,df_participants,by=c("subid","test"))

df_all_Brookes<-df_all_Brookes %>% select(subid,age_days,participant_gender,method,preterm,lab.x,test,training_type,
                                  trial_num,looking_time,trial_error,session_error,lang1) %>%
  rename(Age=age_days,LT=looking_time,Gender=participant_gender,
         Subject=subid,Condition=training_type,Lab=lab.x,Method=method,Language=lang1,Session=test,) %>% 
  mutate(LT=as.numeric(as.character(LT))) 
df_all<-rbind(df_all,df_all_Brookes)


write.csv2(df_all,"df_all.csv")