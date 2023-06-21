library('tidyverse')

#note binge drinking quesiton didn't have a never option until 11:30 on Thursday 22.10.2020

df=read_csv('data/study1a/stim_norming_data_anonymized.csv',col_types=cols())

# columns with image-specific questions
img_cols=which(grepl('https:',as.character(df[1,])))
names(df)[img_cols]<-paste0(names(df)[img_cols],'_',as.character(df[1,img_cols]))


df = df %>%
   slice(3:dim(.)[1]) %>%
   filter(real_pilot_ppt=='1',Finished==1,icf==1) %>%
   mutate(`Duration (in minutes)`=as.numeric(`Duration (in seconds)`)/60) %>%
   filter(!is.na(pIDs)) 

# participant data
ppt_df= df %>% select(ResponseId,pIDs,StartDate,EndDate,Finished,`Duration (in minutes)`,gender,age,drinking_freq,binge_drinking,Insta) %>%
        mutate(gender=as.numeric(gender),
               age=as.numeric(age))

# note: 2 people seem to have done the survey twice:
ppt_df[duplicated(ppt_df$pIDs),]
#'50' looks like they actually filled it out twice and both seem like ok responses - keeping both
#'28' looks like they actually filled it out twice and both seem like ok responses - keeping both

#get ground truth (rated by Christin) people on image
msg_ratings=read_csv('data/study1a/stims_for_pretest_utf.csv',col_types=cols()) %>% select(filename,people) 

# Get message ids
msg_ids=read_csv('data/study1a/Qualtrics_IDs_URLs.csv',col_types=cols()) %>%
  mutate(Qualtrics_ID=sub('IM_','',Qualtrics_ID),
         condition=case_when(grepl('pp',filename)~'pro_prof',
                             grepl('ap',filename)~'anti_prof',
                             grepl('ps',filename)~'pro_social',
                             grepl('as',filename)~'anti_social',
                             TRUE~NA_character_)) %>%
  select(Qualtrics_ID,filename,condition) %>%
  left_join(msg_ratings) %>%
  mutate(people=as.numeric(people),
         attention_ground_truth=case_when(people==0~1,
                                          people==1~2,
                                          people>1 & people<6~3,
                                          people>5 & people<11~4,
                                          people>10~5,
                                          TRUE~NA_real_))

# drink data
drink_df = df %>% pivot_longer(values_to='drink_rating',names_to = 'variable', cols=names(df)[grepl('drink_',names(df))]) %>%
           select(ResponseId,pIDs,variable,drink_rating) %>%
           mutate(variable=sub(' - \\[Field-2\\]','',variable),
                  drink_rating=as.numeric(drink_rating)) %>%
           separate(variable,into=c('QualtricsMsgNr','variable','QualtricsBaseLink','QualtricsMsgID'),sep='_') %>%
           filter(!is.na(drink_rating)) %>%
           select(-variable,-QualtricsBaseLink) %>%
           left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) 

# attention data
attention_df = df %>% pivot_longer(values_to='attention_rating',names_to = 'variable', cols=names(df)[grepl('attention_',names(df))]) %>%
  select(ResponseId,pIDs,variable,attention_rating) %>%
  mutate(variable=sub(' - \\[Field-2\\]','',variable),
         attention_rating=as.numeric(attention_rating)) %>%
  separate(variable,into=c('QualtricsMsgNr','variable','QualtricsBaseLink','QualtricsMsgID'),sep='_') %>%
  filter(!is.na(attention_rating)) %>%
  select(-variable,-QualtricsBaseLink)

ppt_attention=attention_df %>% left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) %>% group_by(pIDs) %>%
              summarize(attention_score=sum(attention_rating==attention_ground_truth,na.rm=T)/n())

ppt_df=ppt_df %>% left_join(ppt_attention)

# emo data
emo_df = df %>% pivot_longer(values_to='emo_rating',names_to = 'variable', cols=names(df)[grepl('emo_',names(df))]) %>%
  select(ResponseId,pIDs,variable,emo_rating) %>%
  filter(!is.na(emo_rating)) %>%
  separate(variable,into=c('QualtricsMsgNr','remove','variable','QualtricsBaseLink','QualtricsMsgID'),sep='_') %>%
  separate(QualtricsMsgID,into=c('QualtricsMsgID','specific_emotion'),sep=' - ') %>%
  mutate(emo_rating=as.numeric(emo_rating)) %>%
  select(ResponseId,pIDs,QualtricsMsgNr,QualtricsMsgID,specific_emotion,emo_rating) %>%
  left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) 


# source data
source_df = df %>% pivot_longer(values_to='source_rating',names_to = 'variable', cols=names(df)[grepl('source_',names(df))]) %>%
  select(ResponseId,pIDs,variable,source_rating) %>%
  separate(variable,into=c('QualtricsMsgNr','remove','remove1','remove2','QualtricsMsgID'),sep='_') %>%
  separate(QualtricsMsgID,into=c('QualtricsMsgID','source'),sep=' - ') %>%
  filter(!is.na(source_rating)) %>%
  mutate(source_rating=as.numeric(source_rating)) %>%
  select(ResponseId,pIDs,QualtricsMsgNr,QualtricsMsgID,source,source_rating) %>%
  left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) 



# valence_data
valence_df = df %>% pivot_longer(values_to='valence_rating',names_to = 'variable', cols=names(df)[grepl('pro_anti_',names(df))]) %>%
  select(ResponseId,pIDs,variable,valence_rating) %>%
  separate(variable,into=c('QualtricsMsgNr','remove','remove1','remove2','remove3','QualtricsMsgID'),sep='_') %>%
  separate(QualtricsMsgID,into=c('QualtricsMsgID','valence'),sep=' - ') %>%
  filter(!is.na(valence_rating)) %>%
  mutate(valence_rating=as.numeric(valence_rating)) %>%
  select(ResponseId,pIDs,QualtricsMsgNr,QualtricsMsgID,valence,valence_rating) %>%
  left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) 


# familiarity
familiarity_df = df %>% pivot_longer(values_to='fam_rating',names_to = 'variable', cols=names(df)[grepl('fam_',names(df))]) %>%
  select(ResponseId,pIDs,variable,fam_rating) %>%
  separate(variable,into=c('QualtricsMsgNr','remove','remove1','remove2','QualtricsMsgID'),sep='_') %>%
  separate(QualtricsMsgID,into=c('QualtricsMsgID','familiarity_variable'),sep=' - ') %>%
  filter(!is.na(fam_rating)) %>%
  mutate(fam_rating=as.numeric(fam_rating)) %>%
  select(ResponseId,pIDs,QualtricsMsgNr,QualtricsMsgID,familiarity_variable,fam_rating) %>%
  left_join(msg_ids,by=c('QualtricsMsgID'='Qualtrics_ID')) 

# Save out datastructures
saveRDS(drink_df,'data/study1a/cleaned/drink_df.RDS')
saveRDS(attention_df,'data/study1a/cleaned/attention_df.RDS')
saveRDS(emo_df,'data/study1a/cleaned/emo_df.RDS')
saveRDS(source_df,'data/study1a/cleaned/source_df.RDS')
saveRDS(valence_df,'data/study1a/cleaned/valence_df.RDS')
saveRDS(familiarity_df,'data/study1a/cleaned/familiarity_df.RDS')
saveRDS(ppt_df,'data/study1a/cleaned/ppt_df.RDS')

print('COMPLETE')