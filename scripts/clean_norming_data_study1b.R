library('tidyverse')

df=read_csv('data/study1b/stim_norming_Nonalc_data_anonymized.csv',col_types=cols())

# columns with image-specific questions
img_cols=which(grepl('https:',as.character(df[1,])))
names(df)[img_cols]<-paste0(names(df)[img_cols],'_',as.character(df[1,img_cols]))


df = df %>%
   slice(3:dim(.)[1]) %>%
   filter(Finished==1,icf==1) %>%
   mutate(`Duration (in minutes)`=as.numeric(`Duration (in seconds)`)/60) %>%
   filter(!is.na(pIDs)) %>%
   mutate(pIDs=paste0(pIDs,'_study1b'))

# participant data
ppt_df= df %>% select(ResponseId,pIDs,StartDate,EndDate,Finished,`Duration (in minutes)`,gender,age,drinking_freq,binge_drinking,Insta) %>%
        mutate(gender=as.numeric(gender),
               age=as.numeric(age))

# no duplicate participants
ppt_df[duplicated(ppt_df$pIDs),]

#get ground truth (rated by Christin) people on image
msg_ratings=read_csv('data/study1b/nonalc_stims_for_pretest.csv',col_types=cols()) %>% select(filename,people) 

# Get message ids
msg_ids=read_csv('data/study1b/QualtricsURLs_allstims.csv',col_types=cols()) %>% 
  mutate(condition=case_when(grepl('npp',filename)~'non_pro_prof',
                             grepl('nps',filename)~'non_pro_social',
                             grepl('ap',filename)~'anti_prof',
                             grepl('pp',filename)~'pro_prof',
                             grepl('as',filename)~'anti_social',
                             grepl('ps',filename)~'pro_social',
                             TRUE~NA_character_)) %>%
  select(QualtricsURL,filename,condition) %>%
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
                  variable=sub('alc[[:digit:]]_','',variable),
                  variable=sub('drink_non','',variable),
                  variable=sub('drink','',variable),
                  variable=sub('[[:digit:]]+_','',variable),
                  variable=sub('[[:digit:]]_','',variable),
                  QualtricsURL=sub('_h','h',variable),
                  drink_rating=as.numeric(drink_rating)) %>% 
           filter(!is.na(drink_rating)) %>%
           select(-variable) %>% 
           left_join(msg_ids) %>%
           #zscore craving
           mutate(drink_rating_z =scale(drink_rating))

# emo data
emo_df = df %>% pivot_longer(values_to='emo_rating',names_to = 'variable', cols=names(df)[grepl('emo_',names(df))]) %>%
  select(ResponseId,pIDs,variable,emo_rating) %>%
  filter(!is.na(emo_rating)) %>% 
  mutate(variable=sub('\\[Field-2\\] - ','',variable),
         variable=sub('alc[[:digit:]]_','',variable),
         variable=sub('emo_non','',variable),
         variable=sub('emo','',variable),
         variable=sub('[[:digit:]]+_','',variable),
         variable=sub('[[:digit:]]_','',variable),
         variable=sub('[[:digit:]]_','',variable),
         variable=sub('_h','h',variable),
         variable=sub('_h','h',variable),
         emo_rating=as.numeric(emo_rating)) %>% 
  separate(variable,into = c('QualtricsURL','specific_emotion'),sep=" - ") %>% 
  select(ResponseId,pIDs,QualtricsURL,specific_emotion,emo_rating) %>% 
  left_join(msg_ids)  %>%
  filter(specific_emotion %in% c('positive','negative'))


# source data
source_df = df %>% pivot_longer(values_to='source_rating',names_to = 'variable', cols=names(df)[grepl('source_',names(df))]) %>%
  select(ResponseId,pIDs,variable,source_rating) %>%
  separate(variable,into=c('remove', 'QualtricsURL','source'),sep=' - ') %>% 
  filter(!is.na(source_rating)) %>%
  mutate(source_rating=as.numeric(source_rating)) %>%
  select(ResponseId,pIDs,QualtricsURL,source,source_rating) %>%
  left_join(msg_ids) 

# familiarity
familiarity_df = df %>% pivot_longer(values_to='fam_rating',names_to = 'variable', cols=names(df)[grepl('fam_',names(df))]) %>%
  select(ResponseId,pIDs,variable,fam_rating) %>% 
  separate(variable,into=c('remove', 'QualtricsURL','familiarity_variable'),sep=' - ') %>%   
  filter(!is.na(fam_rating)) %>%
  mutate(fam_rating=as.numeric(fam_rating)) %>%
  select(ResponseId,pIDs,QualtricsURL,familiarity_variable,fam_rating) %>%
  left_join(msg_ids) 

# Clean ppt_df

ppt_df = ppt_df %>% 
  mutate_at(c('binge_drinking','drinking_freq'),as.numeric) %>%
  mutate(gender_char=case_when(gender==1~'male',
                               gender==2~'female',
                               gender==3~'non-binary',
                               gender==4~'prefer to self-describe',
                               gender==5~'prefer not to say'),
         drinking_freq_char=case_when(drinking_freq==1~'Every day', 
                                      drinking_freq==2~'5 to 6 times a week',
                                      drinking_freq==3~'3 to 4 times a week',
                                      drinking_freq==4~'twice a week',
                                      drinking_freq==5~'once a week',
                                      drinking_freq==6~'2 to 3 times a month',
                                      drinking_freq==7~'once a month',
                                      drinking_freq==8~'3 to 11 times in the past year',
                                      drinking_freq==9~'1 to 2 times in the past year',
                                      drinking_freq==10~'I did not drink any alcohol in the past year, but I did drink in the past.',
                                      drinking_freq==11~'I never drank any alcohol in my life',
                                      TRUE~NA_character_),
         binge_drinking_char=case_when(binge_drinking==1~'Every day', 
                                       binge_drinking==2~'5 to 6 days a week',
                                       binge_drinking==3~'3 to 4 days a week',
                                       binge_drinking==4~'two days a week',
                                       binge_drinking==5~'one day a week',
                                       binge_drinking==6~'2 to 3 days a month',
                                       binge_drinking==7~'onc day a month',
                                       binge_drinking==8~'3 to 11 days in the past year',
                                       binge_drinking==9~'1 to 2 days in the past year',
                                       binge_drinking==10~'Never',
                                       TRUE~NA_character_),
         binge_drinking_rev=case_when(binge_drinking==10~1,
                                      binge_drinking==9~2,
                                      binge_drinking==8~3,
                                      binge_drinking==7~4,
                                      binge_drinking==6~5,
                                      binge_drinking==5~6,
                                      binge_drinking==4~7,
                                      binge_drinking==3~8,
                                      binge_drinking==2~9,
                                      binge_drinking==1~10,
                                      TRUE~binge_drinking),
         drinking_freq_rev=case_when(drinking_freq==11~1,
                                     drinking_freq==10~2,
                                     drinking_freq==9~3,
                                     drinking_freq==8~4,
                                     drinking_freq==7~5,
                                     drinking_freq==6~6,
                                     drinking_freq==5~7,
                                     drinking_freq==4~8,
                                     drinking_freq==3~9,
                                     drinking_freq==2~10,
                                     drinking_freq==1~11,
                                     TRUE~NA_real_))

# Split conditions 

drink_df=drink_df %>% mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                                               grepl('anti',condition)~'anti-alcohol',
                                               grepl('pro_',condition)~'pro-alcohol'),
                             val_cond=as.factor(val_cond),
                                           source_cond=case_when(grepl('prof',condition)~'professional',
                                                                 grepl('soc',condition)~'peer-produced',
                                                                 TRUE~NA_character_),
                                           source_cond=factor(source_cond))

source_df<- source_df %>% mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                                                                   grepl('anti',condition)~'anti-alcohol',
                                                                   grepl('pro_',condition)~'pro-alcohol'),
                                                val_cond=as.factor(val_cond),
                                                source_cond=case_when(grepl('prof',condition)~'professional',
                                                                      grepl('soc',condition)~'peer-produced',
                                                                      TRUE~NA_character_),
                                                source_cond=factor(source_cond))



# Save out datastructures
saveRDS(drink_df,'data/study1b/cleaned/drink_df.RDS')
saveRDS(emo_df,'data/study1b/cleaned/emo_df.RDS')
saveRDS(source_df,'data/study1b/cleaned/source_df.RDS')
saveRDS(familiarity_df,'data/study1b/cleaned/familiarity_df.RDS')
saveRDS(ppt_df,'data/study1b/cleaned/ppt_df.RDS')

