#Clean log files
library('tidyverse')
logs=read_csv('data/study2/all_logs.csv',col_types=cols())
logs=logs %>% 
  filter(pID!='pID') %>%
  mutate(alc_cue=sub('imgs/alc_cues/','',alc_cue),
         alc_cue=sub('.jpg','',alc_cue))

#Identify non alcohol cue trials in logs
all_alc_cues=read_csv('data/study2/FINAL_alc_cues.csv',col_types=cols()) %>%
  select(file,type,subtype)

logs=logs %>% left_join(all_alc_cues,by=c('alc_cue'='file')) 

#1 person did the task twice
#logs %>% group_by(pID) %>% count() %>% filter(n>88)%>%select(pID) %>% as.character()
##leaving this in here because can't determine which one was first + shouldn't matter too much

## Make new condition vars
logs = logs %>% mutate(val_cond=case_when(grepl('anti',condition)~'anti-alcohol',
                                          grepl('pro_',condition)~'pro-alcohol',
                                          TRUE~NA_character_),
                       source_cond=case_when(grepl('prof',condition)~'professional',
                                             grepl('soc',condition)~'peer-produced',
                                             TRUE~NA_character_)) %>%
        #zscore craving
        mutate(rating.keys_z=as.vector(scale(rating.keys)))

names(logs)[grepl('keys_z',names(logs))]<-'rating.keys_z'

write_csv(logs,'data/study2/cleaned/logs_cleaned.csv')




#Surveys
S1=read_csv('data/study2/Scanner_Task_Pilot_S1_anonymized.csv',col_types=cols()) %>%
  slice(3:dim(.)[1]) %>% filter(Finished==1) 
## some people did S1 multiple times because they got stuck with the task > only keep their first response
duplicated_pIDs=unique(S1$pID[duplicated(S1$pID)])
remove_duplicates<-function(duplicated_pIDs,df){
  for(p in duplicated_pIDs){
    rows=which(df$pID==p)
    rows=rows[2:length(rows)]
    df=df[-rows,]
  }
  df
}
S1=remove_duplicates(duplicated_pIDs,S1)
S1=S1 %>% filter(pID %in% logs$pID) %>%
  mutate(age=as.numeric(age),
         gender=as.numeric(gender),
         gender_char=case_when(gender==1~'male',
                               gender==2~'female',
                               gender==3~'non-binary',
                               gender==4~'prefer to self-describe',
                               gender==5~'prefer not to say',
                               TRUE~NA_character_)) %>%
  mutate_at(names(.)[grepl('AUDIT',names(.))],as.numeric) %>%
  mutate(AUDIT_score=rowSums(.[,names(.)[grepl('AUDIT',names(.))]],na.rm=T),
         AUDIT_cat=case_when(AUDIT_score>8~'high',
                             AUDIT_score<9~'low',
                             TRUE~NA_character_)) 



S2=read_csv('data/study2/Scanner_Task_Pilot_S2_anonymized.csv',col_types=cols()) %>%
  slice(3:dim(.)[1])
# Also some duplicated data in S2
duplicated_pIDs=unique(S2$pID[duplicated(S2$pID)])
S2=remove_duplicates(duplicated_pIDs,S2)

S2=S2 %>% filter(pID %in% logs$pID) %>%
  pivot_longer(values_to='debrief_rating',names_to='debrief_item',names(.)[grepl('debrief',names(.)) & !grepl('open',names(.))]) %>%
  mutate(debrief_rating=as.numeric(debrief_rating),
         debrief_item=case_when(grepl('_1',debrief_item)~'easy to understand',
                                grepl('_2',debrief_item)~"don't know what to do",
                                grepl('_3',debrief_item)~'ratings reflect preferences',
                                grepl('_4',debrief_item)~'like task',
                                grepl('_5',debrief_item)~'trouble focusing',
                                grepl('_6',debrief_item)~'too fast',
                                grepl('_7',debrief_item)~'good speed',
                                grepl('_8',debrief_item)~'interesting'))

S1=S1 %>% filter(pID %in% logs$pID)
S2=S2 %>% filter(pID %in% logs$pID)

#Add participant info to logs
logs= S1 %>% select(gender_char,age,AUDIT_score,AUDIT_cat,pID) %>% distinct() %>% right_join(logs,by=c('pID'))

# Norming Data Amsterdam Beverage Picture set
abps=read_csv('data/study2/FINAL_alc_cues.csv',col_types=cols()) %>% rename_all(paste0,'_alc_cue') %>% 
 dplyr::rename(alc_cue=file_alc_cue)

# Make message-level DF based on Pavlovia logs from the lab study
cue_df=logs %>% dplyr::group_by(alc_cue,val_cond,source_cond) %>% 
  dplyr::summarise(avg_rating=mean(rating.keys,na.rm=T),
            sd_rating=sd(rating.keys,na.rm=T))%>% 
  mutate(alc_cue=sub('.jpg','',alc_cue)) %>%left_join(abps)

write_csv(cue_df,'data/study2/cleaned/cue_df.csv')
write_csv(S1,'data/study2/cleaned/S1_cleaned.csv')
write_csv(S2,'data/study2/cleaned/S2_cleaned.csv')