library('tidyverse')
library('broom.mixed')

round_format=function(vec,digits=2){
  format(round(vec,digits),nsmall=digits)
}


study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS') %>% select(pIDs, condition, drink_rating) %>% mutate(Study='1a')
study1a_emo_df=readRDS('data/study1a/cleaned/emo_df.RDS') %>% select(pIDs, condition, emo_rating, specific_emotion) %>% mutate(Study='1a')
study1a_familiarity_df=readRDS('data/study1a/cleaned/familiarity_df.RDS')%>% select(pIDs, condition, familiarity_variable,fam_rating,filename) %>% mutate(Study='1a')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS') %>% select(pIDs, condition, drink_rating) %>% mutate(Study='1b')
study1b_emo_df=readRDS('data/study1b/cleaned/emo_df.RDS') %>% select(pIDs, condition, emo_rating, specific_emotion) %>% mutate(Study='1b',emo_rating=as.numeric(emo_rating)) 
study1b_familiarity_df=readRDS('data/study1b/cleaned/familiarity_df.RDS') %>% select(pIDs, condition, familiarity_variable,fam_rating,filename) %>% mutate(Study='1b')
study2_logs=read_csv('data/study2/cleaned/logs_cleaned.csv',col_types=cols())  %>% filter(type=='alcohol') %>%
  rename(pIDs=pID,drink_rating=rating.keys) %>% mutate(Study='2') %>% select(pIDs, condition, drink_rating, type, Study) %>%
  mutate(condition=sub('soc','social',condition))

# Craving by condition and study

drink_df=study1a_drink_df %>% bind_rows(study1b_drink_df) %>% bind_rows(study2_logs) %>%
  group_by(condition,Study) %>% summarize(M=mean(drink_rating,na.rm=T),SD=sd(drink_rating,na.rm=T)) %>%
  mutate(`Craving`=paste0(round_format(M),', ',round_format(SD))) %>% select(-M,-SD)

emo_df=study1a_emo_df %>% bind_rows(study1b_emo_df) %>% 
  group_by(condition,specific_emotion,Study) %>% summarize(M=mean(emo_rating,na.rm=T),SD=sd(emo_rating,na.rm=T)) %>%
  pivot_wider(names_from='specific_emotion',values_from=c('M','SD')) %>% 
mutate(`Positive Emotion`=paste0(round_format(M_positive),', ',round_format(SD_positive)),
       `Negative Emotion`=paste0(round_format(M_negative),', ',round_format(SD_negative))) %>%
  select(!contains('_'))

fam_df=study1a_familiarity_df %>% bind_rows(study1b_familiarity_df) %>% 
  filter(grepl('like me',familiarity_variable) | grepl('familiar with',familiarity_variable)) %>% 
  group_by(condition, Study, pIDs, filename) %>% summarize(fam_index=mean(fam_rating,na.rm=T)) %>% 
  group_by(condition,Study) %>% summarize(M=mean(fam_index,na.rm=T),SD=sd(fam_index,na.rm=T)) %>%
  mutate(`Familiarity Index`=paste0(round_format(M),', ',round_format(SD))) %>% select(-M,-SD)

descriptives_table=drink_df %>% left_join(emo_df) %>% left_join(fam_df) %>% ungroup() %>%
  mutate(`Valence`=case_when(grepl('anti',condition)~'anti-alcohol',
                                     grepl('non',condition)~'non-alcoholic',
                                     grepl('pro_',condition)~'pro-alcohol',TRUE~NA_character_),
         `Source`=case_when(grepl('prof',condition)~'professional',
                                    grepl('soc',condition)~'social', TRUE~NA_character_)) %>%
  #delete the condition column and order the other columns so that: Study, Message Valence, Message Source, everything else
  select(-condition) %>% select(Study,`Valence`,`Source`,everything())


write_csv(descriptives_table,'Tables/descriptives_table.csv')