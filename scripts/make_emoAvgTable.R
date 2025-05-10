library('tidyverse')
library('broom.mixed')
library('lmerTest')

formatp <- function(p_value){
  formatted_p <- ifelse(p_value<0.001,'<.001',as.character(format(round(p_value,3),nsmall=3)))
  return(formatted_p)
}

round_format=function(vec,digits=2){
  format(round(vec,digits),nsmall=digits)
}


study1a_emo_df=readRDS('data/study1a/cleaned/emo_df.RDS') %>% mutate(Study='Study 1a')
study1b_emo_df=readRDS('data/study1b/cleaned/emo_df.RDS') %>% mutate(Study='Study 1b') %>% mutate(emo_rating=as.numeric(emo_rating))

emo_1ab=bind_rows(study1a_emo_df,study1b_emo_df) %>% pivot_wider(names_from=specific_emotion,values_from=emo_rating) %>%
  mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                            grepl('anti',condition)~'anti-alcohol',
                            grepl('pro',condition)~'pro-alcohol',
                            TRUE~NA_character_))

positive_Avg=emo_1ab %>% 
  filter(val_cond!='non-alcoholic') %>%
  lmer(positive~val_cond+(1|pIDs)+(1|filename)+(1|Study),data=.,control = lmerControl(optimizer='bobyqa')) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(group=ifelse(group=='pIDs','pID',
                              ifelse(group=='filename','sID',group))) %>%
  mutate(p.value=case_when(is.na(p.value)~NA_character_,
                           p.value>=0.001~paste0('p = ',round(p.value,3)),
                           p.value<0.001~paste0('p < .001'),
                           TRUE~NA_character_),
         Estimate=paste0('B = ',round(estimate,2),', 95\\%CI [',round(conf.low,2),', ',round(conf.high,2),'], ',p.value),
         Estimate=ifelse(grepl('NA',Estimate),estimate,Estimate))%>%
  dplyr::select(effect,term,Estimate) %>%
  write_csv(.,'Tables/EmoAvgpos_table.csv')

negative_Avg=emo_1ab %>% 
  filter(val_cond!='non-alcoholic') %>%
  lmer(negative~val_cond+(1|pIDs)+(1|filename)+(1|Study),data=.,control = lmerControl(optimizer='bobyqa')) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(group=ifelse(group=='pIDs','pID',
                      ifelse(group=='filename','sID',group)))  %>%
  mutate(p.value=case_when(is.na(p.value)~NA_character_,
                           p.value>=0.001~paste0('p = ',round(p.value,3)),
                           p.value<0.001~paste0('p < .001'),
                           TRUE~NA_character_),
         Estimate=paste0('B = ', round(estimate,2),', 95\\%CI [',round(conf.low,2),', ',round(conf.high,2),'], ',p.value),
         Estimate=ifelse(grepl('NA',Estimate),estimate,Estimate))%>%
  dplyr::select(effect,term,Estimate) %>%
  write_csv(.,'Tables/EmoAvgneg_table.csv')





