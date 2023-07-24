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


study1a_drink_df=readRDS('../data/study1a/cleaned/drink_df.RDS')
study1a_ppts=readRDS('../data/study1a/cleaned/ppt_df.RDS')
study2_logs=read_csv('../data/study2/cleaned/logs_cleaned.csv',col_types=cols())
study2_ppts=read_csv('../data/study2/cleaned/S1_cleaned.csv') 

s1_urge_model<-study1a_drink_df %>% 
  left_join(study1a_ppts) %>% 
  lmer(drink_rating~val_cond*source_cond + age + gender_char + as.numeric(binge_drinking) + as.numeric(drinking_freq)+
         (1|pIDs)+(1|QualtricsMsgID),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))

urge_table<-study2_logs %>% 
  left_join(study2_ppts) %>% 
  filter(type=='alcohol') %>% 
  lmer(rating.keys~val_cond*source_cond + age + gender_char + AUDIT_score +(1|pID)+(1|file),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 2',.before=1) %>%
  mutate(study=2, group=ifelse(group=='file','sID',group)) %>%
  rbind(s1_urge_model,.) %>%
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>%
  select(study,effect, group, term, estimate, `95% CI`, p.value) %>% 
  pivot_wider(.,names_from = study, values_from=c('estimate','p.value',`95% CI`)) %>% 
  select(effect,group,term,names(.)[grepl('_1',names(.))],names(.)[grepl('_2',names(.))]) %>% 
  arrange(!is.na(group),!(!is.na(estimate_1) & !is.na(estimate_2))) %>% 
  mutate(term=case_when(is.na(group)~term,TRUE~paste(group,term,sep=' '))) %>%
  mutate_at(names(.)[grepl('estimate',names(.))],round_format) %>% 
  mutate_at(names(.)[grepl('estimate',names(.))],sub,pattern='NA',replacement=' ') %>% 
  select(-group) %>%
  mutate_all(as.character)

urge_table[is.na(urge_table)]<-''
urge_table$term=c('Intercept','Valence: pro-alcohol','Source: professional','Age','Gender: Male', 'ValencexSource','Gender: Non-Binary','Binge Drinking Freq.','Drinking Freq.','AUDIT','pID Intercept','sID Intercept','Residual')

urge_table=urge_table %>% mutate(p.value_1=ifelse(p.value_1!='',paste0('p = ',p.value_1),p.value_1),
                                 p.value_2=ifelse(p.value_2!='',paste0('p = ',p.value_2),p.value_2),
                                 `Study 1a`=ifelse(p.value_1!='',paste0(estimate_1,' ',`95% CI_1`,', ',p.value_1),estimate_1),
                                 `Study 2`=ifelse(p.value_2!='',paste0(estimate_2,' ',`95% CI_2`,', ',p.value_2),estimate_2)) %>%
          select(effect,term,`Study 1a`, `Study 2`) 

write_csv(urge_table,'Tables/cravingTable.csv')
#