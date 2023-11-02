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


study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS')
study1a_ppts=readRDS('data/study1a/cleaned/ppt_df.RDS')
study2_logs=read_csv('data/study2/cleaned/logs_cleaned.csv',col_types=cols())
study2_ppts=read_csv('data/study2/cleaned/S1_cleaned.csv') 

s1_craving_model_main<-study1a_drink_df %>% 
  left_join(study1a_ppts) %>% 
  #mutate(gender_char=fct_relevel(gender_char,'male',after=0),
  #       age=scale(age,scale=FALSE),
  #       binge_drinking_rev=as.numeric(binge_drinking_rev),
  #       drinking_freq_rev=as.numeric(drinking_freq_rev),
  #       binge_drinking_rev=scale(binge_drinking_rev,scale=FALSE),
  #       drinking_freq_rev=scale(drinking_freq_rev,scale=FALSE))%>%
  lmer(drink_rating_z~val_cond+source_cond + (1|pIDs)+(1|QualtricsMsgID),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))

craving_table_main<-study2_logs %>% 
  left_join(study2_ppts) %>% 
  #mutate(gender_char=fct_relevel(gender_char,'male',after=0),
  #       age=scale(age,scale=FALSE),
  #       AUDIT_score=scale(AUDIT_score,scale=FALSE))%>%
  lmer(rating.keys_z~val_cond+source_cond + val_cond*type +(1|pID)+(1|file),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 2',.before=1) %>%
  mutate(study=2, group=ifelse(group=='file','sID',group)) %>%
  rbind(s1_craving_model_main,.) %>%
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

craving_table_main[is.na(craving_table_main)]<-''
craving_table_main$term=c('Intercept','Valence: pro-alcohol','Source: professional','Cue Type: non-alcoholic', 'Valence x Cue Type','pID Intercept','sID Intercept','Residual')

craving_table_main=craving_table_main %>% mutate(p.value_1=ifelse(p.value_1!='',paste0('p = ',p.value_1),p.value_1),
                                 p.value_2=ifelse(p.value_2!='',paste0('p = ',p.value_2),p.value_2),
                                 `Study 1a`=ifelse(p.value_1!='',paste0(estimate_1,' ',`95% CI_1`,', ',p.value_1),estimate_1),
                                 `Study 2`=ifelse(p.value_2!='',paste0(estimate_2,' ',`95% CI_2`,', ',p.value_2),estimate_2),
                                 effect=ifelse(effect=='ran_pars','random',effect)) %>%
          select(effect,term,`Study 1a`, `Study 2`) 

write_csv(craving_table_main,'Tables/cravingTableMain.csv')





s1_craving_model_inter<-study1a_drink_df %>% 
  left_join(study1a_ppts) %>% 
  #mutate(gender_char=fct_relevel(gender_char,'male',after=0),
  #       age=scale(age,scale=FALSE),
  #       binge_drinking_rev=as.numeric(binge_drinking_rev),
  #       drinking_freq_rev=as.numeric(drinking_freq_rev),
  #       binge_drinking_rev=scale(binge_drinking_rev,scale=FALSE),
  #       drinking_freq_rev=scale(drinking_freq_rev,scale=FALSE))%>%
  lmer(drink_rating_z~val_cond*source_cond + (1|pIDs)+(1|QualtricsMsgID),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))

craving_table_inter<-study2_logs %>% 
  left_join(study2_ppts) %>% 
  #mutate(gender_char=fct_relevel(gender_char,'male',after=0),
  #       AUDIT_score=scale(AUDIT_score,scale=FALSE))%>%
  lmer(rating.keys_z~val_cond*source_cond + val_cond*type + (1|pID)+(1|file),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 2',.before=1) %>%
  mutate(study=2, group=ifelse(group=='file','sID',group)) %>%
  rbind(s1_craving_model_inter,.) %>%
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

craving_table_inter[is.na(craving_table_inter)]<-''
craving_table_inter = craving_table_inter %>% mutate(term=c('Intercept','Valence: pro-alcohol','Source: professional','Valence x Source','Cue Type: non-alcoholic','Valence x Cue Type','pID Intercept','sID Intercept','Residual'),
                                                     order=c(1,2,3,5,4,6,7,8,9)) %>%
                      arrange(order) %>% select(-order)

craving_table_inter=craving_table_inter %>% mutate(p.value_1=ifelse(p.value_1!='',paste0('p = ',p.value_1),p.value_1),
                                                 p.value_2=ifelse(p.value_2!='',paste0('p = ',p.value_2),p.value_2),
                                                 `Study 1a`=ifelse(p.value_1!='',paste0(estimate_1,' ',`95% CI_1`,', ',p.value_1),estimate_1),
                                                 `Study 2`=ifelse(p.value_2!='',paste0(estimate_2,' ',`95% CI_2`,', ',p.value_2),estimate_2),
                                                 effect=ifelse(effect=='ran_pars','random',effect)) %>%
  select(effect,term,`Study 1a`, `Study 2`)

write_csv(craving_table_inter,'Tables/cravingTableInter.csv')