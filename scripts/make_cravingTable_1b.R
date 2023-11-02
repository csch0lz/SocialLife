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


study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS')
study1b_ppts=readRDS('data/study1b/cleaned/ppt_df.RDS')

craving_table_main<-study1b_drink_df %>% 
  left_join(study1b_ppts) %>% 
  mutate(val_cond=factor(val_cond,levels=c('non-alcoholic','anti-alcohol','pro-alcohol'))) %>%
  lmer(drink_rating_z~val_cond+
         (1|pIDs)+(1|filename),data=.) %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(group=ifelse(group=='pIDs','pID',
                      ifelse(group=='filename','sID',group))) %>%
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>%
  select(effect, group, term, estimate, `95% CI`, p.value) %>% 
  arrange(!is.na(group),!is.na(estimate)) %>% 
  mutate(term=case_when(is.na(group)~term,TRUE~paste(group,term,sep=' '))) %>%
  mutate_at(names(.)[grepl('estimate',names(.))],round_format) %>% 
  mutate_at(names(.)[grepl('estimate',names(.))],sub,pattern='NA',replacement=' ') %>% 
  select(-group) %>%
  mutate_all(as.character)

craving_table_main[is.na(craving_table_main)]<-''
craving_table_main$term=c('Intercept','Valence: anti-alcohol','Valence: pro-alcohol','sID Intercept','pID Intercept','Residual')

craving_table_main=craving_table_main %>% mutate(p.value=ifelse(p.value!='',paste0('p = ',p.value),p.value),
                                 Estimate=ifelse(p.value!='',paste0(estimate,' ',`95% CI`,', ',p.value),estimate),
                                 effect=ifelse(effect=='ran_pars','random',effect)) %>%
          select(effect,term,Estimate) 

write_csv(craving_table_main,'Tables/cravingTableMain_1b.csv')


