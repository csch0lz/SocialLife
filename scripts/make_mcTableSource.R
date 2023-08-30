library('tidyverse')
library('broom.mixed')
library('kableExtra')
library('lmerTest')
library('flextable')

formatp <- function(p_value){
  formatted_p <- ifelse(p_value<0.001,'<.001',as.character(format(round(p_value,3),nsmall=3)))
  return(formatted_p)
}

round_format=function(vec,digits=2){
  format(round(vec,digits),nsmall=digits)
}

study1a_source_df=readRDS('data/study1a/cleaned/source_df.RDS')
study1b_source_df=readRDS('data/study1b/cleaned/source_df.RDS')

professional_mod=study1a_source_df %>% filter(grepl('professional',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsMsgID),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='professional',group=ifelse(group=='PROLIFIC_PID','pID',
                                            ifelse(group=='QualtricsMsgID','mID',group))) 

professional_mod_nonalc=study1b_source_df %>% filter(grepl('non',condition),grepl('professional',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsURL),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='professional',group=ifelse(group=='PROLIFIC_PID','pID',
                                            ifelse(group=='QualtricsURL','mID',group))) 

average_mod=study1a_source_df %>% filter(grepl('average',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsMsgID),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='average',group=ifelse(group=='PROLIFIC_PID','pID',
                                       ifelse(group=='QualtricsMsgID','mID',group))) 

average_mod_nonalc=study1b_source_df %>% filter(grepl('non',condition), grepl('average',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsURL),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='average',group=ifelse(group=='PROLIFIC_PID','pID',
                                       ifelse(group=='QualtricsURL','mID',group))) 

mc_table2=study1a_source_df %>% filter(grepl('influencer',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsMsgID),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='influencer',group=ifelse(group=='PROLIFIC_PID','pID',
                                          ifelse(group=='QualtricsMsgID','mID',group))) %>% 
  rbind(average_mod,.) %>% 
  rbind(professional_mod,.) %>% 
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>% 
  select(rating,effect, group, term, estimate, `95% CI`, p.value) 

mc_table2_nonalc=study1b_source_df %>% filter(grepl('non',condition), grepl('influencer',source)) %>% lmer(source_rating~source_cond+(1|ResponseId)+(1|QualtricsURL),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>%  
  mutate(rating='influencer',group=ifelse(group=='PROLIFIC_PID','pID',
                                          ifelse(group=='QualtricsURL','mID',group))) %>% 
  rbind(average_mod_nonalc,.) %>% 
  rbind(professional_mod_nonalc,.) %>% 
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>% 
  select(rating,effect, group, term, estimate, `95% CI`, p.value) 

mc_table2[is.na(mc_table2)] <- ''
mc_table2_nonalc[is.na(mc_table2_nonalc)]<-''
mc_table2$term=rep(c('Intercept','source: professional','pID Intercept','sID Intercept','Residual'),3)
mc_table2_nonalc$term=rep(c('Intercept','source: professional','pID Intercept','sID Intercept','Residual'),3)

empty_row=mc_table2[1,]
empty_row[1,]<-NA
empty_row_1=empty_row %>% mutate(effect='STUDY 1a', term='Alcoholic Drinks')
empty_row_2=empty_row %>% mutate(effect='STUDY 1b', term='Non-Alcoholic Drinks')

mc_table2_all<-rbind(empty_row_1,mc_table2,empty_row_2,mc_table2_nonalc) %>% select(-group)
names(mc_table2_all)[1]<-'DV'
names(mc_table2_all)[dim(mc_table2_all)[2]]<-'p'
mc_table2_all$effect=ifelse(mc_table2_all$effect=='ran_pars','random',mc_table2_all$effect)
mc_table2_all$DV = ifelse(mc_table2_all$DV=='average','peer',mc_table2_all$DV)


write_csv(mc_table2_all,'Tables/mcTableSource.csv')
