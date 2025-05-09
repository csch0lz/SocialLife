library('tidyverse')
library('broom.mixed')
library('lmerTest')
library('emmeans')

formatp <- function(p_value){
  formatted_p <- ifelse(p_value<0.001,'<.001',as.character(format(round(p_value,3),nsmall=3)))
  return(formatted_p)
}

round_format=function(vec,digits=2){
  format(round(vec,digits),nsmall=digits)
}


study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS')
study1a_ppts=readRDS('data/study1a/cleaned/ppt_df.RDS')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS')
study1b_ppts=readRDS('data/study1b/cleaned/ppt_df.RDS')
study2_logs=read_csv('data/study2/cleaned/logs_cleaned.csv',col_types=cols())
study2_ppts=read_csv('data/study2/cleaned/S1_cleaned.csv') 

s1a_craving_model<-study1a_drink_df %>% 
  left_join(study1a_ppts) %>% 
  lmer(drink_rating_z~val_cond*source_cond + (1|pIDs)+(1|QualtricsMsgID),data=.) 

s1a_table <- s1a_craving_model%>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study='1a',group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))

s1b_craving_model<-study1b_drink_df %>% 
  left_join(study1b_ppts) %>% 
  lmer(drink_rating_z~val_cond*source_cond + (1|pIDs)+(1|filename),data=.) 

s1b_table <- s1b_craving_model %>%
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study='1b',group=ifelse(group=='pIDs','pID',
                              ifelse(group=='filename','sID',group)))

s2_craving_model<-study2_logs %>% 
  left_join(study2_ppts) %>% 
  lmer(rating.keys_z~val_cond *type+val_cond*source_cond+source_cond*type +(1|pID)+(1|file),data=.) 

craving_table = s2_craving_model %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 2',.before=1) %>%
  mutate(study='2', group=ifelse(group=='file','sID',group)) %>%
  rbind(s1b_table,.) %>%
  rbind(s1a_table,.) %>%
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>%
  select(study,effect, group, term, estimate, `95% CI`, p.value) %>% 
  pivot_wider(.,names_from = study, values_from=c('estimate','p.value',`95% CI`)) %>% 
  select(effect,group,term,names(.)[grepl('_1a',names(.))],names(.)[grepl('_1b',names(.))],names(.)[grepl('_2',names(.))]) %>% 
  arrange(effect,!grepl('Intercept',term),!is.na(group),grepl(':',term),!grepl('val',term), is.na(estimate_1b),!(is.na(estimate_1b) & is.na(estimate_1a) & is.na(estimate_2))) %>% 
  mutate(term=case_when(is.na(group)~term,TRUE~paste(group,term,sep=' '))) %>%
  mutate_at(names(.)[grepl('estimate',names(.))],round_format) %>% 
  mutate_at(names(.)[grepl('estimate',names(.))],sub,pattern='NA',replacement=' ') %>% 
  select(-group) %>%
  mutate_all(as.character)

craving_table[is.na(craving_table)]<-''
craving_table$term=c('Intercept','Valence 1: pro-alcohol','Valence 2: non-alcoholic', 'Source: professional','Cue Type: non-alcoholic', 'Valence 1 x Source', "Valence 2 x Source", "Valence 1 x Cue Type", 'Source x Cue Type','pID Intercept','sID Intercept','Residual')

craving_table=craving_table %>% mutate(p.value_1a=ifelse(p.value_1a!='',paste0('p = ',p.value_1a),p.value_1a),
                                       p.value_1b=ifelse(p.value_1b!='',paste0('p = ',p.value_1b),p.value_1b),
                                 p.value_2=ifelse(p.value_2!='',paste0('p = ',p.value_2),p.value_2),
                                 `Study 1a`=ifelse(p.value_1a!='',paste0(estimate_1a,' ',`95% CI_1a`,', ',p.value_1a),estimate_1a),
                                 `Study 1b`=ifelse(p.value_1b!='',paste0(estimate_1b,' ',`95% CI_1b`,', ',p.value_1b),estimate_1b),
                                 `Study 2`=ifelse(p.value_2!='',paste0(estimate_2,' ',`95% CI_2`,', ',p.value_2),estimate_2),
                                 effect=ifelse(effect=='ran_pars','random',effect)) %>%
          select(effect,term,`Study 1a`,`Study 1b`, `Study 2`) 

write_csv(craving_table,'Tables/cravingTable.csv')

mm_valence1b=emmeans(s1b_craving_model,specs=~val_cond)
pc_valence1b=contrast(mm_valence1b,"pairwise")

write_csv(data.frame(mm_valence1b),'Tables/craving_table_emmeans_valence_1b.csv')
write_csv(data.frame(pc_valence1b),'Tables/craving_table_pairwiseComps_valence1b.csv')

mm_source1a=emmeans(s1a_craving_model,specs=~source_cond)
pc_source1a=contrast(mm_source1a,"pairwise")
mm_source1b=emmeans(s1b_craving_model,specs=~source_cond)
pc_source1b=contrast(mm_source1b,"pairwise")
mm_source2=emmeans(s2_craving_model,specs=~source_cond)
pc_source2=contrast(mm_source2,"pairwise")

write_csv(data.frame(pc_source1a),'Tables/craving_table_pairwiseComps_source1a.csv')
write_csv(data.frame(pc_source1b),'Tables/craving_table_pairwiseComps_source1b.csv')
write_csv(data.frame(pc_source2),'Tables/craving_table_pairwiseComps_source2.csv')