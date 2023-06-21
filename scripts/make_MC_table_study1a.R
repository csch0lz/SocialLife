library('tidyverse')
library('lmerTest')
library('broom.mixed')

formatp <- function(p_value){
  formatted_p <- ifelse(p_value<0.001,'<.001',as.character(format(round(p_value,3),nsmall=3)))
  return(formatted_p)
}

#optimize output column width with shorter column names for results tables with p-values and B
optimizeColWidth<-function(table){
  names(table)=sub('.value','',names(table))
  names(table)=sub('estimate_','B',names(table))
  return(table)
}

valence_df=readRDS('data/study1a/cleaned/valence_df.RDS')

pos_val_mod=valence_df %>% 
  #only look at ratings of the extent to which the stim shows positive outcomes of drinking
  filter(grepl('positive',valence)) %>% 
  lmer(valence_rating~val_cond+(1|ResponseId)+(1|QualtricsMsgID),data=.) %>%
  broom.mixed::tidy(.,conf.int=T) %>% 
  mutate(rating='positive',group=ifelse(group=='ResponseId','pID',
                                        ifelse(group=='QualtricsMsgID','mID',group)))
mc_table=valence_df %>% 
  #only look at ratings of the extent to which the stim shows negative outcomes of drinking
  filter(grepl('negative',valence)) %>% 
  lmer(valence_rating~val_cond+(1|ResponseId)+(1|QualtricsMsgID),data=.) %>% 
  broom.mixed::tidy(.,conf.int=T) %>%
  mutate(rating='negative',group=ifelse(group=='ResponseId','pID',
                                        ifelse(group=='QualtricsMsgID','mID',group))) %>% 
  rbind(pos_val_mod,.) %>%
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>% 
  select(rating,effect, group, term, estimate, `95% CI`, p.value) %>% 
  pivot_wider(.,names_from = rating, values_from=c('estimate','p.value',`95% CI`)) %>% 
  select(effect,group,term,names(.)[grepl('_positive',names(.))],names(.)[grepl('_negative',names(.))]) %>% 
  arrange(!is.na(group),!(!is.na(estimate_negative) & !is.na(estimate_positive))) %>% 
  mutate(term=case_when(is.na(group)~term,TRUE~paste(group,term,sep=' '))) %>%
  select(-group) 

mc_table[is.na(mc_table)]<-''
mc_table$term<-c('Intercept','Valence: pro-alcohol','pID Intercept','sID Intercept','Residual')

mc_table=optimizeColWidth(mc_table)

write_csv(mc_table, 'Tables/mc_table_study1a.csv')