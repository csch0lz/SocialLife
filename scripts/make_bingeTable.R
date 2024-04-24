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

binge_table=study1b_drink_df %>% 
  left_join(study1b_ppts) %>%
  mutate(val_cond=factor(val_cond,levels=c('non-alcoholic','anti-alcohol','pro-alcohol'))) %>% 
  mutate(binge_drinking_rev=scale(binge_drinking_rev,scale=FALSE)) %>%
  lmer(drink_rating~binge_drinking_rev*val_cond + (1|pIDs) + (1|filename),data=.)  %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(group=ifelse(group=='pIDs','pID',
                      ifelse(group=='filename','sID',group))) %>% 
  mutate(`95% CI`=case_when(!is.na(conf.low)~paste0('[',format(round(conf.low,2),nsmall=2),';',format(round(conf.high,2),nsmall=2),']'),TRUE~NA_character_),
         p.value=formatp(p.value)) %>%
  select(effect, group, term, estimate, `95% CI`, p.value) %>%  
  mutate(term=case_when(is.na(group)~term,TRUE~paste(group,term,sep=' '))) %>%
  mutate_at(names(.)[grepl('estimate',names(.))],round_format) %>% 
  mutate_at(names(.)[grepl('estimate',names(.))],sub,pattern='NA',replacement=' ') %>% 
  select(-group) %>%
  mutate_all(as.character)  %>%
  mutate(effect=ifelse(effect=='ran_pars','random',effect))


binge_table[is.na(binge_table)]<-''
binge_table$term=c('Intercept','Binge Drinking Frequency (BD)','Valence 1: Anti- vs. Non-Alcoholic', 'Valence 2: Pro- vs. Non-Alcoholic', 'BD x Valence 1','BD x Valence 2','sID','pID','Residual')


binge_table=binge_table %>% mutate(p.value=ifelse(p.value!='',paste0('p = ',p.value),p.value)) |> rename('p-value'='p.value')

write_csv(binge_table,'Tables/bingeTable.csv')
