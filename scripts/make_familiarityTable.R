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


study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS') %>% mutate(Study='Study 1a')
study1a_familiarity_df=readRDS('data/study1a/cleaned/familiarity_df.RDS') %>% mutate(Study='Study 1a')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS')%>% mutate(Study='Study 1b')
study1b_familiarity_df=readRDS('data/study1b/cleaned/familiarity_df.RDS')%>% mutate(Study='Study 1b')

drink_1ab=study1a_drink_df %>% bind_rows(study1b_drink_df)
familiarity_1ab=study1a_familiarity_df %>% bind_rows(study1b_familiarity_df)

familiarity_1ab_wide = familiarity_1ab %>% 
  pivot_wider(values_from=fam_rating,names_from=familiarity_variable) %>% 
  mutate(fam_index=(`shows people that are like me`+`shows a situation I'm familiar with`)/2)

fam_table=drink_1ab %>% right_join(familiarity_1ab_wide) %>%
  lmer(drink_rating~fam_index*val_cond*source_cond+(1|pIDs)+(1|filename)+(1|Study),data=.)  %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))


fam_table$term=c('Intercept','Familiarity Index (FI)','Valence (1): pro-alcohol','Valence (2): non-alcoholic','FI x Valence (1)','FI x Valence (2)','pID Intercept','sID Intercept','study intercept','Residual')

fam_table=fam_table %>% mutate(p.value=case_when(is.na(p.value)~NA_character_,
                                                 p.value>=0.001~paste0('p = ',round(p.value,3)),
                                                 p.value<0.001~paste0('p < .001'),
                                                 TRUE~NA_character_),
                               Estimate=paste0(round(estimate,2),' [',round(conf.low,2),', ',round(conf.high,2),'], ',p.value),
                               Estimate=ifelse(grepl('NA',Estimate),estimate,Estimate))

fam_table$Estimate[!grepl(',',fam_table$Estimate)]<-as.character(round(as.numeric(fam_table$Estimate[!grepl(',',fam_table$Estimate)]),2))

fam_table %>% dplyr::select(effect,term,Estimate) %>% mutate(effect=ifelse(effect=='ran_pars','random',effect)) %>%
write_csv(.,'Tables/familiarityTable.csv')
