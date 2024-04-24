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


study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS')  %>% mutate(Study='Study 1a')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS') %>% mutate(Study='Study 1b')
study1a_emo_df=readRDS('data/study1a/cleaned/emo_df.RDS') %>% mutate(Study='Study 1a')
study1b_emo_df=readRDS('data/study1b/cleaned/emo_df.RDS') %>% mutate(Study='Study 1b') %>% mutate(emo_rating=as.numeric(emo_rating))

drink_1ab=bind_rows(study1a_drink_df,study1b_drink_df)
emo_1ab=bind_rows(study1a_emo_df,study1b_emo_df) %>% pivot_wider(names_from=specific_emotion,values_from=emo_rating)

emo_model=emo_1ab %>% left_join(drink_1ab) %>%
  filter(!grepl('non',val_cond)) %>%
  mutate(positive=scale(positive,scale=FALSE),
         negative=scale(negative,scale=FALSE)) %>% 
  lmer(drink_rating~positive*val_cond+negative*val_cond+positive*source_cond+negative*source_cond+(1|pIDs)+(1|filename)+(1|Study),data=.,control = lmerControl(optimizer='bobyqa')) 

emo_table = emo_model%>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  #add_row(.,effect='STUDY 1',.before=1)
  mutate(group=ifelse(group=='pIDs','pID',
                              ifelse(group=='filename','sID',group))) 


emo_table = emo_table %>%
  mutate(term=c('Intercept','Positive emotions (PosEmo)','Valence: pro-alcohol','Negative emotions (NegEmo)','Source: professional', 'PosEmo x Valence','NegEmo x Valence','PosEmo x Source','NegEmo x Source','pID Intercept','sID Intercept','Study intercept','Residual'),
         order=c(1,2,4,3,5,6,7,8,9,10,11,12,13)) %>% arrange(order) %>% select(-order)

emo_table=emo_table %>% mutate(p.value=case_when(is.na(p.value)~NA_character_,
                                                 p.value>=0.001~paste0('p = ',round(p.value,3)),
                                                 p.value<0.001~paste0('p < .001'),
                                                 TRUE~NA_character_),
                               Estimate=paste0(round(estimate,2),' [',round(conf.low,2),', ',round(conf.high,2),'], ',p.value),
                               Estimate=ifelse(grepl('NA',Estimate),estimate,Estimate))%>%
  dplyr::select(effect,term,Estimate) 


emo_table$Estimate[!grepl(',',emo_table$Estimate)]<-as.character(round(as.numeric(emo_table$Estimate[!grepl(',',emo_table$Estimate)]),2))


names(emo_table)[3]<-'Estimate (95% CI), p-value'

emo_table %>% mutate(effect=ifelse(effect=='ran_pars','random',effect)) %>%
  arrange(effect) %>%
  write_csv(.,'Tables/emoTable.csv')

mm_valence=emmeans(emo_model,specs=~val_cond)
pc_valence=contrast(mm_valence,"pairwise")

write_csv(data.frame(mm_valence),'Tables/emo_table_emmeans_valence.csv')
write_csv(data.frame(pc_valence),'Tables/emo_table_pairwiseComps_valence.csv')

