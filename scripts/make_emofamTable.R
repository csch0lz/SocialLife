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

emo_table = emo_table %>% mutate(effect=ifelse(effect=='ran_pars','random',effect),
                                 mod='Emotion') %>%
  arrange(effect) 

study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS') %>% mutate(Study='Study 1a')
study1a_familiarity_df=readRDS('data/study1a/cleaned/familiarity_df.RDS') %>% mutate(Study='Study 1a')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS')%>% mutate(Study='Study 1b')
study1b_familiarity_df=readRDS('data/study1b/cleaned/familiarity_df.RDS')%>% mutate(Study='Study 1b')

drink_1ab=study1a_drink_df %>% bind_rows(study1b_drink_df)
familiarity_1ab=study1a_familiarity_df %>% bind_rows(study1b_familiarity_df)

familiarity_1ab_wide = familiarity_1ab %>% 
  pivot_wider(values_from=fam_rating,names_from=familiarity_variable) %>% 
  mutate(fam_index=(`shows people that are like me`+`shows a situation I'm familiar with`)/2)

fam_table_full=drink_1ab %>% right_join(familiarity_1ab_wide) %>%
  filter(!grepl('non',val_cond)) %>%
  mutate(fam_index=scale(fam_index,scale=FALSE)) %>%
  lmer(drink_rating~fam_index*val_cond+fam_index*source_cond+(1|pIDs)+(1|filename)+(1|Study),data=.,control=lmerControl(optimizer='bobyqa'))  %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))


fam_table_full$term=c('Intercept','Familiarity Index (FI)','Valence: pro-alcohol','Source: professional', 'FI x Valence','FI x Source','pID Intercept','sID Intercept','Study Intercept','Residual')

fam_table_full=fam_table_full %>% mutate(p.value=case_when(is.na(p.value)~NA_character_,
                                                           p.value>=0.001~paste0('p = ',round(p.value,3)),
                                                           p.value<0.001~paste0('p < .001'),
                                                           TRUE~NA_character_),
                                         Estimate=paste0(round(estimate,2),' [',round(conf.low,2),', ',round(conf.high,2),'], ',p.value),
                                         Estimate=ifelse(grepl('NA',Estimate),estimate,Estimate)) %>%
  dplyr::select(effect,term,Estimate)

fam_table_full$Estimate[!grepl(',',fam_table_full$Estimate)]<-as.character(round(as.numeric(fam_table_full$Estimate[!grepl(',',fam_table_full$Estimate)]),2))

names(fam_table_full)[3]<-'Estimate (95% CI), p-value'

fam_table_full =fam_table_full %>% mutate(effect=ifelse(effect=='ran_pars','random',effect),
                                          mod='Familiarity') %>% arrange(effect) %>% rename(termFam=term) 

emo_fam_table=bind_rows(emo_table,fam_table_full) %>% pivot_wider(names_from=mod,values_from=c('Estimate (95% CI), p-value')) %>%
  mutate(termFam=lead(termFam,dim(emo_table)[1]),
         Familiarity=lead(Familiarity,dim(emo_table)[1]),
         index=row_number(),
         Familiarity=ifelse(index>2,lag(Familiarity,1),Familiarity),
         Familiarity=ifelse(index==3,NA,Familiarity),
         termFam=ifelse(index>2,lag(termFam,1),termFam),
         termFam=ifelse(index==3,NA,termFam),
         Familiarity=ifelse(index>7,lag(Familiarity,2),Familiarity),
         Familiarity=ifelse(index==8 | index==9,NA,Familiarity),
         termFam=ifelse(index>7,lag(termFam,2),termFam),
         termFam=ifelse(index==8 | index==9,NA,termFam)) %>%
  select(effect,term,Emotion,termFam,Familiarity) %>%
  slice(1:dim(emo_table)[1]) %>%
  rename(`Emotion Term`=term,`Familiarity Term`=termFam,`Emotion Effect`=Emotion,`Familiarity Effect`=Familiarity)  


write_csv(emo_fam_table,'Tables/emoFamTable.csv')


