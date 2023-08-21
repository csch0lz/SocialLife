library('tidyverse')
library('reshape2')
library('ggpubr')
library('ggpattern')

study1a_emo_df=readRDS('data/study1a/cleaned/emo_df.RDS')
study1b_emo_df=readRDS('data/study1b/cleaned/emo_df.RDS')
study1a_drink_df=readRDS('data/study1a/cleaned/drink_df.RDS')
study1b_drink_df=readRDS('data/study1b/cleaned/drink_df.RDS')

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

emo_df_all = study1a_emo_df %>% bind_rows(study1b_emo_df)
emo_vars=unique(emo_df_all$specific_emotion)
drink_1ab=study1a_drink_df %>% bind_rows(study1b_drink_df)
drink_all_agg=drink_1ab %>% group_by(filename,val_cond) %>% summarise(drink_rating=mean(drink_rating,na.rm=T)) %>% ungroup()

###ANTI-ALCOHOL (PROFESSIONAL)
emo_anti_prof=emo_df_all %>%mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                                                 grepl('pro_',condition)~'pro-alcohol',
                                                 grepl('anti_',condition)~'anti-alcohol',
                                                 TRUE~NA_character_),
                              source_cond=case_when(grepl('prof',condition)~'professional',
                                                    grepl('social',condition)~'peer-produced',
                                                    TRUE~NA_character_)) %>%
  group_by(specific_emotion,val_cond,source_cond,filename) %>%
  summarise(emo_rating=mean(emo_rating,na.rm=T)) %>%
  ungroup %>%  
  pivot_wider(values_from=emo_rating,names_from=specific_emotion) %>% 
  left_join(drink_all_agg) %>% 
  filter(val_cond=='anti-alcohol',source_cond=='professional') %>%
  dplyr::select(all_of(emo_vars),drink_rating) %>% 
  rename(embarrassed="embarrassed (for someone else)",`alcohol craving`='drink_rating')

emo_pvals_anti_prof = emo_anti_prof %>% pivot_longer(cols=c(any_of(emo_vars),'embarrassed'),names_to='emotion') %>%
  group_by(emotion) %>% summarise(p=cor.test(value,`alcohol craving`)$p.value) %>%
  mutate(emotion=factor(emotion,levels=c('positive','happy','jealous','amused','negative','embarrassed','sad','shocked','scared'))) %>% 
  arrange(emotion) %>%
  mutate(psig=case_when(p<0.001~'p<.00025',TRUE~'not significant'))

emo_cormat_anti_prof=emo_anti_prof %>%
  cor %>%
  get_lower_tri %>%
  melt(.,na.rm=T) %>% 
  filter(Var1=="alcohol craving" | Var2=="alcohol craving") %>%
  mutate(Var1=factor(Var1,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving')),
         Var2=factor(Var2,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving'))
  ) %>%
  left_join(emo_pvals_anti_prof,by=c('Var2'='emotion')) %>%
  mutate(psig=ifelse(Var2=='alcohol craving','p<.00025',psig)) 

emo_plot_anti_prof=emo_cormat_anti_prof %>% 
  mutate(Var2=factor(Var2,levels = Var2[order(value)])) %>%
  ggplot(.,aes(x=Var1,y=Var2,fill=value)) +
  geom_tile_pattern(color='white',aes(pattern=psig)) +
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0,limit=c(-1,1),space='Lab',name='Pearson\nCorrelation')+
  scale_pattern_discrete(choices=c('circle','none')) + 
  coord_fixed() +
  labs(x='',y='') +
  ggtitle('Anti - Prof.')+
  theme(axis.text.x=element_text(angle=45,vjust=1, size=8, hjust=1),legend.position='none')

###ANTI-ALCOHOL (Peer-Produced)
emo_anti_peer=emo_df_all %>%mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                                                      grepl('pro_',condition)~'pro-alcohol',
                                                      grepl('anti_',condition)~'anti-alcohol',
                                                      TRUE~NA_character_),
                                   source_cond=case_when(grepl('prof',condition)~'professional',
                                                         grepl('social',condition)~'peer-produced',
                                                         TRUE~NA_character_)) %>%
  group_by(specific_emotion,val_cond,source_cond,filename) %>%
  summarise(emo_rating=mean(emo_rating,na.rm=T)) %>%
  ungroup %>%  
  pivot_wider(values_from=emo_rating,names_from=specific_emotion) %>% 
  left_join(drink_all_agg) %>% 
  filter(val_cond=='anti-alcohol',source_cond=='peer-produced') %>%
  dplyr::select(all_of(emo_vars),drink_rating) %>% 
  rename(embarrassed="embarrassed (for someone else)",`alcohol craving`='drink_rating')

emo_pvals_anti_peer = emo_anti_peer %>% pivot_longer(cols=c(any_of(emo_vars),'embarrassed'),names_to='emotion') %>%
  group_by(emotion) %>% summarise(p=cor.test(value,`alcohol craving`)$p.value) %>%
  mutate(emotion=factor(emotion,levels=c('positive','happy','jealous','amused','negative','embarrassed','sad','shocked','scared'))) %>% 
  arrange(emotion) %>%
  mutate(psig=case_when(p<0.001~'p<.00025',TRUE~'not significant'))

emo_cormat_anti_peer=emo_anti_peer %>%
  cor %>%
  get_lower_tri %>%
  melt(.,na.rm=T) %>% 
  filter(Var1=="alcohol craving" | Var2=="alcohol craving") %>%
  mutate(Var1=factor(Var1,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving')),
         Var2=factor(Var2,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving'))
  ) %>%
  left_join(emo_pvals_anti_peer,by=c('Var2'='emotion')) %>%
  mutate(psig=ifelse(Var2=='alcohol craving','p<.00025',psig)) 

emo_plot_anti_peer=emo_cormat_anti_peer %>% 
  mutate(Var2=factor(Var2,levels = Var2[order(value)])) %>%
  ggplot(.,aes(x=Var1,y=Var2,fill=value)) +
  geom_tile_pattern(color='white',aes(pattern=psig)) +
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0,limit=c(-1,1),space='Lab',name='Pearson\nCorrelation')+
  scale_pattern_discrete(choices=c('circle','none')) + 
  coord_fixed() +
  labs(x='',y='') +
  ggtitle('Anti - Peer')+
  theme(axis.text.x=element_text(angle=45,vjust=1, size=8, hjust=1),legend.position='none')

###PRO-ALCOHOL
emo_pro=emo_df_all %>%mutate(val_cond=case_when(grepl('non',condition)~'non-alcoholic',
                                                grepl('pro_',condition)~'pro-alcohol',
                                                grepl('anti_',condition)~'anti-alcohol',
                                                TRUE~NA_character_)) %>%
  group_by(specific_emotion,val_cond,filename) %>%
  summarise(emo_rating=mean(emo_rating,na.rm=T)) %>%
  ungroup %>%  
  pivot_wider(values_from=emo_rating,names_from=specific_emotion) %>% 
  left_join(drink_all_agg) %>% 
  filter(val_cond=='pro-alcohol') %>%
  dplyr::select(all_of(emo_vars),drink_rating) %>% 
  rename(embarrassed="embarrassed (for someone else)",`alcohol craving`='drink_rating')

emo_pvals_pro = emo_pro %>% pivot_longer(cols=c(any_of(emo_vars),'embarrassed'),names_to='emotion') %>%
  group_by(emotion) %>% summarise(p=cor.test(value,`alcohol craving`)$p.value) %>%
  mutate(emotion=factor(emotion,levels=c('positive','happy','jealous','amused','negative','embarrassed','sad','shocked','scared'))) %>%
  arrange(emotion) %>%
  mutate(psig=case_when(p<0.00025~'p<.00025',TRUE~'not significant'))

emo_cormat_pro=emo_pro %>% 
  cor %>%
  get_lower_tri %>%
  melt(.,na.rm=T) %>% 
  filter(Var1=="alcohol craving" | Var2=="alcohol craving") %>%
  mutate(Var1=factor(Var1,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving')),
         Var2=factor(Var2,levels=c('scared','shocked','sad','embarrassed','negative','amused','jealous','happy','positive','alcohol craving'))
  ) %>%
  left_join(emo_pvals_pro,by=c('Var2'='emotion')) %>%
  mutate(psig=ifelse(Var2=='alcohol craving','p<.00025',psig)) 

emo_plot_pro=emo_cormat_pro %>% 
  mutate(Var2=factor(Var2,levels = Var2[order(value)])) %>%
  ggplot(.,aes(x=Var1,y=Var2,fill=value)) +
  geom_tile_pattern(color='white',aes(pattern=psig)) +
  scale_fill_gradient2(low='blue',high='red',mid='white',midpoint=0,limit=c(-1,1),space='Lab',name='Pearson\nCorrelation')+
  scale_pattern_discrete(choices=c('circle','none'),guide=guide_legend(title='Significance Pattern',override.aes=list(fill='white',color='black'))) +  
  coord_fixed() +
  labs(x='',y='') +
  ggtitle('Pro')+
  theme(axis.text.x=element_text(angle=45,vjust=1, size=8, hjust=1),legend.position='right')

final=ggarrange(emo_plot_anti_prof,emo_plot_anti_peer,emo_plot_pro,nrow=1)

ggsave('Figures/EmoCorMat_Figure.png', plot = final,width=1200,height=589,units='px')
