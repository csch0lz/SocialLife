

# supplementary table source effects
pro_source=drink_1ab %>% right_join(familiarity_1ab_wide) %>%
  filter(grepl('pro_',condition),!grepl('non',condition)) %>%
  mutate(fam_index=scale(fam_index,scale=FALSE)) %>%
  lmer(drink_rating~fam_index*condition+(1|pIDs)+(1|filename)+(1|Study),data=.)  %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group))) %>% 
  mutate(Valence='Valence: pro-alcohol')

anti_source=drink_1ab %>% right_join(familiarity_1ab_wide) %>%
  filter(grepl('anti_',condition),!grepl('non',condition)) %>%
  mutate(fam_index=scale(fam_index,scale=FALSE)) %>%
  lmer(drink_rating~fam_index*condition+(1|pIDs)+(1|filename)+(1|Study),data=.)  %>% 
  broom.mixed::tidy(.,conf.int=TRUE) %>% 
  mutate(study=1,group=ifelse(group=='pIDs','pID',
                              ifelse(group=='QualtricsMsgID','sID',group)))%>% 
  mutate(Valence='Valence: anti-alcohol')

