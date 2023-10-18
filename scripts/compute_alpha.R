library('tidyverse')
library('psych')

study1a=readRDS('data/study1a/cleaned/familiarity_df.RDS')
study1b=readRDS('data/study1b/cleaned/familiarity_df.RDS')
study1ab=bind_rows(study1a,study1b)

fam_data=study1ab %>% filter(grepl('familiar',familiarity_variable) | grepl('like me',familiarity_variable)) %>% 
  mutate(familiarity_variable=ifelse(grepl('familiar',familiarity_variable),'familiar','like_me')) %>%
  pivot_wider(values_from=fam_rating,names_from=familiarity_variable) %>% 
  group_by(filename,condition) %>%
  summarize(familiar=mean(familiar,na.rm=T),like_me=mean(like_me,na.rm=T)) %>% ungroup()

fam_data %>% select(familiar,like_me) %>% alpha
                 