library(lme4)
library(simr)

#set seed
set.seed(1111)

test_beta=0.2

#STUDY 2
study2_logs=read_csv('data/study2/cleaned/logs_cleaned.csv',col_types=cols())
study2_ppts=read_csv('data/study2/cleaned/S1_cleaned.csv') 

# Step 1: Fit model to your real data
model <- craving_model<-study2_logs %>% 
  left_join(study2_ppts) %>% 
  lmer(
    rating.keys_z~val_cond *type+val_cond*source_cond+source_cond*type +(1|pID)+(1|file),
    data = .,  # replace with your actual dataset
    control = lmerControl(optimizer = "bobyqa")
  )

# Step 2: Set interaction effect to medium size
fixef(model)["val_condpro-alcohol:source_condprofessional"] <- test_beta

# Step 3: Power simulation
powerSim(model,
         test = fixed("val_condpro-alcohol:source_condprofessional", method = "t"),
         nsim = 1000)
# Step 4: Check the power result

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

# Step 2: Set interaction effect to medium size
fixef(emo_model)["positive:val_condpro-alcohol"] <- test_beta

# Step 3: Power simulation
powerSim(emo_model,
         test = fixed("positive:val_condpro-alcohol", method = "t"),
         nsim = 1000)
# Step 4: Check the power result
