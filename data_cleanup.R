library(tidyverse)
load("doehm_survivor_data/castaway_details.rda")
load("doehm_survivor_data/confessionals.rda")
#load("doehm_survivor_data/screen_time.rda")
load("doehm_survivor_data/castaways.rda")
#load("doehm_survivor_data/boot_mapping.rda")
load("doehm_survivor_data/jury_votes.rda")
load("doehm_survivor_data/advantage_movement.rda")
load("doehm_survivor_data/challenge_results.rda")

# keep relevant details from each df 
# advantages 
clean_adv <- advantage_movement %>% 
  filter(event=="Found") %>% 
  group_by(version_season,castaway_id) %>% 
  summarise(N_Idols=n()) %>% 
  ungroup() 

# demog
demog_pre <- castaway_details %>% 
  select(castaway_id,gender) 

# game info 
castaways_pre <- castaways %>% 
  select(castaway_id, version, season, version_season, castaway,season,order,jury_status, result) %>% 
  mutate(winner = ifelse(result=="Sole Survivor",1,0)) 

# person/season level 
clean_castaways <- left_join(castaways_pre,demog_pre,by="castaway_id")

# confessionals - person/season level 
confessionals_clean <- confessionals %>%
  select(version_season,castaway_id,confessional_count) %>%
  group_by(version_season,castaway_id) %>%
  mutate(all_confess = sum(confessional_count)) %>%
  ungroup() %>%
  select(-confessional_count) %>%
  unique() %>% 
  select(castaway_id,version_season,all_confess) 

# merge on confessionals by 
cast_confess_demog <- left_join(clean_castaways, confessionals_clean, by=c("castaway_id","version_season"))

cast_confess_demog_adv <- left_join(cast_confess_demog,clean_adv,b=c("castaway_id", "version_season")) %>% 
  mutate(merge = ifelse(is.na(jury_status)==FALSE|order>15,1,0))

# challenges won by season/player 
clean_challenge_wins <- challenge_results %>% 
  group_by(castaway_id,version_season) %>% 
  mutate(wins = sum(ifelse(result=="Won" & outcome_type=="Tribal",1,0))) %>% 
  select(castaway_id,version_season,wins) %>% 
  ungroup() %>% 
  unique() 

merge_chall <- left_join(cast_confess_demog_adv, clean_challenge_wins, by=c("castaway_id","version_season")) %>% 
  filter(version_season!="AU08" & is.na(wins)==FALSE)

final_df <- merge_chall %>% 
  mutate(idols = ifelse(is.na(N_Idols)==FALSE,N_Idols,0), 
         confess_per_ep = round(all_confess/order,2)) %>% 
  select(-N_Idols) 


write.csv(final_df, "/Users/kq/Documents/dlab/survivor.csv", row.names=FALSE)
