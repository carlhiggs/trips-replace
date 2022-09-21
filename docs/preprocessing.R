library(tidyverse)

scenariosDF <- crossing(data.frame(max_walk=c(0,1,2)),
                        data.frame(max_cycle=c(0,2,5)),
                        data.frame(purpose=c("commuting", "all"))) %>%
  filter(max_walk!=max_cycle) %>%
  mutate(scen=paste0(purpose,"_",max_walk,"_",max_cycle)) %>%
  mutate(title1=paste0(ifelse(max_walk>0, max_walk, max_cycle), "km \u2265 ",
                       ifelse(max_walk>0, "walking", "cycling"), 
                       ifelse(max_walk>0 & max_cycle>0, paste(
                         " ;", max_walk, "km","<", "cycling \u2264", max_cycle, "km"), ""))) 

finalLocation <- "../output/melbourne-outputs"

output_df_agg_all <- readRDS(paste0(finalLocation,"/output_df_agg.rds")) %>% ## add titles
  left_join(scenariosDF)
output_diseases_change <- readRDS(paste0(finalLocation,"/output_diseases_change.rds")) %>% ## add titles
  left_join(scenariosDF)
output_life_years_change <- readRDS(paste0(finalLocation,"/output_life_years_change.rds")) %>% ## add titles
  left_join(scenariosDF)
PAall<-readRDS(paste0(finalLocation,"/PAall.rds")) %>% ## add titles
  left_join(scenariosDF)
PAallGuide<-readRDS(paste0(finalLocation,"/PAallGuide.rds")) %>% ## add titles
  left_join(scenariosDF)
output_transport_modes<-readRDS(paste0(finalLocation,"/output_transport_modes.rds")) %>% ## add titles
  left_join(scenariosDF)

output_transport_modes <- as_tibble(output_transport_modes %>%
                                      mutate(mode=factor(mode,
                                                         levels=c("walking","bicycle","public.transport","car","other"),
                                                         labels=c("Walking","Cycling","Public transport","Car","Other"))) %>%
                                      mutate(age=ifelse(age=="all", "all age groups", age),
                                             sex=ifelse(sex=="all", "females and males", sex), 
                                             sex=ifelse(sex=="male", "males", sex), 
                                             sex=ifelse(sex=="female", "females", sex)) %>%
                                      mutate(scenario=factor(scenario, levels=c("bl","sc"), labels=c("Base case", "Scenario"))))


write_csv(output_transport_modes ,'output_transport_modes.csv')