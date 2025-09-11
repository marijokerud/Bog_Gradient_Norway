library(dplyr)
library(tidyr)
library(tibble)
library(vegan)
library(openxlsx)

#SPECIES RICHNESS 
richness.func <- comm.long %>% 
  bind_rows(comm.long %>%                                         
              group_by(plot_id) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total_richness")) %>%                            #calculate total species richness
  bind_rows(comm.long %>% 
              filter(funtype %in% c("live", "spha", "moss")) %>% 
              group_by(plot_id) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total_moss"))   %>% 
  rename(no_species = n)

#SHANNON INDEX, on total_richness and moss_richness

richness.shannon <- diversity(comm.sp, index = "shannon")
richness.shannon <- as.data.frame(richness.shannon) %>% 
  rename(shannon_diversity = richness.shannon) %>% 
  rownames_to_column("plot_id")




#EVENNESS
richness <- richness.func %>% 
  mutate(log_richness = log1p(no_species)) %>% 
  left_join(richness.shannon, by = "plot_id") %>% 
  mutate(evenness = shannon_diversity/log_richness) 

#pivot_wider(names_from = funtype, values_from = no_species) %>% 
#select(plot_id, total_richness, shrub, forb, gram, live, spha, moss, total_moss) %>% 

#write.xlsx(richness, "output/richness.xlsx")





