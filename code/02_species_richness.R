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

comm.sp.all
comm.sp.moss

total_richness <- diversity(comm.sp.all, index = "shannon")
total_moss <- diversity(comm.sp.moss, index = "shannon")

richness.shannon <- as.data.frame(total_richness) %>% 
  rownames_to_column("plot_id") %>% 
  left_join(as.data.frame(total_moss) %>% 
              rownames_to_column("plot_id")) %>% 
  pivot_longer(
    cols = starts_with("tot"),
    names_to = "funtype", 
    values_to = "shannon_diversity")


#EVENNESS on total_richness and moss_richness
richness <- richness.func %>% 
  filter(funtype %in% c("total_richness", "total_moss")) %>% 
  mutate(log_richness = log1p(no_species)) %>% 
  left_join(richness.shannon, by = c("plot_id", "funtype")) %>% 
  mutate(evenness = shannon_diversity/log_richness) 

#pivot_wider(names_from = funtype, values_from = no_species) %>% 
#select(plot_id, total_richness, shrub, forb, gram, live, spha, moss, total_moss) %>% 

#write.xlsx(richness, "output/richness.xlsx")





