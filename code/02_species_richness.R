library(dplyr)
library(tidyr)
library(tibble)
library(vegan)

#SHANNON INDEX

richness.shannon <- diversity(comm.sp, index = "shannon")
richness.shannon <- as.data.frame(richness.shannon) %>% 
  rename(shannon_diversity = richness.shannon) %>% 
  rownames_to_column("site")


#SPECIES RICHNESS & #EVENNESS

comm.long <- comm.raw %>% 
  gather(key = quadrat, value = abundance, - species) %>% 
  filter(!is.na(abundance)) %>%                                        #remove NA's
  left_join(plant.type) %>%                                            # add functional trait to data
  count(quadrat, funtype) %>%                                          #count no species in functional groups
  complete(quadrat, nesting(funtype), fill = list(n = 0))              #fill inn functional groups (funtype) with 0 occurences


richness.func <- comm.long %>% 
  bind_rows(comm.long %>%                                         
              group_by(quadrat) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total_richness")) %>%                            #calculate total species richness
  rename(no_species = n)

richness.sp <- richness.func %>% 
  pivot_wider(names_from = funtype, values_from = no_species) %>% 
  select(quadrat, total_richness) %>% 
  column_to_rownames("quadrat") %>% 
  mutate(log_richness = log(total_richness)) %>% 
  rownames_to_column("site") %>% 
  left_join(richness.shannon, by = "site") %>% 
  mutate(evenness = shannon_diversity/log_richness)





