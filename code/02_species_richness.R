library(dplyr)
library(tidyr)
library(tibble)
library(vegan)
library(openxlsx)


#SHANNON INDEX

richness.shannon <- diversity(comm.sp, index = "shannon")
richness.shannon <- as.data.frame(richness.shannon) %>% 
  rename(shannon_diversity = richness.shannon) %>% 
  rownames_to_column("quadrat")


#SPECIES RICHNESS & #EVENNESS
richness.func <- comm.long %>% 
  bind_rows(comm.long %>%                                         
              group_by(quadrat) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total_richness")) %>%                            #calculate total species richness
  rename(no_species = n)

richness <- richness.func %>% 
  pivot_wider(names_from = funtype, values_from = no_species) %>% 
  select(quadrat, total_richness) %>% 
  mutate(log_richness = log(total_richness)) %>% 
  left_join(richness.shannon, by = "quadrat") %>% 
  mutate(evenness = shannon_diversity/log_richness)

write.xlsx(richness.sp, "output/richness.xlsx")





