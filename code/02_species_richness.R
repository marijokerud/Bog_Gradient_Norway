library(dplyr)
library(tidyr)
library(tibble)
library(vegan)
library(openxlsx)


#SHANNON INDEX

richness.shannon <- diversity(comm.sp, index = "shannon")
richness.shannon <- as.data.frame(richness.shannon) %>% 
  rename(shannon_diversity = richness.shannon) %>% 
  rownames_to_column("plot_id")


#SPECIES RICHNESS & #EVENNESS
richness.func <- comm.long %>% 
  bind_rows(comm.long %>%                                         
              group_by(plot_id) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total_richness")) %>%                            #calculate total species richness
  rename(no_species = n)

richness <- richness.func %>% 
  pivot_wider(names_from = funtype, values_from = no_species) %>% 
  select(plot_id, total_richness) %>% 
  mutate(log_richness = log(total_richness)) %>% 
  left_join(richness.shannon, by = "plot_id") %>% 
  mutate(evenness = shannon_diversity/log_richness) %>% 
  mutate(site= substr(plot_id, 1, 3))


write.xlsx(richness.sp, "output/richness.xlsx")





