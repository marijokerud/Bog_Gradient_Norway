library(readxl)
library(tidyverse)



#GET DATA
data<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Species", col_names = TRUE)
plant.type<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Functional", col_names = TRUE)

################  Number of species per plot  ################
data.glmer <- data %>% 
  select(-species_old) %>% 
  gather(key = quadrat, value = abundance, - species) %>% 
  left_join(plant.type) %>% 
  filter(!is.na(abundance)) 

data.nmds <- data %>% 
  select(-species_old)


############### Not used
data.glmer <- data.glmer %>% 
  complete(quadrat, nesting(funtype), fill = list(n = 0)) %>%        #fill inn funtype with 0 occurences
  bind_rows(data.glmer %>%                                         #calculate total species richness
              group_by(quadrat) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total")) %>% 
  mutate(site = gsub(".$", "", quadrat)) %>%
  mutate(site = gsub("G", "S", site))


data.glmer<- data.glmer %>% 
  left_join(Site) %>% 
  left_join(env_mean) 