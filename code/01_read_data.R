library(readxl)
library(tidyverse)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#GET DATA
data<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Species", col_names = TRUE)
plant.type<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Functional", col_names = TRUE)
enviromental.data <- read_excel(path = "Data/new_N_s.xlsx", col_names = TRUE)
climate.data <- read_excel(path = "Data/Env_2018.xlsx", sheet = "New", col_names = TRUE)

data <- data %>% 
  select(-species_old)


################ ENVIRONMENAL DATA #####################

climate <- climate.data %>% 
  select(site, Precipitation, Tetraterm, lat, long) %>% 
  gather(key = Variable, value = Deposition, - site, - lat, - long)

enviromental <- enviromental.data %>% 
  select(-Grid, -Site) %>% 
  bind_rows(climate)


################  SPECIES RICHNESS  ################

richness <- data %>% 
  gather(key = quadrat, value = abundance, - species) %>% 
  filter(!is.na(abundance)) %>%                                        #remove NA's
  mutate(quadrat= substr(quadrat, 1, 4)) %>%                           #extract the first four letters in quadrat
  left_join(plant.type) %>%                                            # add functional trait to data
  count(quadrat, funtype) %>%                                          #count no species in functional groups
  complete(quadrat, nesting(funtype), fill = list(n = 0))              #fill inn functional groups (funtype) with 0 occurences


richness <- richness %>% 
  bind_rows(richness %>%                                         
              group_by(quadrat) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total")) %>%                            #calculate total species richness
  rename(no_species = n) %>% 
  mutate(site = gsub(".$", "", quadrat)) %>%                           #add site
  mutate(site = gsub("G", "S", site))


data.glmer<- richness %>% 
  left_join(enviromental) 


################  ORDINATION DATA  ################

data.nmds <- data %>% 
  select(-species_old)
