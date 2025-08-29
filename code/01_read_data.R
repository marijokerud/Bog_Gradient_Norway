library(readxl)
library(tidyverse)
library(labdsv)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

#GET DATA
comm.raw <-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Species", col_names = TRUE)
plant.type <-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Functional", col_names = TRUE)
enviromental.data <- read_excel(path = "Data/new_N_s.xlsx", col_names = TRUE)
climate.data <- read_excel(path = "Data/Env_2018.xlsx", sheet = "New", col_names = TRUE)
plot.info <-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Plot", col_names = TRUE)

comm.raw <- comm.raw %>% 
  select(-species_old)

plot.info <- plot.info %>% 
  select(quadrat, micro.topo)


################ ENVIRONMENAL DATA #####################

climate <- climate.data %>% 
  select(site, Precipitation, Tetraterm, lat, long) %>% 
  gather(key = Variable, value = Deposition, - site, - lat, - long)

enviromental <- enviromental.data %>% 
  select(-Grid, -Site) %>% 
  bind_rows(climate) %>% 
  pivot_wider(names_from = Variable, values_from = Deposition)

enviromental.PCA <- enviromental %>% 
  select(-site) %>% #, -lat, -long
  as.data.frame()


################  SPECIES RICHNESS  ################

data.glmer<- richness %>% 
  left_join(enviromental) 


species_richness <- comm.raw %>% 
  gather(key = quadrat, value = abundance, - species) %>% 
  filter(!is.na(abundance))  %>% 
  mutate(site = gsub(".$", "", quadrat)) %>% 
  group_by(site) %>%
  summarise(
    richness = n_distinct(species),        # number of unique species
    total_abundance = sum(abundance),      # optional: sum abundances
    .groups = "drop"
  )

print(species_richness)

################  PCA DATA  ################

pca.data <- enviromental %>% 
  left_join(species_richness) %>% 
  select(-site)

################  NMDS DATA  ################

species.mat <- comm.raw %>% 
  gather(key = quadrat, value = abundance, - species) %>% 
  filter(!is.na(abundance)) %>%                                        #remove NA's
  mutate(quadrat= substr(quadrat, 1, 4)) %>% 
  select(quadrat, species, abundance) %>% 
  as.data.frame

species.mat<- matrify(species.mat)
species.matrix <- species.mat 
