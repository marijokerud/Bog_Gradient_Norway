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


plot.info <- plot.info %>% 
  mutate(site= substr(plot_id, 1, 3)) %>% 
  select(site, plot_id, micro.topo) %>% 
  left_join(enviromental %>% 
              select(site, lat, long))

################  COMMUNITY DATA  ################

comm.wide.all <- comm.raw %>% 
  gather(key = plot_id, value = cover, - species) %>% 
  filter(!is.na(cover)) %>%                                        #remove NA's
  mutate(cover = sqrt(cover)) %>% 
  pivot_wider(names_from = species, 
              values_from = cover, 
              values_fill = 0)

comm.sp.all <- comm.wide %>%
  column_to_rownames("plot_id")

comm.wide.moss <- comm.raw %>% 
  gather(key = plot_id, value = cover, - species) %>% 
  filter(!is.na(cover)) %>%                                        #remove NA's
  mutate(cover = sqrt(cover)) %>% 
  left_join(plant.type) %>% 
  filter(funtype %in% c("live", "spha", "moss")) %>% 
  select(-funtype) %>% 
  pivot_wider(names_from = species, 
              values_from = cover, 
              values_fill = 0)

comm.sp.moss <- comm.wide.moss %>%
  column_to_rownames("plot_id")


# meta data
comm.info <- comm.wide %>%
  select(plot_id) %>% 
  mutate(site= substr(plot_id, 1, 3)) %>%
  left_join(enviromental)


comm.long <- comm.raw %>% 
  gather(key = plot_id, value = abundance, - species) %>% 
  filter(!is.na(abundance)) %>%                                        #remove NA's
  left_join(plant.type) %>%                                            # add functional trait to data
  count(plot_id, funtype) %>%                                          #count no species in functional groups
  complete(plot_id, nesting(funtype), fill = list(n = 0))              #fill inn functional groups (funtype) with 0 occurences

#filter(funtype %in% c("total","shrub","forb", "gram", "live", "moss", "spha")) #remove ferns and tree (Pinus juv.)


################  SPECIES RICHNESS  ################


species_richness <- comm.raw %>% 
  gather(key = plot_id, value = abundance, - species) %>% 
  filter(!is.na(abundance))  %>% 
  mutate(site = gsub(".$", "", plot_id)) %>% 
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
  gather(key = plot_id, value = abundance, - species) %>% 
  filter(!is.na(abundance)) %>%                                        #remove NA's
  mutate(plot_id= substr(plot_id, 1, 4)) %>% 
  select(plot_id, species, abundance) %>% 
  as.data.frame

species.mat<- matrify(species.mat)
species.matrix <- species.mat 
