library(tidyverse)
library(tidymodels)
library(broom)
library(ggplot2)
library(vegan)

## COMMUNITY ORDINATION (PCA)

comm_wide <- comm.raw %>% 
  gather(key = quadrat, value = cover, - species) %>% 
  filter(!is.na(cover)) %>%                                        #remove NA's
  mutate(cover = sqrt(cover)) %>% 
  pivot_wider(names_from = species, 
              values_from = cover, 
              values_fill = 0)
 
comm_sp <- comm_wide %>%
    select(-quadrat)
  
# meta data
comm_info <- comm_wide %>%
    select(quadrat) %>% 
    mutate(site= substr(quadrat, 1, 3)) %>%
    left_join(enviromental)
  
# make pca
res <- rda(comm_sp)
  
summary(res)
scores(res, display = "sites")
  
out <- bind_cols(comm_info, scores(res, display = "sites"))
  
sp <- scores(res, display = "species")

dist_matrix <- vegdist(comm_sp, method = "euclidean")

# adonis test
adonis <- adonis2(dist_matrix ~ Total.N , data =   comm_info, permutations = 999, method = "euclidean")
  
list(out, sp, res, adonis)
