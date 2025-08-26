library(vegan)

community_NMDS <- metaMDS(species.matrix, # Our community-by-species matrix
                 k=3, maxit = 500, trymax = 100, distance = "jaccard") # The number of reduced dimensions, "jaccard"

print(community_NMDS)

#explore the effect of individual points
gof <- goodness(object = community_NMDS)
plot(community_NMDS, display = "sites", type = "none")
points(community_NMDS, display = "sites", cex = 2*gof/mean(gof)) #. Symbol size is proportional to the amount of variance associated with each sample unit

#Evaluating Fit
stressplot(community_NMDS)

plot(community_NMDS)

ordiplot(community_NMDS,type="n")
orditorp(community_NMDS,display="species",col="red",air=0.5)
orditorp(community_NMDS,display="sites",cex=0.8,air=0.8)




############## NOT USED YET
#Calculate scores 
Site.scores <- as.data.frame(scores(community_NMDS)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
Species.scores <- as.data.frame(scores(community_NMDS)$species)


Site.scores$site <- rownames(Site.scores)
site.scores <- Site.scores %>% 
  mutate(felt_id = str_sub(site, start = 1, end = 2)) %>% 
  mutate(year = str_sub(site, start = 4, end = 7)) %>% 
  mutate(year2 = str_sub(year, start = 3, end = 4)) %>% 
  mutate(year3 = year2) %>% 
  mutate(year3 = gsub("23", "28", year3)) %>% 
  mutate(year3 = gsub("18", "23", year3)) %>%
  mutate(year3 = gsub("13", "18", year3)) %>% 
  mutate(year3 = gsub("08", "13", year3)) %>% 
  mutate(year3 = gsub("03", "8", year3)) %>% 
  mutate(year3 = gsub("98", "3", year3)) %>% 
  mutate(year3 = gsub("95", "0", year3)) %>% 
  mutate(year3 = as.numeric(year3))


Species.scores$species <- rownames(Species.scores)
species.scores <- Species.scores %>% 
  mutate(species_short = species) %>% 
  mutate(art1 = str_sub(species_short, start = 1, end = 4)) %>%
  mutate(art2 = species_short) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', species_short)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species_short = paste(art1, art2)) %>% 
  left_join(norwegian.name)


#
Site.scores$site <- rownames(Site.scores)
site.scores <- Site.scores %>% 
  mutate(felt_id = str_sub(site, start = 1, end = 2)) %>% 
  mutate(year = str_sub(site, start = 7, end = 8)) %>% 
  mutate(year2 = year) %>% 
  mutate(year3 = str_sub(site, start = 5, end = 8)) %>% 
  mutate(year = gsub("08", "0", year)) %>% #Gi verdi 0
  mutate(year = gsub("13", "1", year)) %>% #Gi verdi 1
  mutate(year = gsub("18", "2", year)) %>% #Gi verdi 2
  mutate(year = gsub("23", "3", year)) %>% #Gi verdi 2
  mutate(year = as.numeric(year)) %>% 
  mutate(year3 = as.numeric(year3))


#mutate(year = recode_factor(year,
"0" = "0",
"1" = "1",
"2" = "2",
"3" = "3"))
