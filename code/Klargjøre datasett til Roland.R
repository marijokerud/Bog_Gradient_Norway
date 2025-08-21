#Klargjøre datasett til Roland
library(xlsx)

data.Roland <- data %>% 
  gather(key = plott, value = abundance, - species) %>% #species, funtype left_join(sppfun)
  left_join(plant.type) %>% 
  filter(!is.na(abundance)) %>% 
  count(plott, funtype) 


data.Roland <- data.Roland %>% 
  complete(plott, nesting(funtype), fill = list(n = 0)) %>%        #fill inn funtype with 0 occurences
  bind_rows(data.Roland %>%                                         #calculate total species richness
              group_by(plott) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total")) %>% 
  mutate(site = gsub(".$", "", plott)) %>%
  mutate(site = gsub("G", "S", site)) %>% 
  group_by(site, funtype) %>% 
  summarise(species.no = mean(n))


data.Roland<- data.Roland %>% 
  left_join(Site) %>% 
  left_join(env_mean) 

data.RolandPRINT<- data.Roland %>% 
  spread(Variable, Deposition) %>% 
  select(-Total.Ca, -Total.Cl, -Total.K, -Total.Mg, -Total.Na, -Total.S, -ratioN, -Grid)


write.xlsx(as.data.frame(data.RolandPRINT), file = "N-dataset.xlsx", sheetName = "Sheet1",
            col.names = TRUE, row.names = TRUE, append = FALSE)
