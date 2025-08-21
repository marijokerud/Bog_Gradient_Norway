library(tidyr)
library(dplyr)
library(lme4)
library(broom)
library(ggplot2)
library(readxl)

################  Grass:forb ratio  ################
data.ratio <- data %>% 
  gather(key = plott, value = abundance, - species) %>% #species, funtype left_join(sppfun)
  left_join(plant.type) %>% 
  filter(!is.na(abundance)) %>% 
  count(plott, funtype) %>% 
  complete(plott, nesting(funtype), fill = list(n = 0)) %>% 
  spread(key = funtype, value = n) %>%
  select(plott, gram, forb) %>% 
  mutate(site = gsub(".$", "", plott)) %>%
  mutate(site = gsub("G", "S", site)) %>% 
  left_join(Site) %>% 
  left_join(env_mean)

  
result.ratio <- data.ratio %>% 
  filter(Variable %in% c("Total.OXI", "Total.RED", "ratioN",  "Total.N", "Total.S", "Precipitation", "Tetraterm")) %>%    
  #filter(funtype %in% c("forb", "gram", "live", "moss", "shrub", "spha", "total", "grass_forb")) %>% 
  group_by(Variable) %>% #funtype
  mutate(Deposition = scale(Deposition, center = TRUE, scale = TRUE)) %>%
  do(fit = glmer(cbind(gram, forb)  ~ Deposition  + (1 | site), family = "binomial"(link = "logit"), data = .))  



names(result.ratio)
res.ratio<-tidy(result.ratio, fit)

#write.csv2(res.ratio, file = "result-funksjonelle-grupper_grass_forb_ratio.csv")

pred.ratio <- result.ratio %>% augment(fit) %>% rename("scaled_deposition" = "Deposition") #funker ikke!!

realN <- env_mean %>% 
  ungroup() %>% 
  select(site, Variable, Deposition)


pred.ratio <- pred.ratio %>% left_join(realN)

