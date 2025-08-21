library(tidyverse)
library(lme4)
library(broom)
library(ggplot2)


ind.dat <- read.csv("R:/Prosjekter/22551000_eikevolltjonnmyra_statens_vegvesen/Eikevolltjønnmyra data/ellen.csv",sep=";", header=T)
summary(ind.dat)

#ALL ELLENBERG VALUES
data.ellenberg <- data %>% 
  gather(key = plott, value = abundance, - species) %>%
  filter(abundance >= 1) %>% 
  mutate(species = str_replace(species, "cf.", "")) %>% 
  left_join(ind.dat)

resultEllenberg <- data.ellenberg %>% 
  select(-"T", -"K", -"TRU") %>% 
  gather(key = ellenberg, value = value, -species, -plott, -abundance, -ord) %>% 
  group_by(plott, ellenberg) %>% 
  summarise(CWM = weighted.mean(value, abundance, na.rm = TRUE)) %>% 
  mutate(site = gsub(".$", "", plott)) %>%
  mutate(site = gsub("G", "S", site)) %>% 
  
  
  #left_join(Site) %>% 
  left_join(env_mean)


#OLD SCRIPTS

#Ellenberg R
ind.datR <- ind.dat %>% 
  select(species, R)

data.ellenbergR2 <- data %>% 
  gather(key = plott, value = abundance, - species) %>%
  filter(abundance >= 1) %>% 
  mutate(species = str_replace(species, "cf.", "")) %>% 
  left_join(ind.datR) %>% 
  filter(R >= 1)

resultEllenbergR <- data.ellenbergR2 %>% 
  group_by(plott) %>% 
  summarise(CWM = weighted.mean(R, abundance, na.rm = TRUE)) %>% 
  mutate(ellenberg = "R")




#Ellenberg N  
ind.datN <- ind.dat %>% 
  select(species, N)


data.ellenbergN <- data %>% 
  gather(key = plott, value = abundance, - species) %>%
  filter(abundance >= 1) %>% 
  mutate(species = str_replace(species, "cf.", "")) %>% 
  left_join(ind.datN) %>% 
  filter(N >= 1)

resultEllenbergN <- data.ellenberg %>% 
  group_by(plott) %>% 
  summarise(CWM = weighted.mean(N, abundance, na.rm = TRUE)) %>% 
  mutate(ellenberg = "N")
  
resultEllenberg <- resultEllenbergN %>% 
  bind_rows(resultEllenbergR) %>% 
  mutate(site = gsub(".$", "", plott)) %>%
  mutate(site = gsub("G", "S", site)) %>% 
  left_join(Site) %>% 
  left_join(env_mean)

hist(resultEllenberg$CWM)


#Mixed model
resultE <- resultEllenberg %>% 
  filter(Variable %in% c("Total.N", "Total.S", "Precipitation", "Tetraterm")) %>%       
  filter(ellenberg %in% c("N", "R")) %>% 
  group_by(Variable, ellenberg) %>% 
  mutate(Deposition = scale(Deposition, center = TRUE, scale = TRUE)) %>%
  do(fit = glmer(CWM  ~ Deposition  + (1 | site), family = "poisson"(link = "log"), data = .))  


names(resultE)
res.Ellenberg<-tidy(resultE, fit)

#write.csv2(res.FUN, file = "result-funksjonelle-grupper_2.csv")

pred.Ellenberg <- resultE %>% augment(fit) %>% rename("scaled_deposition" = "Deposition")

realN <- env_mean %>% 
  ungroup() %>% 
  select(site, Variable, Deposition)


pred.Ellenberg <- pred.FUN %>% left_join(realN)

labelsE <- c(N = "Ellenberg N", R = "Ellenberg R") 

ggplot(pred.Ellenberg, aes(x= Deposition, y= .fixed )) + 
  facet_grid(ellenberg~Variable, scales="free", labeller=labeller(ellenberg = labelsE)) +
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE, color = "black") +
  ylab("Predicted values")
