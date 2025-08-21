citation()
citation("tidyverse")
citation("lme4")
citation("ggplot2")
citation("sp")

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lme4")

library(tidyverse)
library(lubridate)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)


#ARTSFREKVENS DATA
Site<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "Site", col_names = TRUE)
Env1978_1982<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "1978_1982", col_names = TRUE)
Env1983_1987<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "1983_1987", col_names = TRUE)
Env1988_1992<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "1988_1992", col_names = TRUE)
Env1992_1996<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "1992_1996", col_names = TRUE)
Env1997_2001<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "1997_2001", col_names = TRUE)
Env2002_2006<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "2002_2006", col_names = TRUE)
Env2007_2011<-read_excel(path = "Data/Trend_50x50km_Master.xlsx", sheet = "2007_2011", col_names = TRUE)
env_new<-read_excel(path = "Data/Env_2018.xlsx", sheet = "New", col_names = TRUE)

Site <- Site %>% 
  select(site, Site, Grid)

env_clim <- env_new %>% 
  select(site, Precipitation, Tetraterm)


Env1978_1982 <- Env1978_1982 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area, -Precipitation) %>% #, -Total.K, -Total.Ca, -Total.Na, -Total.Mg, -Total.Cl
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(1978))

Env1983_1987 <- Env1983_1987 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(1983))

Env1988_1992 <- Env1988_1992 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Precipitation) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(1988))

Env1992_1996 <- Env1992_1996 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area, -Precipitation) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(1992))

Env1997_2001 <- Env1997_2001 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area, -Precipitation) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(1997))

Env2002_2006 <- Env2002_2006 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area, -Precipitation) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(2002)) 

Env2007_2011 <- Env2007_2011 %>%
  mutate(Grid = gsub("^", "G", Grid.no)) %>% 
  select(-Grid.no, -Grid.area, -Precipitation) %>% 
  #gather(Variable, Measurement, -Grid) %>% 
  filter(Grid %in% c("G15", "G9", "G23", "G14", "G21", "G12", "G39", "G13", "G19", "G25", "G34", "G33", "G44", "G44", "G53", "G53", "G64", "G73", "G82", "G82", "G90")) %>% 
  mutate(year = as.character(2007))

env<- bind_rows(Env1983_1987, Env1988_1992, Env1992_1996, Env1997_2001, Env2002_2006, Env2007_2011)

env<- env %>%
  gather(Variable, Measurement, -Grid, -year) %>% 
  filter(!is.na(Measurement)) %>% 
  left_join(Site)
#filter(Variable == "Total.S") %>% 
#summarise(maks = max(Measurement))

env_cor <- env %>%
  group_by(Grid, Variable) %>% 
  summarise(Deposition = mean(Measurement)) %>% 
  left_join(Site) %>% 
  spread(key = Variable, value = Deposition) %>% 
  mutate(ratioN = Total.OXI/Total.RED+Total.N) %>% 
  left_join(env_clim)

env_mean <- env_cor %>%
  gather(Variable, Deposition, -Grid, -site, -Site)


write.csv2(env_mean, file = "new_N_s.csv") #add the mean deposition values to excel-file Env_2018

#Check correlations
env1<- env_cor %>% 
  ungroup() %>% 
  select(-Grid, -site, -Site)

cor <- cor(env1)
plot(env1)

write.csv2(cor, file = "cor.csv") 

library(ggpubr)

ggpaired(data = env_cor, cond1 = "Total.OXI", cond2 = "Total.RED", id = "site",  fill = "condition")

