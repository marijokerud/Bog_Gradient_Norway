library(tidyverse)
library(tidyr)
library(dplyr)
library(lme4)
library(broom)
library(ggplot2)
library(readxl)
library(xlsx)



#ARTSFREKVENS DATA
data<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Species", col_names = TRUE)
plant.type<-read_excel(path = "Data/species-richness-2018.xlsx", sheet = "Functional", col_names = TRUE)



################  Number of species per plot  ################
data.glmer <- data %>% 
  gather(key = quadrat, value = abundance, - species) %>% #species, funtype left_join(sppfun)
  left_join(plant.type) %>% 
  filter(!is.na(abundance)) %>% 
  count(quadrat, funtype) 


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
  


result <- data.glmer %>% 
  filter(Variable %in% c("Total.OXI", "Total.RED", "ratioN",  "Total.N", "Total.S", "Precipitation", "Tetraterm")) %>%        #
  filter(funtype %in% c("forb", "gram", "live", "moss", "shrub", "spha", "total")) %>% #, "grass_forb"
  group_by(Variable, funtype) %>% 
  mutate(Deposition = scale(Deposition, center = TRUE, scale = TRUE)) %>%
  do(fit = glmer(n  ~ Deposition  + (1 | site), family = "poisson"(link = "log"), data = .))  


names(result)
res.FUN<-tidy(result, fit)

#res.FUN2 <- res.FUN %>% 
  spread(key = "funtype", value = "estimate")

#write.csv2(res.FUN2, file = "result-funksjonelle-grupper2.csv")

pred.FUN <- result %>% augment(fit) %>% rename("scaled_deposition" = "Deposition")

site<- Site %>% 
  select(-Site)

realN <- env_mean %>% 
  ungroup() %>% 
  select(site, Variable, Deposition)


pred.FUNplot <- pred.FUN %>% left_join(realN)

#PLOTS
#Set colours, linetype and lables
#deep purple, medium purple, brown,     black,   light green, medium green, dark green, orange
"#762a83",    "#af8dc3",     "#8c510a", "black", "#d9f0d3",   "#7fbf7b",    "#1b7837", "#f1a340"
               forbs,      gram,      live,     moss,        shrub,    spha,      total
cbPalette <- c("#762a83", "#af8dc3", "#7fbf7b", "#1b7837",  "#8c510a", "#f1a340", "black")
linetypeN <- c("solid",   "dotted", "solid",   "solid",    "dotted", "dotted", "solid")
linetypeS <- c("solid",   "dotted", "solid",   "solid",    "dotted", "dotted", "solid")
linetypeT <- c("dotted", "dotted", "dotted", "dashed",  "dotted", "dotted", "dotted")
labelsPLOT <- c(fern = "Ferns", forb = "Forbs", gram = "Graminoids", live = "Liverworths", moss = "Mosses", shrub = "Shrubs", spha = "Sphagnum", total = "Total", tree = "Tree")


#TOTAL NITROGEN
pred.N <- pred.FUNplot %>% 
  filter(Variable == "Total.N")

Nplot <-
ggplot(pred.N, aes(x= Deposition, y= .fixed, group = funtype, colour = funtype, linetype = funtype)) + #what about .mu?
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  scale_colour_manual(values = cbPalette, 
                      name = "Functional types",
                      labels = labelsPLOT) +
  scale_linetype_manual(values = linetypeN, 
                        name = "Functional types",
                        labels = labelsPLOT) +
  xlab("Total N deposition") +
  ylab("Predicted values") +
  theme_bw() +
  theme(panel.grid.minor=element_blank())
  
  

#TOTAL SULPHUR
pred.S <- pred.FUNplot %>% 
  filter(Variable == "Total.S")

Splot <-
ggplot(pred.S, aes(x= Deposition, y= .fixed, group = funtype, colour = funtype, linetype = funtype)) + 
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  scale_colour_manual(values = cbPalette, 
                      name = "Functional types",
                      labels = labelsPLOT) +
  scale_linetype_manual(values = linetypeS, 
                        name = "Functional types",
                        labels = labelsPLOT) +
  xlab("Total S deposition") +
  ylab("Predicted values") +
  theme_bw() +
  theme(panel.grid.minor=element_blank())

#TEMPERATURE
pred.T <- pred.FUNplot %>% 
  filter(Variable == "Tetraterm")

Tplot <-
ggplot(pred.T, aes(x= Deposition, y= .fixed, group = funtype, colour = funtype, linetype = funtype)) + 
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE) +
  scale_colour_manual(values = cbPalette, 
                      name = "Functional types",
                      labels = labelsPLOT) +
  scale_linetype_manual(values = linetypeT, 
                        name = "Functional types",
                        labels = labelsPLOT) +
  xlab("Temperature") +
  ylab("Predicted values") +
  theme_bw() +
  theme(panel.grid.minor=element_blank())

#PRECIPITATION
pred.P <- pred.FUNplot %>% 
  filter(Variable == "Precipitation")

Pplot <-
ggplot(pred.P, aes(x= Deposition, y= .fixed, group = funtype, colour = funtype)) + 
  geom_smooth(formula = y ~ x, method = "lm", se = TRUE, linetype = "dotted") +
  scale_colour_manual(values = cbPalette, 
                      name = "Functional types",
                      labels = labelsPLOT) +
  xlab("Precipitation") +
  ylab("Predicted values") +
  theme_bw() +
  theme(panel.grid.minor=element_blank())


library(cowplot)

Figure1<- 
  plot_grid(Nplot, Splot, Tplot, Pplot, labels = c("A", "B", "C", "D"), ncol = 2)


pdf("Figure.pdf", width = 11.7, height = 8.3, useDingbats=FALSE)
plot_grid(Figure1)
dev.off()


