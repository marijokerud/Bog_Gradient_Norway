library(tidyverse)
library(tidyr)
library(dplyr)
library(lme4)
library(broom)
library(broom.mixed)
library(ggplot2)
library(readxl)
library(xlsx)
library(glmmTMB)
library(bbmle) 
library(DHARMa)
library(performance)

## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))

data.glmer <- richness %>% 
  left_join(env_output, by = "plot_id") %>% 
  mutate(site = factor(site))
  mutate(scale_PC1 = as.numeric(scale(PC1)), 
         scale_PC2 = as.numeric(scale(PC2)))

    

#res.reg<- glmer(no_species  ~ Deposition  + (1 | site), family = "poisson"(link = "log"), data = .))  

# Poisson mixed model (random intercept for site)
m_pois <- glmmTMB(
  total_richness ~ PC1 * PC2 + (1 | site),
  data = data.glmer,
  family = poisson(link = "log")
)

# Diagnostics for Poisson
res_pois <- DHARMa::simulateResiduals(m_pois)
plot(res_pois)                     # residual plots
DHARMa::testDispersion(res_pois) # overdispersion? (p < 0.05 indicates trouble)
DHARMa::testZeroInflation(res_pois)




################ OLD CODE
################  Number of species per plot  ################
data.glmer <- data %>% 
  gather(key = plot_id, value = abundance, - species) %>% #species, funtype left_join(sppfun)
  left_join(plant.type) %>% 
  filter(!is.na(abundance)) %>% 
  count(plot_id, funtype) 


data.glmer <- data.glmer %>% 
  complete(plot_id, nesting(funtype), fill = list(n = 0)) %>%        #fill inn funtype with 0 occurences
  bind_rows(data.glmer %>%                                         #calculate total species richness
              group_by(plot_id) %>% 
              summarise(n = sum(n)) %>% 
              mutate(funtype = "total")) %>% 
  mutate(site = gsub(".$", "", plot_id)) %>%
  mutate(site = gsub("G", "S", site))


data.glmer<- data.glmer %>% 
  left_join(Site) %>% 
  left_join(env_mean) 
  
result <- data.glmer %>% 
  #filter(Variable %in% c("Total.OXI", "Total.RED", "ratioN",  "Total.N", "Total.S", "Precipitation", "Tetraterm")) %>%        
  filter(funtype %in% c("total")) %>% 
  group_by(Variable, funtype) %>% 
  mutate(Deposition = scale(Deposition, center = TRUE, scale = TRUE)) %>%
  do(fit = glmer(no_species  ~ Deposition  + (1 | site), family = poisson(link = "log"), data = .))  

#result <- data.glmer %>% 
  filter(Variable %in% c("Total.OXI", "Total.RED", "ratioN",  "Total.N", "Total.S", "Precipitation", "Tetraterm")) %>%        #
  filter(funtype %in% c("forb", "gram", "live", "moss", "shrub", "spha", "total")) %>% #, "grass_forb"
  group_by(Variable, funtype) %>% 
  mutate(Deposition = scale(Deposition, center = TRUE, scale = TRUE)) %>%
  do(fit = glmer(no_species  ~ Deposition  + (1 | site), family = "poisson"(link = "log"), data = .))  


out <- result %>%
  ungroup() %>%
  rowwise() %>%
  mutate(tidy = list(broom.mixed::tidy(fit))) %>%  # list() keeps a list-col
  ungroup() %>%
  unnest(tidy)

print(out)

out_flat <- out %>%
  select(-fit, everything()) %>%           # drop obvious list/S4 cols if present
  select(where(~ !is.list(.)))                    # ensure only atomic cols remain

# (Optional) If you only want fixed effects rows:
# out_flat <- out_flat %>% filter(effect == "fixed")

# 2) Export to CSV
write.csv(out_flat, "output/glmer_results.csv", row.names = FALSE)
write.xlsx(out_flat, "output/glmer_results.xlsx", row.names = FALSE, sep=".")


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


