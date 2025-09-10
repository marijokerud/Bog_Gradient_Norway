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
library(see)
library(qqplotr)
library(randomForest)

## cosmetic
theme_set(theme_bw()+
            theme(panel.spacing=grid::unit(0,"lines")))


data.glmer <- richness %>% 
  left_join(env_output, by = "plot_id") %>% 
  mutate(site = factor(site)) %>% 
  mutate(micro.topo = factor(micro.topo))
summary(data.glmer)   


################ GLM REGRESSION ################ 
# Poisson mixed model (random intercept for site)
# TOTAL SPECIES RICHNESS
mod_richness <- glmer(
  total_richness ~ PC1 * PC2 + (1 | site),
  data = data.glmer,
  family = poisson(link = "log")
  )
mod_richness2 <- glmer(
  total_richness ~ PC1 * PC2 + micro.topo + (1 | site),
  data = data.glmer,
  family = poisson(link = "log")
  )

AICtab(mod_richness, mod_richness2)
performance::compare_performance(mod_richness, mod_richness2, rank = TRUE)
best <- mod_richness2
summary(mod_richness2)

# Diagnostics for Poisson
res_richness <- DHARMa::simulateResiduals(best)
plot(res_richness)                            # residual plots
DHARMa::testDispersion(res_richness)          # overdispersion? (p < 0.05 indicates trouble), # should be non-significant for a good NB fit
DHARMa::testZeroInflation(res_richness)
performance::check_collinearity(best)     # PCs are orthogonal, but this confirms
performance::r2(best)                     # marginal & conditional R2
summary(best)                             # coefficients on the log scale
confint(best)

fx <- broom.mixed::tidy(best, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
fx <- fx %>%
  mutate(
    pct_change_per_SD = (estimate - 1) * 100,
    ci_low_pct        = (conf.low - 1) * 100,
    ci_high_pct       = (conf.high - 1) * 100
  )
fx

# SHANNON DIVERSITY
mod_shannon1 <- lmer(
  shannon_diversity ~ PC1 * PC2 + (1 | site),
  data = data.glmer)

mod_shannon2 <- lmer(
  shannon_diversity ~ PC1 * PC2 + micro.topo + (1 | site),
  data = data.glmer)

mod_shannon3 <- glmer(
  shannon_diversity ~ PC1 * PC2 + (1 | site),
  data = data.glmer,
  family = Gamma(link = "log"))

check_distribution_hist <- data.glmer %>% 
  ggplot(aes(x = shannon_diversity)) +
  geom_histogram()   

check_distribution_box <- data.glmer %>% 
  ggplot(aes(y = shannon_diversity)) +
  geom_boxplot()   

mod_shannon4 <- glmer(
  shannon_diversity ~ PC1 * PC2 + micro.topo + (1 | site),
  data = data.glmer, 
  family = Gamma(link = "log"))

AICtab(mod_shannon1, mod_shannon2, mod_shannon3, mod_shannon4)
res_shannon<-compare_performance(mod_shannon1, mod_shannon2, mod_shannon3, mod_shannon4, rank = TRUE)
plot(res_shannon)
best <- mod_shannon2
summary(best)


# EVENNESS
mod_evenness1 <- lmer(
  evenness ~ PC1 * PC2 + (1 | site),
  data = data.glmer)

mod_evenness2 <- lmer(
  evenness ~ PC1 * PC2 + micro.topo + (1 | site),
  data = data.glmer)

mod_evenness3 <- glmer(
  evenness ~ PC1 * PC2 + (1 | site),
  data = data.glmer,
  family = Gamma(link = "log"))

check_distribution_hist <- data.glmer %>% 
  ggplot(aes(x = evenness)) +
  geom_histogram()   

check_distribution_box <- data.glmer %>% 
  ggplot(aes(y = evenness)) +
  geom_boxplot()   

mod_evenness4 <- glmer(
  evenness ~ PC1 * PC2 + micro.topo + (1 | site),
  data = data.glmer, 
  family = Gamma(link = "log"))

AICtab(mod_evenness1, mod_evenness2, mod_evenness3, mod_evenness4)
res_evenness<-compare_performance(mod_evenness1, mod_evenness2, mod_evenness3, mod_evenness4, rank = TRUE)
plot(res_evenness)
best <- mod_evenness2
summary(best)

# MODEL CHECKING, https://easystats.github.io/see/articles/performance.html#checking-model-assumptions
#Binned Residuals
result <- binned_residuals(best) 
result #> Warning: Probably bad model fit. Only about 50% of the residuals are inside the error bounds.
plot(result)
#Check for Multicollinearity - Variance Inflation Factor
result <- check_collinearity(best)
plot(result)
#Check for Outliers
result <- check_outliers(best)
plot(result, type = "dots")
plot(result, type = "bars")
#Check for Normal Distributed Residuals
result <- simulate_residuals(best)
plot(result, type = "density")       #Points deviates in beginning and end
result <- check_residuals(best)
plot(result, type = "density")
plot(result, type = "qq")
#Check for Heteroscedasticity
result <- check_heteroscedasticity(best)
plot(result)
#Check for Homogeneity
result <- check_homogeneity(best)
plot(result)
#Posterior Predictive Checks
check_predictions(best)
#Overall Model Check
check_model(best)
#Compare Model Performances
result<-compare_performance(mod_shannon1, mod_shannon2, rank = TRUE)
result<-compare_performance(mod_shannon3, mod_shannon4, rank = TRUE)
result<-compare_performance(mod_shannon2, mod_shannon4, rank = TRUE)
result<-compare_performance(mod_shannon1, mod_shannon2, mod_shannon3, mod_shannon4, rank = TRUE)
plot(result)
#Model and Vector Properties
result <- check_distribution(best)
plot(result)

plot(res_evenness)
plot(res_shannon)




# Diagnostics for Poisson
res_shannon <- DHARMa::simulateResiduals(best)
plot(res_shannon)                            # residual plots
DHARMa::testDispersion(res_shannon)          # overdispersion? (p < 0.05 indicates trouble), # should be non-significant for a good NB fit
DHARMa::testZeroInflation(res_shannon)
performance::check_collinearity(best)     # PCs are orthogonal, but this confirms
performance::r2(best)                     # marginal & conditional R2
summary(best)                             # coefficients on the log scale
confint(best)

fx <- broom.mixed::tidy(best, effects = "fixed", conf.int = TRUE, exponentiate = TRUE)
fx <- fx %>%
  mutate(
    pct_change_per_SD = (estimate - 1) * 100,
    ci_low_pct        = (conf.low - 1) * 100,
    ci_high_pct       = (conf.high - 1) * 100
  )
fx



################ OLD CODE
fit_zipoisson <- glmmTMB(
  total_richness ~ PC1 * PC2 + (1 | site),
  data = data.glmer,
  ziformula=~1,
  family = poisson(link = "log"))

summary(fit_zipoisson)
fit_zinbinom <- update(fit_zipoisson,family=nbinom2)
fit_zinbinom1 <- update(fit_zipoisson,family=nbinom1)
AICtab(fit_zipoisson,fit_zinbinom,fit_zinbinom1) #Changes does not improve fit, keep original model 
AICtab(fit_zipoisson,mod_richness)  

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


