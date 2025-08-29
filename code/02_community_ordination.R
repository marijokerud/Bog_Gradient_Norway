library(tidyverse)
library(tidymodels)
library(broom)
library(glue)
library(ggplot2)
library(vegan)
library(ggrepel)

## COMMUNITY ORDINATION (PCA)

#DATA
comm.wide <- comm.raw %>% 
  gather(key = quadrat, value = cover, - species) %>% 
  filter(!is.na(cover)) %>%                                        #remove NA's
  mutate(cover = sqrt(cover)) %>% 
  pivot_wider(names_from = species, 
              values_from = cover, 
              values_fill = 0)
 
comm.sp <- comm.wide %>%
    select(-quadrat)
  
# meta data
comm.info <- comm.wide %>%
    select(quadrat) %>% 
    mutate(site= substr(quadrat, 1, 3)) %>%
    left_join(enviromental)
  
# MAKE PCA
res <- rda(comm.sp)
  
summary(res)
scores(res, display = "sites")
  
out <- bind_cols(comm.info, scores(res, display = "sites"))
output <- out %>% 
  select(quadrat, site, Total.N, PC1, PC2) %>% 
  left_join(plot.info, by = "quadrat")
  
sp <- scores(res, display = "species") |>
  as.data.frame() |>
  rownames_to_column(var = "species")


### DO NOT KNOW WHY 
dist_matrix <- vegdist(comm.sp, method = "euclidean")
# adonis test
adonis <- adonis2(dist_matrix ~ Total.N , data =   comm.info, permutations = 999, method = "euclidean")
list(output, sp, res, adonis)
 

####### MAKE SITE PCA FIGURE  
# calculte centroids
centroids <- output  %>% 
    left_join(output  %>% 
                group_by(site)  %>% 
                summarise(centroid1 = mean(PC1),
                          centroid2 = mean(PC2)),
              by = "site")

site.name <- centroids %>% 
  select(site, centroid1, centroid2, micro.topo) %>% 
  unique()
  
e_B <- eigenvals(res)/sum(eigenvals(res))

Site <- output %>%
  ggplot(aes(x = PC1, y = PC2, colour = site, shape = micro.topo)) +
  geom_point(size = 2) +
  #geom_text_repel(data = site.name, aes(x= centroid1, y = centroid2, label = site)) +   #Take out for cleaner plot
  scale_shape_manual(values = c(24, 22), name = "Microtopography") + 
  geom_point(data = centroids,
             aes(x = centroid1, y = centroid2, colour = site),
             shape = 16,
             size = 3) +
  geom_segment(data = centroids,
               aes(x = centroid1, y = centroid2,
               xend = PC1, yend = PC2, 
               colour = site),
               size = 0.6,
               alpha = 0.5,
               show.legend = FALSE) +
  
  stat_ellipse(
    data = output,
    aes(x = PC1, y = PC2, group = micro.topo, linetype = micro.topo),
    inherit.aes = FALSE,     # don't use colour = site from the main mapping
    level = 0.95,            # 95% ellipse
    type  = "t",             # or "norm"
    linewidth = 0.6,
    colour = "black",
    alpha = 0.7 ) +
  scale_linetype_manual(values = c("dashed", "solid"), name = "Microtopography") +

  coord_equal() +
  labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
       y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
       tag = "(a)") +
  guides(colour=guide_legend(title="Site")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.tag.position = c(0, 0.8),
        plot.tag = element_text(vjust = -1.5, hjust = -0.5, size = 10))

Site

####### MAKE SPECIES PCA FIGURE
# PC range for species
PC1_min <- min(sp$PC1) - 1
PC1_max <- max(sp$PC1) + 1
PC2_min <- min(sp$PC2) - 1
PC2_max <- max(sp$PC2) + 1

important_species <- sp %>%
  mutate(length = sqrt(PC1^2 + PC2^2)) %>%
  filter(length > 0.6)


Species <- sp %>%
  ggplot(aes(x = PC1, y = PC2)) +
    coord_equal(xlim = c(PC1_min, PC1_max),
                ylim = c(PC2_min, PC2_max)) +
    geom_segment(data = sp %>%
                 mutate(length = sqrt(PC1^2 + PC2^2)),
                 aes(x = 0, y = 0, xend = PC1, yend = PC2),
                 arrow = arrow(length = unit(0.2,"cm")),
                 alpha = 0.75,
                 color = 'grey70') +
  geom_text_repel(data = important_species, aes(label = species)) +
  labs(x = "PC 1", y = "PC 2", tag = "(b)") +
  theme_bw() +
  theme(aspect.ratio = 1,
          plot.tag.position = c(0, 0.8),
          plot.tag = element_text(vjust = -1.5, hjust = -0.5, size = 10))
