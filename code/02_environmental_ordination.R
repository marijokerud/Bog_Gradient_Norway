library(tidyverse)
library(vegan)


# MAKE PCA
res <- rda(enviromental.PCA, scale = TRUE, center = TRUE)

summary(res)
scores(res, display = "sites")

output <- bind_cols(enviromental, scores(res, display = "sites"))
env_output <- output %>% 
  select(site, PC1, PC2) %>% 
  left_join(plot.info, by = "site") 

  
sp.env <- scores(res, display = "species") |>
    as.data.frame() |>
    rownames_to_column(var = "variables")
  

####### MAKE SITE PCA FIGURE  
# calculate centroids
centroids <- output  %>% 
  left_join(output  %>% 
              group_by(site)  %>% 
              summarise(centroid1 = mean(PC1), 
                        centroid2 = mean(PC2)),
            by = "site")
  
e_B <- eigenvals(res)/sum(eigenvals(res))
  
Site <- output %>%
  ggplot(aes(x = PC1, y = PC2, colour = site)) +
  geom_point(size = 2) +
  coord_equal() +
  labs(x = glue("PCA1 ({round(e_B[1] * 100, 1)}%)"),
       y = glue("PCA2 ({round(e_B[2] * 100, 1)}%)"),
       tag = "(a)") +
  geom_text_repel(aes(label = site)) +   
  guides(colour=guide_legend(title="Site")) +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.tag.position = c(0, 0.8),
        plot.tag = element_text(vjust = -1.5, hjust = -0.5, size = 10))
Site 
 
#### ARROWS
important_env <- sp.env %>%
  mutate(length = sqrt(PC1^2 + PC2^2)) %>%
  filter(length > 0.6)

#  #geom_point(size = 2) +
  
arrow.env <- output %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_segment(data = sp.env %>%
                 mutate(length = sqrt(PC1^2 + PC2^2)),
               aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.2,"cm")),
               alpha = 0.75) +
  geom_text_repel(data = important_env, aes(label = variables)) +
  geom_text_repel(aes(label = site), color = 'grey70') +
  labs(x = "PC 1", y = "PC 2", tag = "(b)") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.tag.position = c(0, 0.8),
        plot.tag = element_text(vjust = -1.5, hjust = -0.5, size = 10))

arrow.env
