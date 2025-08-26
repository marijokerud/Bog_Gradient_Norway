library(tidyverse)
library(tidymodels)
library(broom)
library(ggplot2)
library(factoextra)   # for convenient PCA plots

# ---- 3) PCA (scale recommended: variables in different units) ----
pca <- prcomp(pca.data, center = TRUE, scale. = TRUE)

# Variance explained
print(summary(pca))          # proportions of variance per PC
print(pca$rotation)          # loadings: variable contributions

# ---- 4) Quick, informative plots ----
# Scree plot
fviz_eig(pca)

# Biplot: sites + variable vectors
fviz_pca_biplot(
  pca, repel = TRUE,
  col.var = "steelblue4",
  col.ind = "grey20"
)

# ---- 5) (Optional) Join scores back to coords for mapping or further analysis ----
scores <- as_tibble(pca$x, rownames = "site") %>%
  left_join(enviromental %>% select(site, lat, long), by = "site")

head(scores)



tidy_pca_components <- tidy(pca)
tidy_pca_individuals <- augment(pca, data = enviromental.PCA) # Use original data for augmenting
tidy_pca_summary <- glance(pca)
