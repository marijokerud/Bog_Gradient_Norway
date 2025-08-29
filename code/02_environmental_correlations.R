library(corrplot)
library(reshape2)
library(ggplot2)

######## CORRELATIONS 

cor_matrix <- cor(enviromental.PCA, use = "pairwise.complete.obs")
print(cor_matrix)



# Basic heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")



# Melt the correlation matrix into long format
cor_long <- melt(cor_matrix)

# Plot
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
