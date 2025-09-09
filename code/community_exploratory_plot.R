################ EXPLORATORY PLOTS ################ 

################  TOTAL RICHNESS ################ 
plot1 <- data.glmer %>% 
  ggplot(aes(x = micro.topo, y = total_richness)) +
  geom_boxplot() 
plot1
plot1site <- data.glmer %>% 
  ggplot(aes(x = total_richness, y = PC1, color = site)) +
  geom_point(size = 2) 
plot1site
plot2site <- data.glmer %>% 
  ggplot(aes(x = total_richness, y = PC2, color = site)) +
  geom_point(size = 2) 
plot2site
plot1topo <- data.glmer %>% 
  ggplot(aes(x = total_richness, y = PC1, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot1topo
plot2topo <- data.glmer %>% 
  ggplot(aes(x = total_richness, y = PC2, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot2topo
data.glmer %>% 
  ggplot(aes(x = total_richness, colour = micro.topo)) +
  geom_density()


################  SHANNON DIVERSITY ################ 
plot1 <- data.glmer %>% 
  ggplot(aes(x = micro.topo, y = shannon_diversity)) +
  geom_boxplot() 
plot1
plot1site <- data.glmer %>% 
  ggplot(aes(x = shannon_diversity, y = PC1, color = site)) +
  geom_point(size = 2) 
plot1site
plot2site <- data.glmer %>% 
  ggplot(aes(x = shannon_diversity, y = PC2, color = site)) +
  geom_point(size = 2) 
plot2site
plot1topo <- data.glmer %>% 
  ggplot(aes(x = shannon_diversity, y = PC1, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot1topo
plot2topo <- data.glmer %>% 
  ggplot(aes(x = shannon_diversity, y = PC2, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot2topo
data.glmer %>% 
  ggplot(aes(x = shannon_diversity, colour = micro.topo)) +
  geom_density()

################ EVENNESS ################ 
plot1 <- data.glmer %>% 
  ggplot(aes(x = micro.topo, y = evenness)) +
  geom_boxplot() 
plot1
plot1site <- data.glmer %>% 
  ggplot(aes(x = evenness, y = PC1, color = site)) +
  geom_point(size = 2) 
plot1site
plot2site <- data.glmer %>% 
  ggplot(aes(x = evenness, y = PC2, color = site)) +
  geom_point(size = 2) 
plot2site
plot1topo <- data.glmer %>% 
  ggplot(aes(x = evenness, y = PC1, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot1topo
plot2topo <- data.glmer %>% 
  ggplot(aes(x = evenness, y = PC2, color = micro.topo)) +
  geom_point(size = 2) +
  geom_smooth(method = "glm")
plot2topo
data.glmer %>% 
  ggplot(aes(x = evenness, colour = micro.topo)) +
  geom_density()

