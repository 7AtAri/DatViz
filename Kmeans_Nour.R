library(tidyverse)
library(magrittr)
library(cluster)
library(cluster.datasets)
library(cowplot)
library(NbClust)
library(clValid)
library(ggfortify)
library(clustree)
library(dendextend)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(GGally)
library(ggiraphExtra)
library(knitr)
library(kableExtra)

source('Setup.R')

glimpse(cats)
summary(cats) #%>% kable()# %>% kable_styling()

cats_numeric <-cats[c('Age','Quantity','Fee','PhotoAmt','AdoptionSpeed')]
  
 cats_numeric %>% 
  gather(Attributes, value, 1:5) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency")

corrplot(cor(cats_numeric), type = "upper", method = "ellipse", tl.cex = 0.9)
cats_scaled <- scale(cats_numeric)
cats_scaled

res.pca <- PCA(cats_scaled,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))


# Extract the results for variables
var <- get_pca_var(res.pca)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
 # Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


km2 <- kmeans(na.omit(cats_scaled), centers = 3, nstart=30)
p1 <- fviz_cluster(km2, data = na.omit(cats_scaled), ellipse.type = "convex") + theme_minimal() + ggtitle("k = 2") 
plot_grid(p1, labels = c("k2"))
#, p2, p3, p4, p5, p6,
#, "k3", "k4", "k5", "k6", "k7"
?kmeans
colSums(is.na(cats_numeric))
cats_numeric[c('BreedID2','BreedID1')]
head(cats_scaled)


#dogs

dogs_numeric <-dogs[c('Age','Quantity','Fee','PhotoAmt','AdoptionSpeed')]

dogs_numeric %>% 
  gather(Attributes, value, 1:5) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency")

corrplot(cor(dogs_numeric), type = "upper", method = "ellipse", tl.cex = 0.9)
dogs_scaled <- scale(dogs_numeric)
dogs_scaled

res.pca <- PCA(dogs_scaled,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))


# Extract the results for variables
var <- get_pca_var(res.pca)

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# Control variable colors using their contributions to the principle axis
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal() + ggtitle("Variables - PCA")


km2 <- kmeans(na.omit(dogs_scaled), centers = 3, nstart=30)
p1 <- fviz_cluster(km2, data = na.omit(dogs_scaled), ellipse.type = "convex") + theme_minimal() + ggtitle("k = 2") 
plot_grid(p1, labels = c("k2"))
#, p2, p3, p4, p5, p6,
#, "k3", "k4", "k5", "k6", "k7"
?kmeans
colSums(is.na(cats_numeric))
cats_numeric[c('BreedID2','BreedID1')]
head(cats_scaled)
