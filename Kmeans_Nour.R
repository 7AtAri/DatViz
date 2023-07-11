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

cats_numeric <-cats[c('Age','PhotoAmt','AdoptionSpeed')]
  
 cats_numeric %>% 
  gather(Attributes, value, 1:5) %>% 
  ggplot(aes(x=value)) +
  geom_histogram(fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Value", y = "Frequency")
corrplot(cor(cats), type = "upper", method = "ellipse", tl.cex = 0.9)
corrplot(cor(cats_numeric), type = "upper", method = "ellipse", tl.cex = 0.9)
cats_scaled <- scale(cats_numeric)
cats_scaled

res.pca <- PCA(cats_scaled,  graph = FALSE)
# Visualize eigenvalues/variances
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

res.pca
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

#7, 9, 20, 22, 27, 28, 29, 30, 31, 32
colnames(petdata)

library(dplyr)

desc_length_summary<-petdata %>%
  group_by(AdoptionSpeed,FurLength, Type) %>%
  summarize(count=n())  

colnames(petdata)

aggregate(Type ~ AdoptionSpeed + Desc_length, data = petdata, function(x) length(x))

ggplot(aes(x =AdoptionSpeed , y = FurLength), data = desc_length_summary) +
  geom_point(aes(color=count))

library(rgl)
plot3d( 
  x= desc_length_summary$AdoptionSpeed, y= desc_length_summary$FurLength,
  z= desc_length_summary$Count, 
  #col =desc_length_summary$Type
)

colnames(petdata)
petdata_cluster=filter(petdata,Quantity==1)
#petdata_cluster=filter(petdata_cluster,Health!='Healthy')
petdata_cluster=petdata_cluster[c('StateID','BreedID2','BreedID1',
                                  'Type','State','AdoptionSpeed')]
library(cluster)
gower.dist_pets <- daisy(petdata_cluster[c('StateID','BreedID2','BreedID1','Type')]
                         , metric = c("gower"))
pam_fit <- pam(gower.dist_pets, diss = TRUE, k =5 )
pam_results <- petdata_cluster %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary
library(plotly)
library('Rtsne')
tsne_obj <- Rtsne(gower.dist_pets, is_distance = TRUE)
gower.dist_pets
tsne_data_ <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering), state=petdata_cluster$State,
         adoptionSpeed=petdata_cluster$AdoptionSpeed,type=petdata_cluster$Type)

p<-ggplot(aes(x = X, y = Y, type=type,adoptionSpeed=adoptionSpeed, state=state ), data = tsne_data_) +
  geom_point(aes(color = cluster))
p<-p+scale_colour_discrete(labels = c("1-Cats-multiple states","2-Dogs-multiple states","3-Dogs-multiple states","4-Cats-Kuala Lumpur","5-Dogs-Kuala Lumpur")) 
 
ggplotly(p+scale_colour_discrete(labels = c("1-Cats-multiple states","2-Dogs-multiple states","3-Dogs-multiple states","4-Cats-Kuala Lumpur","5-Dogs-Kuala Lumpur")) 
)

p<-p+theme(axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         axis.title.y=element_blank(),
         axis.text.y=element_blank(),
         axis.ticks.y=element_blank())


p

ggplotly(ggplot(aes(x = X, y = Y, type=type,adoptionSpeed=adoptionSpeed, state=state ), data = tsne_data_
                , scale_fill_discrete(labels=c("1-Cats-multiple states","2-Dogs-multiple states","3-Dogs-multiple states","4-Cats-Kuala Lumpur","5-Dogs-Kuala Lumpur"))) +
           geom_point(aes(color = cluster)
          ))

tsne_data
pam_fit$clusinfo
pam_results
petdata_cluster %>%
  mutate(cluster = pam_fit$clustering) 
pam_fit$clustering


#cats clusters

cats_cluster=filter(cats,Quantity==1)
cats_cluster=cats_cluster[c('StateID','BreedID2','BreedID1','Gender',
                            'MaturitySize','FurLength','Vaccinated','Dewormed',
                            'Sterilized','Health')]
gower.dist.cats <- daisy(cats_cluster, metric = c("gower"))
pam_fit_cats <- pam(gower.dist.cats, diss = TRUE, k =6 )
pam_results_cats <- cats_cluster %>%
  mutate(cluster = pam_fit_cats$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

tsne_obj_cats <- Rtsne(gower.dist.cats, is_distance = TRUE)

tsne_data_cats <- tsne_obj_cats$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_cats$clustering),AdoptionSpeed=cats_cluster$AdoptionSpeed
         ,Vaccinated=cats_cluster$Vaccinated,Dewormed=cats_cluster$Dewormed
         ,Sterilized=cats_cluster$Sterilized)


p_cats<-ggplot(aes(x = X, y = Y, Vaccinated=Vaccinated,
           Dewormed=Dewormed, Sterilized=Sterilized,), data = tsne_data_cats) +
  geom_point(aes(color = cluster))
ggplotly(p_cats)


#dogs clusters

dogs_cluster=filter(dogs,Quantity==1)
dogs_cluster=dogs_cluster[c('StateID','BreedID2','BreedID1','Gender',
                            'MaturitySize','FurLength','Vaccinated','Dewormed',
                            'Sterilized','Health')]
gower.dist.dogs <- daisy(dogs_cluster, metric = c("gower"))
pam_fit_dogs <- pam(gower.dist.dogs, diss = TRUE, k =6 )
pam_results_dogs <- dogs_cluster %>%
  mutate(cluster = pam_fit_dogs$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

tsne_obj_dogs <- Rtsne(gower.dist.dogs, is_distance = TRUE)

tsne_data_dogs <- tsne_obj_dogs$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit_dogs$clustering),AdoptionSpeed=dogs_cluster$AdoptionSpeed
         ,Vaccinated=dogs_cluster$Vaccinated,Dewormed=dogs_cluster$Dewormed
         ,Sterilized=dogs_cluster$Sterilized)


p_dogs<-ggplot(aes(x = X, y = Y, Vaccinated=Vaccinated,
                   Dewormed=Dewormed, Sterilized=Sterilized ), data = tsne_data_dogs) +
  geom_point(aes(color = cluster))

ggplotly(p_dogs+theme_classic())



# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  78
  pam_fit <- pam(gower.dist.cats,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}
# Plot sihouette width (higher is better)
plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

petdata$Desc_length<-sapply(strsplit(petdata$Description," "),length)
petdata$Desc_length

plot()

