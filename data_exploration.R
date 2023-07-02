# ---- Data Exploration --------------------------------------------------------

# ---- Load data and libraries from Setup.R file -------------------------------
source("Setup.R")


# ---- plots and visualizations -----------
hist(petdata$AdoptionSpeed)

names_num <- names(which(sapply(petdata, is.numeric)))
names_num
length(names_num)

hist(petdata$Age)
summary(petdata)

# pairs(train[, names_num[1:10]])
# pairs(train[, names_num[11:20]])
pairs(petdata[c("Age", "BreedID1", "Fee", "Quantity", "VideoAmt", "PhotoAmt")])

pairs(petdata[c("Age", "BreedID1", "Fee", "Quantity", "VideoAmt", "PhotoAmt", "Health", "Vaccinated")])

# corr_mat=cor(train[, names_num], method="s") #create Spearman correlation matrix

corr_mat <- cor(petdata[, c("Age", "BreedID1", "Fee", "Quantity", "VideoAmt", "PhotoAmt", "Health", "Vaccinated", "AdoptionSpeed")], method = "s")

corrplot(corr_mat,
  method = "color",
  type = "upper", order = "hclust",
  tl.col = "black", tl.cex = 1
)

# Plot the breeds
par(mar = c(5, 4, 4, 2) + 10)
barplot(table(petdata$Breed1), xlab = "Breed1", las = 2, cex.lab = 0.5, horiz = TRUE)

# quick look into the sentiment data:
summary(petdata[names(petdata) == c("SentimentScore", "SentimentMagnitude")])
hist(petdata$SentimentScore, xlim = c(-1, 1), ylim = c(0, 2500))
boxplot(petdata$SentimentScore)

# --------- fading sentiment ----------
# Add a new column with row numbers
petdata$index <- seq_along(petdata$PetID)

library(ggplot2)
library(RColorBrewer)
library(grid) 
library(ggplotify)
?rasterGrob
g <- rasterGrob(brewer.pal(11, "RdBu"), width=unit(5,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 
gradient<-grid.raster(brewer.pal(11, "RdBu"), width=unit(5,"npc"), height = unit(1,"npc"), 
            interpolate = TRUE)
grid.draw(gradient) 

ggplot(petdata, aes(factor(SentimentScore))) + 
  # add gradient background 
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) + 
  geom_bar() # add data layer 

df<-data.frame(petdata$SentimentScore,petdata$y)
# Create a plot using ggplot2

# Add a dummy y variable
petdata$y <- 1

ggplot(petdata) +
  geom_tile(aes(x = SentimentScore, y = y, fill = SentimentScore), width = 0.3, height = 2) +
  scale_fill_gradient(low = "darkblue", high = "orange") +
  theme_void()

ggplot(petdata) +
  geom_tile(aes(x = SentimentScore, y = y, fill = SentimentScore), width = 0.1, height = Inf) +
  scale_fill_gradientn(colors = c("darkblue", "white", "orange"), 
                       values = c(-0.1, 0.5, 1.1),
                       guide = "none") + 
  geom_histogram(aes(x = SentimentScore),binwidth = 0.1, bins = 20)


# -- alternative fading option ---- 
# reference:
# https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient

make_gradient <- function(deg = 45, n = 100, cols = blues9) {
  cols <- colorRampPalette(cols)(n + 1)
  rad <- deg / (180 / pi)
  mat <- matrix(
    data = rep(seq(0, 1, length.out = n) * cos(rad), n),
    byrow = TRUE,
    ncol = n
  ) +
    matrix(
      data = rep(seq(0, 1, length.out = n) * sin(rad), n),
      byrow = FALSE,
      ncol = n
    )
  mat <- mat - min(mat)
  mat <- mat / max(mat)
  mat <- 1 + mat * n
  mat <- matrix(data = cols[round(mat)], ncol = n)
  grid::rasterGrob(
    image = mat,
    width = unit(1, "npc"),
    height = unit(1, "npc"), 
    interpolate = TRUE
  )
}

g <- make_gradient(
  deg = -180, n = 500, cols = brewer.pal(9, "RdBu")
)

ggplot(data=subset(petdata, !is.na(SentimentScore)), aes(SentimentScore)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  geom_bar()

ggplot(data=subset(dogs, !is.na(SentimentScore)), aes(SentimentScore)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  geom_bar()

ggplot(data=subset(cats, !is.na(SentimentScore)), aes(SentimentScore)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  geom_bar()

# --- sentiment + other variables -----
ggplot() +
  geom_jitter(data=petdata, aes(x=SentimentScore, y=Age, size=PhotoAmt, color=AdoptionSpeed, shape=factor(Health)))+
  theme_minimal()
  


