# ---- Data Exploration --------------------------------------------------------

# ---- Load data and libraries from Setup.R file -------------------------------
source("Setup.R")


# ---- plots and visualizations -----------
hist(as.numeric(petdata$AdoptionSpeed))

#names_num <- names(which(sapply(petdata, is.numeric)))
#names_num
#length(names_num)

hist(petdata$Age)
summary(petdata)
inspect_types(petdata)%>% show_plot()
inspect_mem(petdata)%>% show_plot()
inspect_na(petdata)%>% show_plot()
inspect_imb(petdata)%>% show_plot()
inspect_cat(petdata[,!(names(petdata)%in%c("ColorID1","ColorID2","ColorID3","PetID"))])%>% show_plot()


petdata$ColorID1_num<-as.numeric(petdata$ColorID1)
# --- inspect categorical variables --------------------------------------------




# --- pairs and corr plot for numeric variables --------------------------------
par(mgp=c(0,0.3,0),mar=c(0,0,0,0)+0.1)

pairs.panels(petdata[c("Age", "Fee", "Quantity", "VideoAmt", "PhotoAmt",
                       "SentimentMagnitude", "AdoptionSpeed", "SentimentScore", "ColorID1_num")], 
             hist.col="darkblue",
             pch=23,
             main = "Pairs and Correlation Plot of Numeric Variables",
             cex.cor=0.6, 
             cex=0.5, 
             jiggle=TRUE,
             factor=1,
             ellipses = FALSE,
             smooth= FALSE,
             density = FALSE,
             rug=FALSE,
             stars=TRUE, 
             #bg=c("yellow","orange","red","purple","blue")[petdata$AdoptionSpeed],
             bg=c("red","green")[petdata$Type],
             #bg=c("green","red","yellow")[petdata$Gender],
             lwd=0.2,
             cex.axis = 0.9,
             tck=-0.05,
             las=1)


# --- pairs plot numeric variables -----------
par(mgp=c(0,0.3,0),mar=c(0,0,0,0)+0.1)
pairs(petdata[c("Age", "Fee", "Quantity", "VideoAmt", "PhotoAmt", "SentimentMagnitude")],
      col="orange",
      pch = 21, 
      main = "Pairs Plot of Numeric Variables", 
      label.pos = 0.5, 
      cex.labels=1,
      cex.axis=0.8,
      tck=-0.04,
      las = 1)
#pairs(petdata[names_num],col="orange", pch = 19, main = "Pairs Plot of Numeric Variables", label.pos = 0.5, cex.labels=0.8)

# corr_mat=cor(train[, names_num], method="s") #create Spearman correlation matrix
?cor()
corr_mat <- cor(petdata[, c("Age", "Fee", "Quantity", "VideoAmt", "PhotoAmt")], method = "s")

corrplot(corr_mat,
  method = "color",
  type = "upper", order = "hclust",
  tl.col = "black", tl.cex = 1
)

# ---- dog-breeds plot ------------------
par(mfrow=c(1,2))
par(mar = c(3, 5, 3, 3)+0.1, mgp=c(0,0.3,-0.2), oma = c(1, 5, 1, 1))

df_dogbreeds<-data.frame(table(dogs$Breed1))
max_table<-max(table(dogs$Breed1))

barplot(df_dogbreeds$Freq[df_dogbreeds$Freq>5],
        las = 1,
        cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.6, 
        xlim = c(0, 7000),
        cex.axis=0.7,
        names=df_dogbreeds$Var1[df_dogbreeds$Freq>5],
        col="darkred",
        #main="Frequencies (>5) of Dog Breeds in the Petdata Dataset"
)
# dogbreeds without mixed breed
barplot(df_dogbreeds$Freq[df_dogbreeds$Freq>5 & df_dogbreeds$Var1!="Mixed Breed"],
        las = 1,
        #cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.6, 
        xlim = c(0, 250),
        cex.axis=0.7,
        bg="lightgrey",
        names=df_dogbreeds$Var1[df_dogbreeds$Freq>5& df_dogbreeds$Var1!="Mixed Breed"],
        col="darkred",
        #cex.main=1,
        #main="Frequencies (>5) of Dog Breeds in the Petdata Dataset")
)
title(main = "Dog Breeds - Frequencies (>5) / and without 'Mixed Breed'", 
      line = -2.7, 
      cex.main=1.39,
      font.main=2,
      #col.main="darkorange",
      outer = TRUE,
      adj=0.28)
#axis(1, at = seq(0, 200, by = 25), labels = FALSE, tick = TRUE)
#axis(1, at = seq(0, 200, by = 25), labels = TRUE, las = 1, cex.axis= 0.7)

# ---- cat-breeds plot ------------------

par(mar = c(3, 5, 3, 3)+0.1, mgp=c(0,0.3,-0.2), oma = c(1, 5, 1, 1))

df_catbreeds<-data.frame(table(cats$Breed1))
max_breed_cats<-max(table(cats$Breed1))

barplot(df_catbreeds$Freq[df_catbreeds$Freq>5],
        las = 1,
        #cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.6, 
        xlim = c(0, 4000),
        cex.axis=0.7,
        names=df_catbreeds$Var1[df_catbreeds$Freq>5],
        col="darkred",
        #main="Frequencies (>5) of Cat Breeds in the Petdata Dataset")
)

barplot(df_catbreeds$Freq[df_catbreeds$Freq>5 & df_catbreeds$Var1!="Domestic Short Hair" & df_catbreeds$Var1!="Domestic Medium Hair"& df_catbreeds$Var1!="Domestic Long Hair"],
        las = 1,
        #cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.6, 
        xlim = c(0, 400),
        cex.axis=0.7,
        names=df_catbreeds$Var1[df_catbreeds$Freq>5& df_catbreeds$Var1!="Domestic Short Hair" & df_catbreeds$Var1!="Domestic Medium Hair"& df_catbreeds$Var1!="Domestic Long Hair"],
        col="darkred",
        #main="Frequencies (>5) of Cat Breeds in the Petdata Dataset")
)

title(main = "Cat Breeds - Frequencies (>3) per Breed / and without Domestic Breeds", 
      line = -3, 
      cex.main=1.32,
      font.main=2,
      #col.main="darkorange",
      outer = TRUE,
      adj=0.395)
#axis(1, at = seq(0, 200, by = 25), labels = FALSE, tick = TRUE)
#axis(1, at = seq(0, 200, by = 25), labels = TRUE, las = 1, cex.axis= 0.7)


# --------- fading sentiment ----------

# quick look into the sentiment data:
summary(petdata[names(petdata) == c("SentimentScore", "SentimentMagnitude")])
hist(petdata$SentimentScore, xlim = c(-1, 1), ylim = c(0, 2500))
boxplot(petdata$SentimentScore)

# Add a new column with row numbers
petdata$index <- seq_along(petdata$PetID)

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


# -- better fading option ---- 
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
  



