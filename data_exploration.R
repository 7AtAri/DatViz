# ---- Data Exploration --------------------------------------------------------

# ---- Load data and libraries from Setup.R file -------------------------------
source("Setup.R")

# --- summaries -----------------------------------------------------------
summary(petdata)
skim(petdata) # nicer summary


#inspect_types(petdata)%>% show_plot()
#inspect_na(petdata)%>% show_plot()


# --- inspect categorical variables --------------------------------------------

labels=c("Adoption Speed", "Breed 1", "Breed 2", "Color 1",
         "Color 2", "Color 3", "Dewormed", "Fur Length", "Gender",
         "Health", "Maturity Size", "Sentiment Score", "State", 
         "Sterilized", "Type", "Vaccinated")
  
inspect_cat(petdata[,!(names(petdata)%in%c("ColorID1","ColorID2","ColorID3","PetID", "Name", "Description", "RescuerID", "StateID"))])%>% 
  show_plot(label_size=0.1, col_palette=2,label_color="black") +
  labs(title = "Frequency in categorical levels in Petdata",
       subtitle = "Grey segments are NAs") +
  theme(plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "white"),
    #panel.border=element_rect(margin(0.9,0,0,0)),
    strip.background = element_blank(),
    axis.text.y.left=element_text(size = 11, hjust=1),
    plot.margin = margin(21,17,17,17))
inspect_cat(cats[,!(names(cats)%in%c("ColorID1","ColorID2","ColorID3","PetID", "Name", "Description", "RescuerID", "StateID"))])%>% 
  show_plot(col_palette=5, label_color="black")+
  labs(title = "Frequency in categorical levels in cats data",
       subtitle = "Grey segments are NAs") +
  theme(plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        #panel.border=element_rect(margin(0.9,0,0,0)),
        strip.background = element_blank(), 
        axis.text.y.left=element_text(size = 11, hjust=1),
        plot.margin = margin(21,17,17,17))
inspect_cat(dogs[,!(names(dogs)%in%c("ColorID1","ColorID2","ColorID3","PetID", "Name", "Description", "RescuerID", "StateID"))])%>% 
  show_plot(col_palette=5,label_color="black")+
  labs(title = "Frequency in categorical levels in dogs data",
       subtitle = "Grey segments are NAs") +
  theme(plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "white"),
        #panel.border=element_rect(margin(0.9,0,0,0)),
        strip.background = element_blank(),
        axis.text.y.left=element_text(size = 11, hjust=1),
        plot.margin = margin(21,17,17,17))


inspect_imb(petdata)%>% show_plot(col_palette=2)

# --- add a numeric color column for pairs plot ----------------------------
petdata$ColorID1_num<-as.numeric(petdata$ColorID1)

# --- pairs and corr plot for numeric variables --------------------------------

# save following plot in png format in current directory
png(file="pairs_and_corr.png", width=1800, height=1200,res=200)

par(mgp=c(0,0.3,0),mar=c(0,0,0,0)+0.1)
pairs.panels(petdata[c("Age", "Fee", "Quantity", "VideoAmt", "PhotoAmt",
                       "SentimentMagnitude", "AdoptionSpeed", "SentimentScore", "ColorID1_num")], 
             hist.col="darkblue",
             pch=23,
             main = "Pairs and Correlation Plot of Numeric Variables",
             cex.cor=0.5, 
             cex=0.45, 
             cex.labels=0.7,
             jiggle=TRUE,
             factor=1,
             ellipses = FALSE,
             smooth= FALSE,
             density = FALSE,
             rug=FALSE,
             stars=TRUE, 
             #bg=c("yellow","orange","red","purple","blue")[petdata$AdoptionSpeed],
             bg=c("purple","orange")[petdata$Type],
             #bg=c("green","red","yellow")[petdata$Gender],
             lwd=0.2,
             cex.axis = 0.6,
             tck=-0.08,
             las=1,
             oma=c(3,3,8,3))

# save the file:
dev.off()
getOption("device")
dev.cur()

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

# ------ corr plot --------------------------------------------------------
# corr_mat=cor(train[, names_num], method="s") #create Spearman correlation matrix

corr_mat <- cor(petdata[, c("Age", "Fee", "Quantity", "VideoAmt", "PhotoAmt")], method = "s")

corrplot(corr_mat,
  method = "color",
  type = "upper", order = "hclust",
  tl.col = "black", tl.cex = 1
)

# ---- dog-breeds plot ---------------------------------------------------
par(mfrow=c(1,2))
par(mar = c(3, 5, 3, 3)+0.1, mgp=c(0,0.3,-0.2), oma = c(1, 7, 1, 2))

table_dogbreeds<-(table(dogs$Breed1))
table_dogbreeds<-table_dogbreeds[order(table_dogbreeds)]
df_dogbreeds<-data.frame(table_dogbreeds)

max_table<-max(table(dogs$Breed1))

barplot(df_dogbreeds$Freq[df_dogbreeds$Freq>5],
        las = 1,
        cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.7, 
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
        cex.names=0.7, 
        xlim = c(0, 250),
        cex.axis=0.7,
        bg="lightgrey",
        names=df_dogbreeds$Var1[df_dogbreeds$Freq>5& df_dogbreeds$Var1!="Mixed Breed"],
        col="darkred",
        #cex.main=1,
        #main="Frequencies (>5) of Dog Breeds in the Petdata Dataset")
)
title(main = "Dog Breeds - Frequencies (>5) / and without 'Mixed Breed'", 
      line = -2.4, 
      cex.main=1.1,
      font.main=2,
      #col.main="darkorange",
      outer = TRUE,
      adj=0.38)
#axis(1, at = seq(0, 200, by = 25), labels = FALSE, tick = TRUE)
#axis(1, at = seq(0, 200, by = 25), labels = TRUE, las = 1, cex.axis= 0.7)

# ---- cat-breeds plot ---------------------------------------------------

par(mar = c(3, 5, 3, 3)+0.1, mgp=c(0,0.3,-0.2), oma = c(1, 5, 1, 5))

table_catbreeds<-(table(cats$Breed1))
table_catbreeds<-table_catbreeds[order(table_catbreeds)]
df_catbreeds<-data.frame(table_catbreeds)

max_breed_cats<-max(table(cats$Breed1))

barplot(df_catbreeds$Freq[df_catbreeds$Freq>5],
        las = 1,
        #cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.7, 
        xlim = c(0, 4000),
        cex.axis=0.7,
        names=df_catbreeds$Var1[df_catbreeds$Freq>5],
        col="darkred",
        #main="Frequencies (>5) of Cat Breeds in the Petdata Dataset")
)

barplot(df_catbreeds$Freq[df_catbreeds$Freq>5 & df_catbreeds$Var1!="Domestic Short Hair" & df_catbreeds$Var1!="Domestic Medium Hair"],
        las = 1,
        #cex.lab = 1,
        horiz = TRUE, 
        cex.names=0.7, 
        xlim = c(0, 400),
        cex.axis=0.7,
        names=df_catbreeds$Var1[df_catbreeds$Freq>5& df_catbreeds$Var1!="Domestic Short Hair"& df_catbreeds$Var1!="Domestic Medium Hair"],
        col="darkred",
        #main="Frequencies (>5) of Cat Breeds in the Petdata Dataset")
)

title(main = "Cat Breeds - Frequencies (>5) and without Domestic Short and Medium Hair", 
      line = -2.3, 
      cex.main=1,
      font.main=2,
      #col.main="darkorange",
      outer = TRUE,
      adj=0.6)


# --------- fading sentiment ----------

# quick look into the sentiment data:
summary(petdata[names(petdata) == c("SentimentScore", "SentimentMagnitude")])
hist(petdata$SentimentScore, xlim = c(-1, 1), ylim = c(0, 2500))
boxplot(petdata$SentimentScore)


# -- better fading option ---- 
# reference:
# https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient

# gradient function:

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

# make the gradient background:
g <- make_gradient(
  deg = -180, n = 500, cols = brewer.pal(9, "RdBu")
)

# ----plots with gradient ---------------------


p2<-ggplot(data=subset(petdata, !is.na(SentimentScore)), 
           aes(SentimentScore, fct_rev(Health))) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Pets Health Status for Sentiment Score") +
  geom_boxplot(alpha=0.2)+
  ylab("Health")+
  xlab("Sentiment Score")+
  xlim(-1.0,1.0)+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  )


p4<-ggplot(data=subset(petdata, !is.na(SentimentScore)), 
           aes(SentimentScore, fct_rev(AdoptionSpeed_fac))) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Pets Adoption Speed for Sentiment Score") +
  geom_violin(alpha=0)+
  ylab("Adoption Speed")+
  xlab("Sentiment Score")+
  xlim(-1.0,1.0)+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  )

  
p3<-ggplot(data=subset(dogs, !is.na(SentimentScore)),
           aes(SentimentScore)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Dogs - Sentiment Scores Frequencies") +
  geom_bar()+
  xlab("Sentiment Score")+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8,),
    axis.title.y = element_text(color="black", size=8,)
  )

p1<-ggplot(data=subset(cats, !is.na(SentimentScore)),
           aes(SentimentScore))+
               #fill=Gender,
               #group=Gender)) +
  #scale_fill_brewer(palette="Greys")+
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Cats - Sentiment Scores Frequencies") +
  geom_bar()+
  xlab("Sentiment Score")+
  labs(fill = "Gender")+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8),
    legend.key.size = unit(0.5, 'cm'), #change legend key size
    legend.key.height = unit(0.5, 'cm'), #change legend key height
    legend.key.width = unit(0.5, 'cm'), #change legend key width
    legend.title = element_text(size=7), #change legend title font size
    legend.text = element_text(size=7), #change legend text font size
    legend.direction = "horizontal",
    legend.position = "bottom"
  )


p5<-ggplot(data=subset(petdata, !is.na(SentimentScore)), 
           aes(SentimentScore, MaturitySize)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Pets Maturity Size for Sentiment Score") +
  geom_violin(alpha=0)+
  ylab("Maturity Size")+
  xlab("Sentiment Score")+
  xlim(-1.0,1.0)+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8),
  )

p6<-ggplot(data=subset(petdata, !is.na(SentimentScore)), 
           aes(SentimentScore, Age)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Pets Age for Sentiment Score") +
  geom_jitter(alpha=0.3)+
  ylab("Age in Months")+
  xlab("Sentiment Score")+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  )


p7<-ggplot(data=subset(petdata, !is.na(SentimentScore)), 
  aes(SentimentScore, State)) +
  annotation_custom(
    grob = g, xmin = Inf, xmax = -Inf, ymin = Inf, ymax = -Inf
  ) + 
  ggtitle("Pet's State for Sentiment Score") +
  geom_boxplot(alpha=0)+
  ylab("State")+
  xlab("Sentiment Score")+
  xlim(-1.0,1.0)+
  theme(
    plot.margin=unit(c(0.6,0.8,0.8,0.6),"cm"),
    plot.title = element_text(color="black", size=10, face="bold"),
    axis.title.x = element_text(color="black", size=8),
    axis.title.y = element_text(color="black", size=8)
  )

grid.arrange(p1, p2, p3, p5, p6, p7,
             nrow=3, ncol=2, 
             top = "Exploring Sentiment in Descriptions",
             vp=viewport(width=0.9, height=0.9))

# --- sentiment + other variables -----

custom.col <- c("black", "#C4961A", "#F4EDCA", 
                "#FFDB6D",  "darkgrey",
               "lightgrey","yellow")

ggplot() +
  geom_jitter(data=dogs,
              aes(x=SentimentScore,
                  y=PhotoAmt, 
                  color=Color1, 
                  shape=Health, 
                  size=Age))+
  scale_colour_manual(values= custom.col)+
  theme_minimal()



# --- Show the pet locations on a interactive map -----------------------------


source("leaflet_petlocations.R")

head(cats)
