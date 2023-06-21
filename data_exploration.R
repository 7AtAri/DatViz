# ---- Data Exploration --------------------------------------------------------

# ---- Load data and libraries from Setup.R file -------------------------------
source("Setup.R")


# ---- plots and visualizations -----------
hist(petdata$AdoptionSpeed)

names_num <- names(which(sapply(petdata, is.numeric)))
names_num
length(names_num)

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
