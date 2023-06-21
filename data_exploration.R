
# ---- Data Exploration --------------------------------------------------------

# ---- load data from Setup.R file -------
source(Setup.R)


# ---- plots and visualizations -----------
hist(train$AdoptionSpeed)

names_num <- names(which(sapply(train, is.numeric)))
names_num
length(names_num)

#pairs(train[, names_num[1:10]])
#pairs(train[, names_num[11:20]])
pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt")])

pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt", "Health","Vaccinated")])



?corrplot
# corr_mat=cor(train[, names_num], method="s") #create Spearman correlation matrix

corr_mat=cor(train[,c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt", "Health","Vaccinated","AdoptionSpeed")], method="s")

corrplot(corr_mat, method = "color",
         type = "upper", order = "hclust", 
         tl.col = "black", tl.cex=1) 

hist(train$Breed1)

# quick look into the sentiment data:
summary(document_sentiment_df) 
hist(document_sentiment_df$score, xlim=c(-1,1), ylim=c(0,2500))
boxplot(document_sentiment_df$score)
