
# Get the current working directory
wd <- getwd()

# Print the current working directory
print(wd)

library(readr)
library(corrplot)
library(ggplot2)

# AdoptionSpeed
# The value is determined by how quickly, if at all, a pet is adopted. 
# The values are determined in the following way: 
# 0 - Pet was adopted on the same day as it was listed. 
# 1 - Pet was adopted between 1 and 7 days (1st week) after being listed. 
# 2 - Pet was adopted between 8 and 30 days (1st month) after being listed. 
# 3 - Pet was adopted between 31 and 90 days (2nd & 3rd month) after being listed. 
# 4 - No adoption after 100 days of being listed. 
# (There are no pets in this dataset that waited between 90 and 100 days).


train <- read_csv("petdata/train/train.csv") 
View(train)

names_num <- names(which(sapply(train, is.numeric)))
names_num
length(names_num)

#pairs(train[, names_num[1:10]])
#pairs(train[, names_num[11:20]])
pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt")])

pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt", "Health","Vaccinated")])


BreedLabels <- read_csv("petdata/PetFinder-BreedLabels.csv")
ColorLabels <- read_csv("petdata/PetFinder-ColorLabels.csv")
StateLabels <- read_csv("petdata/PetFinder-StateLabels.csv")

?corrplot
# corr_mat=cor(train[, names_num], method="s") #create Spearman correlation matrix

corr_mat=cor(train[,c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt", "Health","Vaccinated")], method="s")

corrplot(corr_mat, method = "color",
         type = "upper", order = "hclust", 
         tl.col = "black", tl.cex=1) 

