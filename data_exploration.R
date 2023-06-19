
# Get the current working directory
wd <- getwd()

# Print the current working directory
print(wd)


library(readr)
train <- read_csv("petdata/train/train.csv")
View(train)

names_num <- names(which(sapply(train, is.numeric)))
names_num
length(names_num)

#pairs(train[, names_num[1:10]])
#pairs(train[, names_num[11:20]])
pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt")])

pairs(train[c("Age","Breed1","Fee","Quantity","VideoAmt","PhotoAmt", "Health","Vaccinated")])
