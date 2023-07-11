# ---- Load the libraries -----------------------------------------------------


library(jsonlite)
library(readr)
library(corrplot)
library(ggplot2)
library(stringr)
library(plyr)
library(dplyr)
library(psych)
library(inspectdf)
library(skimr)
library(RColorBrewer)
library(forcats)
library(gridExtra)
# not 100% sure if needed:
library(grid) # needed?
library(ggplotify) #needed?
library(viridis) #needed?
 
# For the map
library(rgdal)
library(sp)
library(leaflet)
library(sf)
library(base64enc)

# ---- Descriptions -----------------------------------------------------------


# AdoptionSpeed
# The value is determined by how quickly, if at all, a pet is adopted.
# The values are determined in the following way:
# 0 - Pet was adopted on the same day as it was listed.
# 1 - Pet was adopted between 1 and 7 days (1st week) after being listed.
# 2 - Pet was adopted between 8 and 30 days (1st month) after being listed.
# 3 - Pet was adopted between 31 and 90 days (2nd & 3rd month) after being listed.
# 4 - No adoption after 100 days of being listed.
# (There are no pets in this dataset that waited between 90 and 100 days).

# c("same day","1 to 7 days", "8 to 30 days", "31 to 90 days", "+90 days")

# ---- clean the workspace: ------------------------------------

rm(list = ls(all.names = TRUE))

# ---- Data import ------------------------------------------------------------


# Use only the train data because test data does not include AdoptionSpeed
petdata <- read_csv("petdata/train/train.csv", show_col_types = FALSE)

breed_lab <- subset(read_csv("petdata/PetFinder-BreedLabels.csv",
                    show_col_types = FALSE), select = -c(Type))
color_lab <- read_csv("petdata/PetFinder-ColorLabels.csv",
                    show_col_types = FALSE)
state_lab <- read_csv("petdata/PetFinder-StateLabels.csv",
                    show_col_types = FALSE)


# ---- Extract sentiment analysis data to the dataframe -----------------------


# Path to the folder containing JSON files
folder_path <- "petdata/train_sentiment"

# Get a list of JSON file names in the folder
json_files <- list.files(folder_path, pattern = ".json$", full.names = TRUE)

# Create an empty data frame with columns for "score" and "magnitude"
sentiment_df <- data.frame(
  SentimentScore = numeric(),
  SentimentMagnitude = numeric(),
  PetID = character(),
  stringsAsFactors = FALSE
)

# Process each JSON file
for (json_file in json_files) {
  # Read the JSON file as text
  json_text <- readLines(json_file, warn = FALSE)
  # Parse the JSON text:
  json_data <- fromJSON(txt = json_text)

  # Extract document-level sentiment
  score <- json_data$documentSentiment$score
  magnitude <- json_data$documentSentiment$magnitude

  # Extract the file name:
  file_name <- basename(json_file)
  pattern <- str_extract(file_name, "[A-Za-z0-9]+")

  # Create a data frame for document-level sentiment
  df_new_row <- data.frame(SentimentScore = score,
                          SentimentMagnitude = magnitude,
                          PetID = pattern)
  sentiment_df <- rbind(sentiment_df, df_new_row)
}



# ---- Merge the labels and sentiment analysis data to the petdata ------------


# Add the sentiment score and maginitude to the petdata
petdata <- merge(petdata, sentiment_df, by = "PetID",
                all.x = TRUE, sort = FALSE)

# Merge the breed, color, and state labels to the petdata
petdata <- merge(petdata, breed_lab, by.x = "Breed1", by.y = "BreedID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "Breed1"] <- "BreedID1"
names(petdata)[names(petdata) == "BreedName"] <- "Breed1"

petdata <- merge(petdata, breed_lab, by.x = "Breed2", by.y = "BreedID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "Breed2"] <- "BreedID2"
names(petdata)[names(petdata) == "BreedName"] <- "Breed2"

petdata <- merge(petdata, color_lab, by.x = "Color1", by.y = "ColorID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "Color1"] <- "ColorID1"
names(petdata)[names(petdata) == "ColorName"] <- "Color1"

petdata <- merge(petdata, color_lab, by.x = "Color2", by.y = "ColorID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "Color2"] <- "ColorID2"
names(petdata)[names(petdata) == "ColorName"] <- "Color2"

petdata <- merge(petdata, color_lab, by.x = "Color3", by.y = "ColorID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "Color3"] <- "ColorID3"
names(petdata)[names(petdata) == "ColorName"] <- "Color3"

petdata <- merge(petdata, state_lab, by.x = "State", by.y = "StateID",
                all.x = TRUE, sort = FALSE)
names(petdata)[names(petdata) == "State"] <- "StateID"
names(petdata)[names(petdata) == "StateName"] <- "State"

# ---- factorize categorical variables ----------------------------------------

#petdata$SentimentScore_fac<-as.factor(petdata$SentimentScore)

petdata$SentimentScore_fac <- cut(petdata$SentimentScore,
                      breaks = c(-1, -0.4, -0.1, 0.1, 0.4, 1),
                      include.lowest = T,
                      right = F)
levels(petdata$SentimentScore_fac)<-c("negative", "moderately negative", "neutral", "moderately positive", "positive")
petdata$ColorID1<-as.factor(petdata$ColorID1)
petdata$ColorID2<-as.factor(petdata$ColorID2)
petdata$ColorID3<-as.factor(petdata$ColorID3)
petdata$StateID<-as.factor(petdata$StateID)
petdata$Type<-as.factor(petdata$Type)
levels(petdata$Type)<-c("Dog","Cat")
petdata$AdoptionSpeed_fac<-as.factor(petdata$AdoptionSpeed)
levels(petdata$AdoptionSpeed_fac)<-c("same day","1 to 7 days", "8 to 30 days", "31 to 90 days", "+90 days")
petdata$Dewormed<-as.factor(petdata$Dewormed)
levels(petdata$Dewormed)<-c("Yes","No","Not Sure")
petdata$Sterilized<-as.factor(petdata$Sterilized)
levels(petdata$Sterilized)<-c("Yes","No","Not Sure")
petdata$Vaccinated<-as.factor(petdata$Vaccinated)
levels(petdata$Vaccinated)<-c("Yes","No","Not Sure")
petdata$Health<-as.factor(petdata$Health)
levels(petdata$Health)<-c("Healthy", "Minor Injury", "Serious Injury")
petdata$MaturitySize<-as.factor(petdata$MaturitySize)
levels(petdata$MaturitySize)<-c("Small", "Medium", "Large", "Extra Large")
petdata$FurLength<-as.factor(petdata$FurLength)
levels(petdata$FurLength)<-c("Short", "Medium", "Long")
petdata$Gender<-as.factor(petdata$Gender)
levels(petdata$Gender)<-c("Male", "Female", "Mixed/Group")

# ---- Split data for dogs and cats -------------------------------------------


dogs <- subset(petdata[petdata$Type == "Dog", ], select = -c(Type))
cats <- subset(petdata[petdata$Type == "Cat", ], select = -c(Type))


# ---- fading function ------------------------------------------------------
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


?colorRampPalette
# ---- Clean up the environment -----------------------------------------------


rm(breed_lab, state_lab, folder_path, json_files, json_text,
  json_file, json_data, score, magnitude, file_name, pattern, df_new_row,
  sentiment_df)


# ---- Breed check ------------------------------------------------------------


# par(mar = c(5, 4, 4, 2) + 10)
# barplot(table(petdata$Breed1), xlab = "Breed1", las=2, cex.lab=0.5, horiz=TRUE)
# dim(table(petdata$Breed1))
# count <- sum(petdata$BreedID1 != 0)
# count
# library(dplyr)

# # Count the number of distinct non-zero BreedIDs
# count <- petdata %>% filter(BreedID1 != 0) %>% distinct(BreedID1) %>% nrow(); count


# ---- check number of animals that have pictures ------------------------

## Path to the folder containing JSON files
# folder_path2 <- "petdata/train_images"

## Get a list of JSON file names in the folder
# image_files <- list.files(folder_path2, pattern = ".jpg$", full.names = TRUE)

# pet_images<-list()
# for (image_file in image_files) {
#  # Process each image file name:
  
#  #extract the pet_ID:
#  file_name <- basename(image_file)
#  pattern <- str_extract(file_name, "[A-Za-z0-9]+")
#    # Create a data frame for document-level sentiment
#    pet_images<-append(pet_images, pattern)
    
#  }
  
#  # number of distinct pet_ids that have an image:
#  
  # create df with pet_ids and image count
#  pet_images_df<-data.frame("Pet_id"=unlist(pet_images,use.names=FALSE )) 
#  pet_images_df<-add_count(pet_images_df, Pet_id) # add a group count column based on Pet_id
#  pet_images_df<-unique(pet_images_df)