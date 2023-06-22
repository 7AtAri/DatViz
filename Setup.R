# ---- Load the libraries -----------------------------------------------------


library(jsonlite)
library(readr)
library(corrplot)
library(ggplot2)
library(stringr)
library(dplyr)


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


# ---- Split data for dogs and cats -------------------------------------------


dogs <- subset(petdata[petdata$Type == 1, ], select = -c(Type))
cats <- subset(petdata[petdata$Type == 2, ], select = -c(Type))


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