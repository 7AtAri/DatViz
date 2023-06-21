
# ---- Set up: -----------------------------------------------------------------


# Get the current working directory
wd <- getwd()
# Print the current working directory
print(wd)

# Libraries
library("jsonlite")
library(readr)
library(corrplot)
library(ggplot2)
library(stringr)


# ----- Descriptions: ----------------------------------------------------------


# AdoptionSpeed
# The value is determined by how quickly, if at all, a pet is adopted. 
# The values are determined in the following way: 
# 0 - Pet was adopted on the same day as it was listed. 
# 1 - Pet was adopted between 1 and 7 days (1st week) after being listed. 
# 2 - Pet was adopted between 8 and 30 days (1st month) after being listed. 
# 3 - Pet was adopted between 31 and 90 days (2nd & 3rd month) after being listed. 
# 4 - No adoption after 100 days of being listed. 
# (There are no pets in this dataset that waited between 90 and 100 days).


# ---- Data import: ------------------------------------------------------------


# Training data:
train <- read_csv("petdata/train/train.csv") 
View(train)

train$Description

test <- read_csv("petdata/test/test.csv") 
View(test)


BreedLabels <- read_csv("petdata/PetFinder-BreedLabels.csv")
ColorLabels <- read_csv("petdata/PetFinder-ColorLabels.csv")
StateLabels <- read_csv("petdata/PetFinder-StateLabels.csv")



# ---- extracting sentiment analysis data to the dataframe:-----------------------------------------------------


# Path to the folder containing JSON files
folder_path <- "petdata/train_sentiment"

# Get a list of JSON file names in the folder
json_files <- list.files(folder_path, pattern = ".json$", full.names = TRUE)

json_files
# Create an empty data frame with columns for "score" and "magnitude"
document_sentiment_df <- data.frame(score = numeric(),
                                    magnitude = numeric(),
                                    pet_id = character(),
                                    stringsAsFactors = FALSE)


# Process the JSON data as needed
for (json_file in json_files) {
  # Process each JSON file:
  # Read the JSON file as text
  json_text <- readLines(json_file, warn = FALSE)
  # 2. Parse the JSON text:
  json_data <- fromJSON(txt = json_text)
  # Extract document-level sentiment
  score <- json_data$documentSentiment$score
  magnitude <- json_data$documentSentiment$magnitude
  #extract the file name:
  file_name <- basename(json_file)
  pattern <- str_extract(file_name, "[A-Za-z0-9]+")
  # Create a data frame for document-level sentiment
  df_new_row <- data.frame(score = score, magnitude = magnitude, pet_id=pattern)
  document_sentiment_df<-rbind(document_sentiment_df, df_new_row)
  
}

# Print the document-level sentiment data frame
print("Document-level Sentiment:")
print(document_sentiment_df)


