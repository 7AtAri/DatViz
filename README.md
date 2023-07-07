# DatViz - Pet Adoption Dataset

TODOs:

* Legend for pairs.panel plot -> orange: cats, purple: dogs
(steps: reimport the png -> make grid -> 1 panel with png + 1 panel with legend)
* make bars in sentiment plots more visible [ARI]
* 3d plot of tsne datapoints with clickable information [ARI]
* refine maps -> add population and/or income data -> log scale to numbers -> hover info [Vipin]
* take the highest influencing variables from regression summary to do some interesting stuff with them
* maybe also visualize interactions between variables (check regression output)
* develop a narrative / story through our data and plots for presentation

our dataset:
https://www.kaggle.com/competitions/petfinder-adoption-prediction/data

+ sentiment analysis data as json files -> converted to data frame in the data_exploration.R file
+ images -> neural network -> embedding -> visualize clusters with r-tsne

Data Fields in train and test data:

* PetID - Unique hash ID of pet profile
* AdoptionSpeed - Categorical speed of adoption. Lower is faster.
* Type - Type of animal (1 = Dog, 2 = Cat)
* Name - Name of pet (Empty if not named)
* Age - Age of pet when listed, in months
* Breed1 - Primary breed of pet (Refer to BreedLabels dictionary)
* Breed2 - Secondary breed of pet, if pet is of mixed breed (Refer to BreedLabels dictionary)
* Gender - Gender of pet (1 = Male, 2 = Female, 3 = Mixed, if profile represents group of pets)
* Color1 - Color 1 of pet (Refer to ColorLabels dictionary)
* Color2 - Color 2 of pet (Refer to ColorLabels dictionary)
* Color3 - Color 3 of pet (Refer to ColorLabels dictionary)
* MaturitySize - Size at maturity (1 = Small, 2 = Medium, 3 = Large, 4 = Extra Large, 0 = Not Specified)
* FurLength - Fur length (1 = Short, 2 = Medium, 3 = Long, 0 = Not Specified)
* Vaccinated - Pet has been vaccinated (1 = Yes, 2 = No, 3 = Not Sure)
* Dewormed - Pet has been dewormed (1 = Yes, 2 = No, 3 = Not Sure)
* Sterilized - Pet has been spayed / neutered (1 = Yes, 2 = No, 3 = Not Sure)
* Health - Health Condition (1 = Healthy, 2 = Minor Injury, 3 = Serious Injury, 0 = Not Specified)
* Quantity - Number of pets represented in profile
* Fee - Adoption fee (0 = Free)
* State - State location in Malaysia (Refer to StateLabels dictionary)
* RescuerID - Unique hash ID of rescuer
* VideoAmt - Total uploaded videos for this pet
* PhotoAmt - Total uploaded photos for this pet
* Description - Profile write-up for this pet. The primary language used is English, with some in Malay or Chinese.


Questions:

- will we only use the train data? -> YES! (Because test data has no AdoptionSpeed column)
