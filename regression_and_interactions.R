# ---- Load data and libraries from Setup.R file -------------------------------
source("Setup.R")

# ---- Regression --------------------------------------------------------------

library(ordinal)

petdata_reduced<-petdata[,!(names(petdata)%in%c("PetID", 
                                                "Name", 
                                                "Description", 
                                                "Breed1", 
                                                "Breed2", 
                                                "Color1",
                                                "Color2",
                                                "Color3",
                                                "State",
                                                "SentimentScore_fac",
                                                "ColorID1_num",
                                                "AdoptionSpeed_fac",
                                                "RescuerID",
                                                "StateID",
                                                "ColorID3",
                                                "ColorID2",
                                                "BreedID2",
                                                "BreedID1"))]
petdata_reduced_nonas<-na.omit(petdata_reduced)
clr_adopt_speed<-clm(AdoptionSpeed_fac~. + Type:Age + 
                       PhotoAmt:Health + 
                       SentimentScore:PhotoAmt + 
                       SentimentScore:SentimentMagnitude+
                       Age:Gender+
                       Quantity:PhotoAmt, 
                     data=petdata_reduced_nonas, 
                     Trace=FALSE)

best_model_int<-step(clr_adopt_speed)
summary(best_model_int)

# ------- multi linear regression on sentiment score ---------
library(MASS)
lm_out <- lm(SentimentScore ~. + 
               Type:Age + 
               AdoptionSpeed_fac:Health + 
               AdoptionSpeed_fac:Vaccinated+
               PhotoAmt:Health + 
               PhotoAmt:SentimentMagnitude + 
               Health:Age+
               Age:Gender+
               Quantity:PhotoAmt, 
             data=petdata_reduced_nonas)
best_model <- stepAIC(lm_out, direction="both")
summary(best_model)
best_model$anova

library(dplyr)
# ------ for dogs: --------
dogs_reduced<-dogs[,!(names(dogs)%in%c("PetID", "Name", 
                                                "Description", 
                                                "Breed1", 
                                                "Breed2", 
                                                "Color1",
                                                "Color2",
                                                "Color3",
                                                "State",
                                                "SentimentScore_fac",
                                                "AdoptionSpeed_fac",
                                                "ColorID1_num",
                                                "RescuerID",
                                                "StateID",
                                                "ColorID3",
                                                "ColorID2",
                                                "BreedID2",
                                                "BreedID1"))]
dogs_reduced_nonas<-na.omit(dogs_reduced)

lm_out_dogs <- lm(SentimentScore ~. + 
               AdoptionSpeed:Health + 
               AdoptionSpeed:Vaccinated+
               PhotoAmt:Health + 
               PhotoAmt:SentimentMagnitude + 
               Health:Age+
               Age:Gender+
               Quantity:PhotoAmt, 
             data=dogs_reduced_nonas)
best_model_dogs <- stepAIC(lm_out_dogs, direction="both")
summary(best_model_dogs)
best_model_dogs$anova
# --- for cats -------------
cats_reduced<-cats[,!(names(cats)%in%c("PetID", 
                                          "Name", 
                                          "Description", 
                                          "Breed1", 
                                          "Breed2", 
                                          "Color1",
                                          "Color2",
                                          "Color3",
                                          "State",
                                          "SentimentScore_fac",
                                          "ColorID1_num",
                                          "AdoptionSpeed_fac",
                                          "RescuerID",
                                          "StateID",
                                          "ColorID3",
                                          "ColorID2",
                                          "BreedID2",
                                          "BreedID1"))]
cats_reduced_nonas<-na.omit(cats_reduced)


lm_out_cats <- lm(SentimentScore ~. + 
                    AdoptionSpeed:Health + 
                    AdoptionSpeed:Vaccinated+
                    PhotoAmt:Health + 
                    PhotoAmt:SentimentMagnitude + 
                    Health:Age+
                    Age:Gender+
                    Quantity:PhotoAmt, 
                  data=cats_reduced_nonas)
best_model_cats <- stepAIC(lm_out_cats, direction="both")
summary(best_model_cats)