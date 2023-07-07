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
                                                "AdoptionSpeed",
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
