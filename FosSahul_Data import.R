#-------------------------------------------------------------
# Data import for biodiversity metrics calculations using FosSahul2.0 database
#
# 
# by Katharina J. Peters1, Frédérik Saltré, Tobias Friedrich, Zenobia Jacobs, Rachel Wood, Matthew McDowell, Sean Ulm and Corey, J. A. Bradshaw
#-------------------------------------------------------------

#SET YOUR WORKING DIRECTORY

library(dplyr)
library(tm)


#read in FosSahul database
FosSahul <- read.csv(file="~/FosSahul2.0.csv")

FosSahul[ FosSahul == "na" ] <- NA

#remove empty columns at bottom
FosSahul = FosSahul[!is.na(FosSahul$ID), ]

#-----------------------------------------------------------------------------------
#calibrate radiocarbon ages

CalC14D <- read.csv(file="~/CalibratedC14Dates_FosSahul.csv")

#delete NAs
CalC14D = na.omit(CalC14D)

#delete date ranges
FosSahul <- FosSahul[!grepl("null", FosSahul$AgeType, ignore.case = T) &
                                   !grepl("range", FosSahul$AgeType, ignore.case = T),]

#change age in original dataset to years insteadt of ka
FosSahul$Age = as.numeric(as.character(FosSahul$Age))
FosSahul$Age = (1000 * FosSahul$Age)
FosSahul$Precision = as.numeric(as.character(FosSahul$Precision))
FosSahul$Precision = (1000 * FosSahul$Precision)

FosSahul$ID = as.factor(FosSahul$ID)
CalC14D$ID = as.factor(CalC14D$ID)

str(FosSahul)
str(CalC14D)
#Merge with original dataset
FosSahul =left_join(FosSahul, CalC14D, by = "ID")


#joining original ages and calibrated ages (for C14)
FosSahul$Age.calibrated = ifelse(grepl("Radiocarbon", FosSahul$DatingTechnique, ignore.case = TRUE), FosSahul$Age_cal, FosSahul$Age)

#same for precision
FosSahul$Precision.calibrated = ifelse(grepl("Radiocarbon", FosSahul$DatingTechnique, ignore.case = TRUE), FosSahul$Precision.y, FosSahul$Precision.x)

#delete now redundant factors
FosSahul$Age <- NULL
FosSahul$Age_cal <- NULL
FosSahul$Precision.x <- NULL
FosSahul$Precision.y <- NULL


#-----------------------------------------------------------------------------------------------------------


#create dataset with only A and A* rating
FS_QR = FosSahul[grepl("A", FosSahul$Quality, ignore.case = T),] #exclude dates below A quality



