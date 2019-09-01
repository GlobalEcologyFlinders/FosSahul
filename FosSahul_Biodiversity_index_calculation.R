#-------------------------------------------------------------
# Biodiversity metrics calculations for FosSahul2.0 database
#
# 
# by Katharina J. Peters1, Frédérik Saltré, Tobias Friedrich, Zenobia Jacobs, Rachel Wood, Matthew McDowell, Sean Ulm and Corey, J. A. Bradshaw
#-------------------------------------------------------------


#SET YOUR WORKING DIRECTORY

#source the data import script
source("FosSahul_Data import.R")

library(ggplot2)
library(divDyn)
library(data.table)
library(dplyr)


#limit dataset to mega fauna only
BIdata = FS_QR[(FS_QR$Megafauna..40kg.=="Yes"),]

############################################################################################################


# Richness analysis of full datast

#import time bins
MyBins = read.csv("~/TimeBins.csv")


#assign time bins to data
obs = data.frame(BIdata$Age.calibrated)
seg = data.frame(MyBins$top, MyBins$bottom, MyBins$bin)

f <- function(x)
{
  which.max(c((seg["MyBins.top"] <= c(x["BIdata.Age.calibrated"]) ) &
                  (seg["MyBins.bottom"] >  c(x["BIdata.Age.calibrated"]) ),
                TRUE                                      ) )
}

X <- cbind(obs, segment = seg$MyBins.bin[apply(obs,1,f)] )

#add bin number to original dataset
BIdata$bin = X$segment

#deleting records that have not been identified to genus level
BIdata = BIdata[!grepl("indet", BIdata$Genus, ignore.case = T),]

#----------------------------------------------------------------------------

# calculating biodiversity metrics

BImetrics = divDyn(BIdata, tax = "Genus", bin = "bin")

#export data
#write.csv(BImetrics, file = "BiodiversityMetricsFosSahul.csv", row.names = FALSE)


############################################################################################################

# Richness analysis of subset (southeastern Australia)


#limit dataset to southeastern Australia
FS_QR$Latitude = as.numeric(as.character(FS_QR$Latitude))
FS_QR$Longitude = as.numeric(as.character(FS_QR$Longitude))
FS_QR_SE <- FS_QR[!(FS_QR$Longitude < 130 | FS_QR$Latitude > -24),]

#limit dataset to mega fauna only
BIdataSE = FS_QR_SE[(FS_QR_SE$Megafauna..40kg.=="Yes"),]

#assign time bins to data
obs = data.frame(BIdataSE$Age.calibrated)
seg = data.frame(MyBins$top, MyBins$bottom, MyBins$bin)

f <- function(x)
{
  which.max(c((seg["MyBins.top"] <= c(x["BIdataSE.Age.calibrated"]) ) &
                (seg["MyBins.bottom"] >  c(x["BIdataSE.Age.calibrated"]) ),
              TRUE                                      ) )
}

X <- cbind(obs, segment = seg$MyBins.bin[apply(obs,1,f)] )

#add bin number to original dataset
BIdataSE$bin = X$segment

#deleting records that have not been identified to genus level
BIdataSE = BIdataSE[!grepl("indet", BIdataSE$Genus, ignore.case = T),]

#----------------------------------------------------------------------------

# calculating biodiversity metrics

BImetricsSE = divDyn(BIdataSE, tax = "Genus", bin = "bin")

#export data
#write.csv(BImetricsSE, file = "BiodiversityMetricsFosSahulSE.csv", row.names = FALSE)

