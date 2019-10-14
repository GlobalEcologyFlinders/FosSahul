# FosSahul
Palaeontological Database

Forthcoming: PETERS, KJ, F SALTRÉ, T FRIEDRICH, Z JACOBS, R WOOD, M MCDOWELL, S ULM, CJA BRADSHAW. FosSahul 2.0, an updated database for the Late Quaternary fossil records of Sahul. Scientific Data (accepted)

The R code included automates the age-reliability assessment from: RODRÍGUEZ-REY, M, S HERRANDO-PÉREZ, R GILLESPIE, Z JACOBS, F SALTRÉ, ..., CJA BRADSHAW. 2015. Criteria for assessing the quality of Middle Pleistocene to Holocene vertebrate fossil ages. Quaternary Geochronology 30: 69-79. http://doi.org/10.1016/j.quageo.2015.08.002


FosSahul 2.0 database and R code accompanying manuscript "FosSahul 2.0, an updated database for the Late Quaternary fossil records of Sahul" submitted to Scientific Data.
 
Excel files:
 
FosSahul2.0.csv: FosSahul database collating non-human vertebrate megafauna fossil records for the Sahul region. Note that location data have been rounded to one degree decimal and might not reflect the exact location of the fossil record. For more information on precise locations, contact the authors.
FosSahul2.0_metadata.xlsx: Column description and further detail on the FosSahul 2.0 database.
CalibratedC14Dates_FosSahul.csv: Calibrated radiocarbon dates for FosSahul 2.0. Needed for the calculation of the biodiversity index.
TimeBins.csv: Time bins needed for the calculation of the biodiversity index.
 
R-scripts:
 
FosSahul_Rating.R: Quality-rating algorithm for the FosSahul database.
FosSahul_Data import.R: Data import script necessary for the calculation of the biodiversity index.
FosSahul_BiodiversityIndex_calculation.R: Code for the calculation of the biodiversity index.
 
