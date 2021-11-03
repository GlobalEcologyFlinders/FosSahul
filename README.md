# <em>FosSahul</em>
Palaeontological Database

PETERS, KJ, F SALTRÉ, T FRIEDRICH, Z JACOBS, R WOOD, M MCDOWELL, S ULM, CJA BRADSHAW. <a href="https://www.nature.com/articles/s41597-019-0267-3"><em>FosSahul</em> 2.0, an updated database for the Late Quaternary fossil records of Sahul</a>. <em>Scientific Data</em> 6:272

The <em>R</em> code included automates the age-reliability assessment from: RODRÍGUEZ-REY, M, S HERRANDO-PÉREZ, R GILLESPIE, Z JACOBS, F SALTRÉ, ..., CJA BRADSHAW. 2015. Criteria for assessing the quality of Middle Pleistocene to Holocene vertebrate fossil ages. <em>Quaternary Geochronology</em> 30: 69-79. http://doi.org/10.1016/j.quageo.2015.08.002
 
## Text & Excel files:
 
- FosSahul2.0.csv: <em>FosSahul</em> database collating non-human vertebrate megafauna fossil records for the Sahul region. Note that location data have been rounded to one degree decimal and might not reflect the exact location of the fossil record. For more information on precise locations, contact the authors.
- FosSahul2.0_metadata.xlsx: Column description and further detail on the <em>FosSahul</em> 2.0 database.
- CalibratedC14Dates_FosSahul.csv: Calibrated radiocarbon dates for <em>FosSahul</em> 2.0. Needed for the calculation of the biodiversity index.
TimeBins.csv: Time bins needed for the calculation of the biodiversity index.
 
## <em>R</em> scripts:
 
- <code>FosSahul_Rating.R</code>: Quality-rating algorithm for the <em>FosSahul</em> database.
- <code>FosSahul_Data import.R</code>: Data import script necessary for the calculation of the biodiversity index.
- <code>FosSahul_BiodiversityIndex_calculation.R</code>: Code for the calculation of the biodiversity index.
 
