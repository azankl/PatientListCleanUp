# Goal is to merge Patient List from Moey and Sample List from Tracy into a Master List
# and clean up any errors

library(tidyverse)
library(readxl)
library(here)

PatientList <- read_excel(here("PatientList_Moey.xlsx"))
SampleList <-read_excel(here("SampleList_Tracy.xlsx"))

#remove blank rows 
PatientList <- filter_all(PatientList, any_vars(. != 0))

#rename G1Tube column to SampleID column
PatientList <- rename(PatientList, SampleID = G1TubeID)

#swap AltID with SampleID if it exits
SampleList <- SampleList %>% 
  mutate (
    SampleID_Temp = SampleID, #store current SampleID before it is overwritten
    SampleID = if_else(AltID != "none", AltID, SampleID), #replace with AltID where it exists
    AltID = if_else(AltID != "none", SampleID_Temp, "none"), #store previous SampleID in AltID
    SampleID_Temp = NULL #delete the temporary variable for SampleID
    )

#combine both lists
MyList <- left_join(SampleList, PatientList, by = "SampleID")

#find Samples with no patient information
MissingDataList <- MyList %>% filter(is.na(FamilyName)) %>% select(Dataset, SampleID)


