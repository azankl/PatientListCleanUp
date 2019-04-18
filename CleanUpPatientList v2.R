# Goal is to merge Patient List from Moey, updated by AZ, and Sample List from Tracy into a Master List
# and clean up any errors

library(tidyverse)
library(readxl)
library(here)

#read Patient List
PatientList <- read_excel(here("PatientList_updatedAZ.xlsx"))

#remove blank rows 
PatientList <- filter_all(PatientList, any_vars(. != 0))

# make family name all caps
PatientList$FamilyName <- str_to_upper(PatientList$FamilyName)

# clean up factors
library(forcats)
PatientList <- mutate(PatientList, Sex = fct_collapse(Sex, M = c("male","Male"), F = c("female","Female")))
PatientList <- mutate(PatientList, Affected = fct_recode(Affected,"yes"="Yes", "no"="No"))

#rename G1Tube column to SampleID column
PatientList <- rename(PatientList, SampleID = G1TubeID)

#read Sample List
SampleList <-read_excel(here("SampleList_Tracy.xlsx"))

#swap AltID with SampleID if it exits (AltID matches G1TubeID in PatientList )
SampleList <- SampleList %>% 
  mutate (
    SampleID_Temp = SampleID, #store current SampleID before it is overwritten
    SampleID = if_else(AltID != "none", AltID, SampleID), #replace with AltID where it exists
    AltID = if_else(AltID != "none", SampleID_Temp, "none"), #store previous SampleID in AltID
    SampleID_Temp = NULL #delete the temporary variable for SampleID
    )

#combine both lists
CombinedList <- left_join(SampleList, PatientList, by = "SampleID")
#Note: SampleList contains 167 samples, PatientList 170 samples, the additional 3 samples are:
#Sample ID FD02523296 failed library prep, see Labarchives entry for Batch6 R_170717
#LabID 163550405 was sent with R_170717 but processed as a clinical sample by GenomeOne, report in Labarchives
#RID 42-1 was sent previously for clinical WGS by GenomeOne

#find Samples with no patient information
MissingDataList <- CombinedList %>% filter(is.na(FamilyName)) %>% select(Dataset, SampleID)

#check matching sex information
WrongSexList <- CombinedList %>% filter(Gender != Sex) %>% select(FamilyName, FirstName, Gender, Sex)
# initially 2 sex mismatches in PatientList, now corrected, Gender in SampleList is correct

#Count samples in each batch
CombinedList %>% count(Manifest)

#add sample FR07887668 orginally sequenced by GenomeOne
CombinedList <- add_row(CombinedList,
                            FamilyName = 'RITCHIE',
                            FirstName = 'Harrison',
                            DOB = '7/5/2013',
                            SampleID = 'FR07887668',
                            Manifest = '16F00039',
                            Gender = 'M',
                            Affected = "yes",
                            RID = '42-1',
                            FatherRID = '42-2',
                            MotherRID = '42-3')

# remove identifiers and reorder columns
CombinedListAnon <- CombinedList %>% select(SampleID, Manifest, Gender, Affected, RID, FatherRID, MotherRID)

#write to disk
readr::write_csv(CombinedListAnon, here("SampleInfo_updated.csv"))


