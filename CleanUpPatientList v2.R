# Goal is to merge Patient List from Moey, updated by AZ, and Sample List from Tracy into a Master List
# and clean up any errors

library(tidyverse)
library(readxl)
library(here)

#read Patient List
PatientList <- read_excel(here("PatientList_updatedAZ.xlsx"))
#NOTE: importing DOB does not work correctly as our DOB column in Excel has incomplete and missing dates

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

#add sample FR07887668 (RID 42-1) orginally sequenced by GenomeOne
addPatient<-read_csv(here("sample_FR07887668.csv"))
CombinedList <- bind_rows(CombinedList, addPatient)
#NOTE: throws warning about coercing factor into char, thats OK

#Count samples in each batch
CombinedList %>% count(Manifest)

#make Gender and Affected a Factor
CombinedList$Gender <- as.factor(CombinedList$Gender)
CombinedList$Affected <- as.factor(CombinedList$Affected)

#add a Family_ID column
FamilyID_pattern <- "[0-9]*"
CombinedList <- add_column(CombinedList, FamilyID = as.numeric(str_match(CombinedList$RID, FamilyID_pattern)[,1]), .before = 'RID')

#add Relationship column (does not work)
#CombinedList <- CombinedList %>% filter(!is.na(FatherRID)) %>% mutate (Relation = "father")

# remove identifiers and reorder columns
CombinedListAnon <- CombinedList %>% select(SampleID, Manifest, Gender, Affected, RID, FatherRID, MotherRID)

#write sample list for Alex Drew to disk
write_csv(CombinedListAnon, here("SampleInfo_updated.csv"))

#make another sample list with a Family_ID column
MasterList <- CombinedList %>% select(SampleID, FamilyName, FirstName, Gender, Affected, RID, FatherRID, MotherRID)

#make list for Scott
OI_like <- c(30,59,20,51,15,41,16,7,11,3)
abnormal_mineralisation <- c(2,25)
OIlike_List <- MasterList %>% select(SampleID, Gender, Affected, FamilyID, RID, FatherRID, MotherRID) %>% filter(FamilyID %in% OI_like)
abnormalMineralisation_List <- MasterList %>% select(SampleID, Gender, Affected, FamilyID, RID, FatherRID, MotherRID) %>% filter(FamilyID %in% abnormal_mineralisation)
write_csv(OIlike_List, here("OILikeList.csv"))
write_csv(abnormalMineralisation_List, here("abnormalMineralisation_List.csv"))

#make list for Notion Family Page
NotionList <- MasterList %>% select(FamilyID, SampleID)
write_csv(NotionList, here("NotionList.csv"))

#make list for Catherine Vacher
List_for_Catherine <- PatientList %>% select(RID, DOB, Sex, Affected )
write_csv(List_for_Catherine, here("List_for_Catherine.csv"))
