#This script turns a PED file into a CSV for creating a database in Notion
#It will add additional columns to the file as well

library(tidyverse)

pedfile <- read_tsv("AZ.sorted.ped", col_names = c("FamilyID", "SampleID", "FatherID", "MotherID", "Sex_numeric", "Affected_numeric"))

#add Sex column
pedfile <- pedfile %>% mutate(Sex = case_when(
  Sex_numeric == 1 ~ "male",
  Sex_numeric == 2 ~ "female",
  TRUE    ~  NA_character_
))

#add Affected column
pedfile <- pedfile %>% mutate(Affected = case_when(
  Affected_numeric == 1 ~ "no",
  Affected_numeric == 2 ~ "yes",
  TRUE    ~  NA_character_
))

#Add Relation column
pedfile <- pedfile %>% mutate(Relation = case_when(
  SampleID %in% FatherID ~ "father",
  SampleID %in% MotherID ~ "mother",
                    TRUE ~ "proband"
))
  
#Add Manifest column
SampleInfo <- read_csv("SampleInfo_updated.csv") %>%
  select(SampleID, Manifest)
pedfile <- left_join(pedfile, SampleInfo, by="SampleID")

#Keep only what I need
pedfile <- pedfile %>% select(SampleID, FamilyID, Relation, Sex, Affected, Manifest)
write_csv(pedfile, "SampleList_for_Notion.csv")



  