#Make pedigree images from PED file

library(kinship2)
library(tidyverse)

pedfile <- read_tsv("AZ.sorted.ped", col_names = c("FamilyID", "SampleID", "FatherID", "MotherID", "Sex", "Affected"))

#change Affected coding to 0/1
pedfile <- pedfile %>% mutate(Affected = Affected-1)


# #Fix_missing_parents
pedfile <- pedfile %>%
   mutate(FatherID = if_else(((FatherID == "0") & (MotherID != "0")),
                               paste(FamilyID,"1", sep = "-"),
                                                     FatherID))
pedfile2<- pedfile %>% filter ((FatherID == "0") & (MotherID != "0"))

newrows <- pedfile2 %>% mutate (SampleID=FatherID, FatherID="0", MotherID="0")

# pedfile <- pedfile %>% mutate(newMotherID = if_else(((MotherID == "0") & (FatherID != "0")),                                                     paste(FamilyID,"1", sep = "-"), #                                                     MotherID))

ped1 <- pedigree(
          id=pedfile$SampleID,
          dadid=pedfile$newFatherID,
          momid=pedfile$newMotherID,
          sex=pedfile$Sex,
          affected=pedfile$Affected,
          famid=pedfile$FamilyID,
          missid = "0")

