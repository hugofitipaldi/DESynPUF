# Exploring dataset
library(tidyverse)
library(rio)

# reading the datasets

clinical_data <- rio::import("data/DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv")
claims_data <- rio::import("data/DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv")
pxs_data <- rio::import("data/DE1_0_2008_to_2010_Prescription_Drug_Events_Sample_1.csv")

# Some pre-processing

# Calculating incidence

names(clinical_data)

clinical_data_diabetes <- clinical_data %>%
  filter(SP_DIABETES == 1)

claims_data_diabetes <- claims_data %>%
  filter(DESYNPUF_ID %in% clinical_data_diabetes$DESYNPUF_ID)


claims_data_diabetes %>%
  group_by(ICD9_DGNS_CD_1) %>%
  tally() %>%
  arrange(desc(n))

table(claims_data_diabetes$ICD9_DGNS_CD_1)
table(claims_data_diabetes$ICD9_DGNS_CD)

claims_data_diabetes %>%
  filter(stringr::str_detect(ICD9_DGNS_CD_1, "250"))

stringr::str_detect(claims_data$)
pxs_hiv <- pxs_data3[pxs_data3$DESYNPUF_ID %in% hiv_claims$DESYNPUF_ID,]
head(pxs_hiv)
table(pxs_hiv$PROD_SRVC_ID)

pxs_hiv %>%
  group_by(DESYNPUF_ID, PROD_SRVC_ID) %>%
  tally() %>%
  arrange(desc(n))
