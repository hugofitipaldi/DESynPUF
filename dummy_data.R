# A example with Alzheimer

library(tidyverse)
# Importing
claims_df <- rio::import(“our_claims_df.csv”)
ICD_10 <- rio::import(“ICD_10_df.csv”)
ICD_conversion <- rio::import(“ICD_10_conversion.csv”)

#Finding which codes that are in our dataset that have changed over time
changed_icd10 <- claims_df[claims_df$ICD_10 %in% ICD_conversion$`Current code assignament`,]

# Pre-processing
changed_icd10$date <- as.Date(changed_icd10$date, fromat = “’%Y-%m-%d”)
changed_icd10$year <- lubridate::year(changed_icd10$date)

changed_icd10 <- merge(changed_icd10, ICD_conversion, by.x = c(“ICD_10”, “year”),
                       by.y =  c(“Previous code(s) Assignament”, “Effective”))

length(changed_icd10$ ICD_10 == changed_icd10$ `Current code assignament`) #diagnosing the changes

claims_df <- claims_df[!(claims_df$ICD_10 %in% ICD_conversion$`Current code assignament`),]
claims_df$year <- lubridate::year(claims_df $date)
claims_df$`Current code assignament` <- claims_df$ICD_10
claims_df <- rbind(claims_df, changed_icd10)

# Merging with description of disease
claims_df <- merge(claims_df, ICD_10, by.x = “Current code assignament”, by.y =  “ICD-10”)

# Filtering HIV
claims_hiv <- claims_df %>%
  filter(`Current code assignament` == “B20”)

claims_df_comorbidities <- claims_df %>%
  filter(patient_id %in% claims_hiv $patient_id & `Current code assignament ` != “B20”)
select(patient_id, `Current code assignament`)

names(claims_df_comorbidities) <- c(“patient_id”, “comorbidities”)
claims_hiv$comorbidities <- NA
# Creating comorbidity variable
claims_hiv <- merge(claims_hiv, claims_df_comorbidities, by = “patient_id”)


library(survival)
coxph(Surv(time, status) ~ ., data)



# If determined codes are not available (claims dataset)
claims_hiv_pres <- claims_hiv %>%
  filter(pharmaceutical_prescriptions %in% c(HIV drugs)) %>%
  group_by(patient_id, pharmaceutical_prescriptions) %>%
  slice(1L) %>%
  arrange(desc(date)) %>%
  ungroup() %>%
  select(patient_id, date, pharmaceutical_prescriptions)


res <- wilcox.test(BULB_PRICE~ BULB_TYPE,
                   data = DATASET,
                   exact = FALSE)



res.man <- manova(cbind(Variable_1, Variable_2) ~ Groups, data)
summary(res.man)


conf_model <- glm(Treatment_group ~ age + sex _ region, family = “binomial”)

prob <- predict(conf_model,type="response")

weights <- ifelse(BMI==1,1/prob,1/(1-prob))



