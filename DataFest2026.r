library(tidyverse)

library(dplyr)

encounters%>%
  group_by(PatientDurableKey) %>%
  summarise(n_visits = n()) %>%
  count(n_visits)

mychart_users <- patients[
  patients$MyChartStatus == "Activated" & 
    !is.na(patients$PatientBirthYearBin),]



# Count gender
gender_counts <- table(mychart_users$SexAssignedAtBirth)

# Filter "Unspecific"
gender_counts <- gender_counts[names(gender_counts) != "*Unspecified"]


# Convert to dataframe
gender_df <- as.data.frame(gender_counts)
colnames(gender_df) <- c("Gender", "Count")

# Pie chart
pie(gender_df$Count,
    labels = paste(gender_df$Gender, round(100 * gender_df$Count / sum(gender_df$Count), 1), "%"),
    main = "Gender Distribution of MyChart Users")


### AGE

current_year <- 2021

# Convert birth year to numeric
mychart_users$PatientBirthYearBin <- as.numeric(as.character(mychart_users$PatientBirthYearBin))

# Create age
mychart_users$PatientBirthYearBin <- current_year - mychart_users$PatientBirthYearBin

# Remove missing or unrealistic ages
mychart_users <- mychart_users[
  !is.na(mychart_users$PatientBirthYearBin) &
    mychart_users$PatientBirthYearBin >= 0 &
    mychart_users$PatientBirthYearBin <= 110,
]

# Histogram of age
hist(
  mychart_users$PatientBirthYearBin,
  breaks = 10,
  main = "Age Distribution of MyChart Users",
  xlab = "Age"
)

# Create age groups for pie chart
mychart_users$PatientBirthYearBin <- cut(
  mychart_users$PatientBirthYearBin,
  breaks = c(0, 18, 30, 45, 60, 75, 100),
  right = FALSE
)

# Count age groups
age_counts <- table(mychart_users$PatientBirthYearBin)

# Pie chart
pie(
  age_counts,
  labels = paste(
    names(age_counts),
    round(100 * age_counts / sum(age_counts), 1),
    "%"
  ),
  main = "Age Group Distribution of MyChart Users"
)




# Calculating LOS

library(lubridate)


# Length of stay (LOS) in hours
encounters$los_hours <- as.numeric(difftime(
  encounters$DischargeInstant,
  encounters$AdmissionInstant,
  units = "hours"
))


library(dplyr)

encounters_clean <- encounters %>%
  filter(
    !is.na(los_hours),
    los_hours >= 0
  )

summary(encounters_clean$los_hours)


# Comparing LOS


library(dplyr)

# 1
patient_summary <- merged_data %>%
  filter(!is.na(los_hours), los_hours >= 0) %>%
  group_by(PatientDurableKey) %>%
  summarise(
    num_visits = n(),
    avg_los = mean(los_hours, na.rm = TRUE),
    total_los = sum(los_hours, na.rm = TRUE)
  )

final_data <- patient_summary %>%
  left_join(
    patients %>% select(DurableKey, PatientBirthYearBin, MyChartStatus),
    by = c("PatientDurableKey" = "DurableKey")
  )

# 1.5
mean(patient_summary$num_visits, na.rm = TRUE)

# 2
current_year <- 2021

final_data$PatientBirthYearBin <- as.numeric(as.character(final_data$PatientBirthYearBin))

final_data$Age <- current_year - final_data$PatientBirthYearBin

summary(final_data$Age)

final_data$AgeGroup <- cut(
  final_data$Age,
  breaks = c(0, 20, 40, 60, 80, Inf),
  labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
  right = FALSE
)

table(final_data$AgeGroup, useNA = "ifany")

library(dplyr)

age_group_summary <- final_data %>%
  filter(!is.na(AgeGroup), Age <= 112) %>%
  group_by(AgeGroup) %>%
  summarise(
    avg_visits = mean(num_visits, na.rm = TRUE),
    median_visits = median(num_visits, na.rm = TRUE),
    n_patients = n()
  )

print(age_group_summary)

barplot(
  age_group_summary$avg_visits,
  names.arg = age_group_summary$AgeGroup,
  main = "Average Visits per Patient by Age Group",
  ylab = "Average Visits"
)

## 3
encounters <- encounters %>%
  filter(PrimaryDiagnosisKey != "-1")

diagnosis_lookup <- diagnosis %>%
  select(DiagnosisKey, DiagnosisName) %>%
  distinct() %>%
  group_by(DiagnosisKey) %>%
  slice(1) %>%   # ensures one row per key
  ungroup()

enc_diag <- encounters %>%
  left_join(
    diagnosis_lookup,
    by = c("PrimaryDiagnosisKey" = "DiagnosisKey")
  )
sum(is.na(enc_diag$DiagnosisName))

top10_diagnoses <- enc_diag %>%
  filter(!is.na(DiagnosisName)) %>%
  count(DiagnosisName, sort = TRUE) %>%
  head(10)

print(top10_diagnoses)

## 4 

top_diag_data <- enc_diag %>%
  filter(DiagnosisName %in% top10_diagnoses$DiagnosisName)

patient_diag_visits <- top_diag_data %>%
  group_by(PatientDurableKey, DiagnosisName) %>%
  summarise(num_visits = n(), .groups = "drop")

final_summary <- patient_diag_visits %>%
  group_by(DiagnosisName) %>%
  summarise(
    avg_visits = mean(num_visits),
    n_patients = n()
  ) %>%
  arrange(desc(avg_visits))

print(final_summary)

# 7

library(dplyr)

current_year <- 2026

patient_lookup <- patients %>%
  mutate(
    PatientBirthYearBin = as.numeric(as.character(PatientBirthYearBin)),
    Age = current_year - PatientBirthYearBin,
    AgeGroup = cut(
      Age,
      breaks = c(0, 20, 40, 60, 80, Inf),
      labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
      right = FALSE
    ),
    MyChartActivated = MyChartStatus == "Activated"
  ) %>%
  select(DurableKey, Age, AgeGroup, MyChartStatus, MyChartActivated)

library(dplyr)

current_year <- 2021

patient_lookup <- patients %>%
  mutate(
    PatientBirthYearBin = as.numeric(as.character(PatientBirthYearBin)),
    Age = current_year - PatientBirthYearBin,
    AgeGroup = cut(
      Age,
      breaks = c(0, 20, 40, 60, 80, Inf),
      labels = c("0-19", "20-39", "40-59", "60-79", "80+"),
      right = FALSE
    ),
    MyChartActivated = MyChartStatus == "Activated"
  ) %>%
  select(DurableKey, Age, AgeGroup, MyChartStatus, MyChartActivated)

enc_diag_full <- enc_diag %>%
  left_join(
    patient_lookup,
    by = c("PatientDurableKey" = "DurableKey")
  )

enc_diag_full <- enc_diag %>%
  left_join(
    patient_lookup,
    by = c("PatientDurableKey" = "DurableKey")
  )

top_diag_data <- enc_diag_full

patient_diag_visits <- top_diag_data %>%
  filter(!is.na(AgeGroup), !is.na(MyChartActivated)) %>%
  group_by(PatientDurableKey, DiagnosisName, AgeGroup, MyChartActivated) %>%
  summarise(num_visits = n(), .groups = "drop")

diag_age_mychart_summary <- patient_diag_visits %>%
  group_by(DiagnosisName, AgeGroup, MyChartActivated) %>%
  summarise(
    avg_visits = mean(num_visits, na.rm = TRUE),
    median_visits = median(num_visits, na.rm = TRUE),
    n_patients = n(),
    .groups = "drop"
  ) %>%
  arrange(DiagnosisName, AgeGroup, MyChartActivated)

summary(diag_age_mychart_summary$n_patients)

print(diag_age_mychart_summary)