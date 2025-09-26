set.seed(42)

# Study ID
study_id <- "MIG666"

# Countries participating in the study
countries <- c("Cambodia", "Brazil", "Colombia", "Curacao", "Thailand", "Poland", "Portugal", "United States")

# Number of sites per country
sites_per_country <- c(
  Cambodia = 2, Brazil = 3, Colombia = 2, Curacao = 1,
  Thailand = 2, Poland = 2, Portugal = 2, "United States" = 4
)

# Generate site list
site_list <- data.frame(SITEID = character(), COUNTRY = character(), stringsAsFactors = FALSE)
site_counter <- 666

for (country in countries) {
  for (i in 1:sites_per_country[country]) {
    site_list <- rbind(site_list, data.frame(SITEID = as.character(site_counter), COUNTRY = country, stringsAsFactors = FALSE))
    site_counter <- site_counter + 1
  }
}

print(site_list)

# Trial summary parameters in long format
ts_data <- data.frame(
  TSPARMCD = c('TITLE', 'PHASE', 'TRT', 'PLANSUB', 'CNTINV', 'COUNTRY'),
  TSPARM = c('Study Title', 'Study Phase', 'Investigational Treatment', 'Planned Number of Subjects', 
             'Planned Number of Sites', 'Participating Countries'),
  TSVAL = c(
    'A Phase 1 Randomized Study of a Migraine Slaying Chewing Gum',
    'Phase 1',
    'Gum MIG666',
    '50',
    as.character(length(site_list$SITEID)),  # Number of sites
    paste(countries, collapse = ', ')  # List of participating countries
  ),
  stringsAsFactors = FALSE
)

# Add STUDYID, DOMAIN, and TSSEQ columns
ts_data$STUDYID <- 'MIG666'
ts_data$DOMAIN <- 'TS'
ts_data$TSSEQ <- 1:nrow(ts_data)

# Reorder columns to match CDISC SDTM standard
ts_data <- ts_data[, c('STUDYID', 'DOMAIN', 'TSSEQ', 'TSPARMCD', 'TSPARM', 'TSVAL')]

# Show the resulting TS domain dataframe
print(ts_data)

# Trial Arms data
arms <- data.frame(
  ARMCD = c('PLACEBO', 'MIGL', 'MIGM', 'MIGH'),
  ARM = c('Placebo', 'Low Dose Gum', 'Medium Dose Gum', 'High Dose Gum'),
  stringsAsFactors = FALSE
)

# Trial Elements data
trial_elements <- data.frame(
  ETCD = c('SCREENING', 'TREATMENT', 'FOLLOWUP'),
  EPOCH = c('Screening', 'Treatment', 'Follow-up'),
  stringsAsFactors = FALSE
)

# Build TA rows by combining arms and elements
ta_rows <- list()

for (i in 1:nrow(arms)) {
  for (j in 1:nrow(trial_elements)) {
    ta_rows[[length(ta_rows) + 1]] <- data.frame(
      STUDYID = 'MIG666',
      DOMAIN = 'TA',
      ARMCD = arms$ARMCD[i],
      ARM = arms$ARM[i],
      TAETORD = j,  # Order of the trial element
      ETCD = trial_elements$ETCD[j],
      EPOCH = trial_elements$EPOCH[j],
      stringsAsFactors = FALSE
    )
  }
}

# Combine the list of data frames into one data frame
ta_df <- do.call(rbind, ta_rows)

# Show the resulting TA domain dataframe
head(ta_df)

# Trial Visits data
visits <- data.frame(
  VISITNUM = 1:10,
  VISIT = c('Screening', 'Treatment Visit 1', 'Treatment Visit 2', 'Treatment Visit 3', 
            'Treatment Visit 4', 'End of Treatment', 'Follow-up Visit 1', 
            'Follow-up Visit 2', 'Follow-up Visit 3', 'End of Study'),
  TVSTRL = c(0, 1, 3, 5, 7, 8, 14, 21, 28, 35),
  stringsAsFactors = FALSE
)

# Add STUDYID and DOMAIN columns
visits$STUDYID <- 'MIG666'
visits$DOMAIN <- 'TV'

# Reorder columns to match the required format
tv_df <- visits[, c('STUDYID', 'DOMAIN', 'VISITNUM', 'VISIT', 'TVSTRL')]

# Show the resulting TV domain dataframe
head(tv_df)

# Number of subjects
n_subjects <- 50

# Subjects per arm (equally distributed)
subjects_per_arm <- n_subjects %/% nrow(arms)

# Initialize subjects list
subjects <- list()
subject_counter <- 1

# Generate subjects for each arm
for (i in 1:nrow(arms)) {
  for (j in 1:subjects_per_arm) {
    # Randomly select a site
    site_row <- site_list[sample(nrow(site_list), 1), ]
    
    # Create a unique subject ID
    subject_id <- paste0(site_row$SITEID, "-SUBJ", sprintf("%03d", subject_counter))
    
    # Append subject data to the list
    subjects[[length(subjects) + 1]] <- data.frame(
      STUDYID = 'MIG666',
      DOMAIN = 'DM',
      USUBJID = subject_id,
      SUBJID = sprintf("SUBJ%03d", subject_counter),
      SITEID = site_row$SITEID,
      COUNTRY = site_row$COUNTRY,
      AGE = sample(18:64, 1),  # Random age between 18 and 64
      SEX = sample(c('M', 'F'), 1),  # Random sex
      RACE = sample(c('WHITE', 'BLACK OR AFRICAN AMERICAN', 'ASIAN', 'OTHER'), 1),  # Random race
      ARMCD = arms$ARMCD[i],
      ARM = arms$ARM[i],
      stringsAsFactors = FALSE
    )
    
    subject_counter <- subject_counter + 1
  }
}

# Handle the remaining subjects if n_subjects is not perfectly divisible by the number of arms
remaining <- n_subjects - subjects_per_arm * nrow(arms)
for (i in 1:remaining) {
  # Randomly choose an arm for remaining subjects
  arm <- arms[sample(nrow(arms), 1), ]
  
  # Randomly select a site
  site_row <- site_list[sample(nrow(site_list), 1), ]
  
  # Create a unique subject ID
  subject_id <- paste0(site_row$SITEID, "-SUBJ", sprintf("%03d", subject_counter))
  
  # Append subject data to the list
  subjects[[length(subjects) + 1]] <- data.frame(
    STUDYID = 'MIG666',
    DOMAIN = 'DM',
    USUBJID = subject_id,
    SUBJID = sprintf("SUBJ%03d", subject_counter),
    SITEID = site_row$SITEID,
    COUNTRY = site_row$COUNTRY,
    AGE = sample(18:64, 1),  # Random age between 18 and 64
    SEX = sample(c('M', 'F'), 1),  # Random sex
    RACE = sample(c('WHITE', 'BLACK OR AFRICAN AMERICAN', 'ASIAN', 'OTHER'), 1),  # Random race
    ARMCD = arm$ARMCD,
    ARM = arm$ARM,
    stringsAsFactors = FALSE
  )
  
  subject_counter <- subject_counter + 1
}

# Convert subjects list into a data frame
dm_df <- do.call(rbind, subjects)

# Display the first few rows of the resulting data frame
head(dm_df)

# Adverse Event terms
ae_terms <- c('Headache', 'Nausea', 'Dizziness', 'Fatigue', 
              'Insomnia', 'Anxiety', 'Back pain', 'Nasopharyngitis', 
              'Photophobia', 'Phonophobia')

# Adverse Event severity
ae_severity <- c('Mild', 'Moderate', 'Severe')

# Adverse Event outcome
ae_outcome <- c('Recovered/Resolved', 'Recovering/Resolving', 'Not Recovered', 'Unknown')

# Adverse Event relation to treatment
aerel_options <- c('Related', 'Not Related', 'Unknown')

# Study start date
study_start_date <- as.Date('2025-01-01')

# Initialize AE records list
ae_records <- list()
ae_seq_counter <- 1

# Loop through subjects in dm_df
for (i in 1:nrow(dm_df)) {
  subj <- dm_df[i, ]
  
  # Randomly decide how many AEs the subject will have (0, 1, 2, or 3)
  n_ae <- sample(c(0, 1, 2, 3), 1, prob = c(0.3, 0.4, 0.2, 0.1))
  
  # Loop through the number of AEs
  for (j in 1:n_ae) {
    aeterm <- sample(ae_terms, 1)  # Random AE term
    aedecod <- toupper(aeterm)  # AE term in uppercase
    
    # Random start date offset and duration
    start_offset <- sample(1:35, 1)
    duration <- sample(1:10, 1)
    aestdtc <- study_start_date + start_offset
    aeendtc <- aestdtc + duration
    
    # Random severity, outcome, and relation to treatment
    severity <- sample(ae_severity, 1, prob = c(0.6, 0.3, 0.1))
    outcome <- sample(ae_outcome, 1, prob = c(0.7, 0.2, 0.05, 0.05))
    aerel <- sample(aerel_options, 1, prob = c(0.5, 0.4, 0.1))
    
    # Determine if the AE is serious
    serious <- ifelse(severity == 'Severe' && runif(1) < 0.5, 'Y', '')
    
    # Append AE record to the list
    ae_records[[length(ae_records) + 1]] <- data.frame(
      STUDYID = subj$STUDYID,
      DOMAIN = 'AE',
      USUBJID = subj$USUBJID,
      AESEQ = ae_seq_counter,
      AETERM = aeterm,
      AEDECOD = aedecod,
      AESTDTC = format(aestdtc, '%Y-%m-%d'),
      AEENDTC = format(aeendtc, '%Y-%m-%d'),
      AESEV = severity,
      AEOUT = outcome,
      AEREL = aerel,
      AESER = serious,
      stringsAsFactors = FALSE
    )
    
    ae_seq_counter <- ae_seq_counter + 1
  }
}

# Combine the list of AE records into a data frame
ae_df <- do.call(rbind, ae_records)

# Display the first 10 rows of the resulting AE data frame
head(ae_df, 10)

# Map ARM to treatment details
treatment_map <- list(
  'Placebo' = list(EXTRT = 'Placebo Gum', EXDOSE = 0, EXDOSU = 'mg', EXDOSFRM = 'Gum', EXDOSFRQ = 'QD', EXROUTE = 'Oral', ARMCD = 'MIGP'),
  'Low Dose Gum' = list(EXTRT = 'Low Dose Gum', EXDOSE = 50, EXDOSU = 'mg', EXDOSFRM = 'Gum', EXDOSFRQ = 'QD', EXROUTE = 'Oral', ARMCD = 'MIGL'),
  'Medium Dose Gum' = list(EXTRT = 'Medium Dose Gum', EXDOSE = 100, EXDOSU = 'mg', EXDOSFRM = 'Gum', EXDOSFRQ = 'QD', EXROUTE = 'Oral', ARMCD = 'MIGM'),
  'High Dose Gum' = list(EXTRT = 'High Dose Gum', EXDOSE = 150, EXDOSU = 'mg', EXDOSFRM = 'Gum', EXDOSFRQ = 'QD', EXROUTE = 'Oral', ARMCD = 'MIGH')
)

# Study start date and treatment duration
study_start_date <- as.Date('2025-01-01')
treatment_duration_days <- 28

# Initialize EX records list
ex_records <- list()

# Loop through each subject in the dm_df data frame
for (i in 1:nrow(dm_df)) {
  subj <- dm_df[i, ]
  
  # Get treatment details from the treatment_map based on the arm
  arm <- subj$ARM
  details <- treatment_map[[arm]]
  
  # Treatment start and end dates
  exstdtc <- study_start_date
  exendtc <- exstdtc + treatment_duration_days - 1  # inclusive
  
  # Create EX record
  ex_records[[length(ex_records) + 1]] <- data.frame(
    STUDYID = subj$STUDYID,
    DOMAIN = 'EX',
    USUBJID = subj$USUBJID,
    ARMCD = details$ARMCD,
    EXTRT = details$EXTRT,
    EXDOSE = details$EXDOSE,
    EXDOSU = details$EXDOSU,
    EXDOSFRM = details$EXDOSFRM,
    EXDOSFRQ = details$EXDOSFRQ,
    EXROUTE = details$EXROUTE,
    EXSTDTC = format(exstdtc, '%Y-%m-%d'),
    EXENDTC = format(exendtc, '%Y-%m-%d'),
    EXDUR = treatment_duration_days,
    stringsAsFactors = FALSE
  )
}

# Combine the list of EX records into a data frame
ex_df <- do.call(rbind, ex_records)

# Display the first few rows of the resulting EX data frame
head(ex_df)

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)

# 1. Create AE and DM Plots (same as before)

# AE Severity Count
ae_severity_plot <- ggplot(ae_df, aes(x = AESEV)) +
  geom_bar(fill = 'steelblue', color = 'black') +
  labs(title = "Adverse Events by Severity", x = "Severity", y = "Count") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# AE Term Count (grouped by ARM)
ae_term_plot <- ggplot(ae_df, aes(x = reorder(AETERM, -table(AETERM)[AETERM]))) +
  geom_bar(fill = 'lightcoral', color = 'black') +
  labs(title = "Adverse Events by Term", x = "Adverse Event Term", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

# AE by Treatment Arm
ae_arm_plot <- ggplot(ae_df, aes(x = ARM)) +
  geom_bar(fill = 'darkorange', color = 'black') +
  labs(title = "Adverse Events by Treatment Arm", x = "Treatment Arm", y = "Count") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)

# DM Sex Distribution (Pie Chart)
dm_sex_plot <- ggplot(dm_df, aes(x = "", fill = SEX)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Sex Distribution in Study") +
  theme_void() +
  scale_fill_manual(values = c("lightblue", "lightpink")) +
  theme(legend.position = "bottom")

# DM Age Distribution (Histogram)
dm_age_plot <- ggplot(dm_df, aes(x = AGE)) +
  geom_histogram(binwidth = 2, fill = 'seagreen', color = 'black', alpha = 0.7) +
  labs(title = "Age Distribution of Participants", x = "Age", y = "Count") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(18, 64, by = 5)) +
  scale_y_continuous(labels = scales::comma)

# 2. Save Plots to a PDF Dashboard

# Open PDF device to save the plots
pdf("study_dashboard.pdf", width = 8.5, height = 11)

# Arrange the plots in a grid
grid.arrange(
  ae_severity_plot, ae_term_plot, ae_arm_plot,
  dm_sex_plot, dm_age_plot,
  ncol = 2, nrow = 3,  # Two columns and three rows layout
  top = "Study Dashboard - AE and DM Analysis"
)

# Close the PDF device
dev.off()

# Print a message
cat("Dashboard has been saved as 'study_dashboard.pdf'.\n")