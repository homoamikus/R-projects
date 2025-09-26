install.packages("tidyr")
install.packages("dplyr") 

library(dplyr)
library(tidyr)

# --- setup --------------------------------------------------------------------
set.seed(42)
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# --- study parameters ---------------------------------------------------------
study_id <- "MIG666"
countries <- c("Cambodia", "Brazil", "Colombia", "Curacao",
               "Thailand", "Poland", "Portugal", "United States")
sites_per_country <- c(Cambodia=2, Brazil=3, Colombia=2, Curacao=1,
                       Thailand=2, Poland=2, Portugal=2, "United States"=4)

# --- site generation (unique IDs, no duplicates) ------------------------------
site_start_id <- 666
n_sites <- sum(sites_per_country)

site_df <- tibble(
  SITEID  = as.character(seq(site_start_id, length.out = n_sites)),
  COUNTRY = rep(names(sites_per_country), times = sites_per_country)
)

# --- trial summary (TS) -------------------------------------------------------
ts_df <- tibble(
  STUDYID = study_id,
  DOMAIN  = "TS",
  TSSEQ   = 1:6,
  TSPARMCD = c("TITLE", "PHASE", "TRT", "PLANSUB", "CNTINV", "COUNTRY"),
  TSPARM   = c("Study Title", "Study Phase", "Investigational Treatment",
               "Planned Number of Subjects", "Planned Number of Sites",
               "Participating Countries"),
  TSVAL    = c("A Phase 1 Randomized Study of a Migraine Slaying Chewing Gum",
               "Phase 1", "Gum MIG666", "50",
               nrow(site_df),
               paste(countries, collapse = ", "))
)

# --- arms and elements --------------------------------------------------------
arms <- tibble(
  ARMCD = c("PLACEBO", "MIGL", "MIGM", "MIGH"),
  ARM   = c("Placebo", "Low Dose Gum", "Medium Dose Gum", "High Dose Gum")
)

trial_elements <- tibble(
  ETCD  = c("SCREENING", "TREATMENT", "FOLLOWUP"),
  EPOCH = c("Screening", "Treatment", "Follow-up")
)

# TA: arms × elements
ta_df <- arms %>%
  tidyr::crossing(trial_elements) %>%
  group_by(ARMCD) %>%
  mutate(
    STUDYID = study_id,
    DOMAIN  = "TA",
    TAETORD = row_number()
  ) %>%
  ungroup()

# TE
te_df <- tibble(
  STUDYID = study_id,
  DOMAIN  = "TE",
  ETCD    = c("SCREENING", "TREATMENT", "FOLLOWUP"),
  ELEMENT = c("Screening", "Treatment", "Follow-up"),
  TESTRL  = c(0, 1, 15)
)

# TV (visits)
visits <- tibble(
  VISITNUM = 1:10,
  VISIT = c("Screening", "Treatment Visit 1", "Treatment Visit 2", "Treatment Visit 3",
            "Treatment Visit 4", "End of Treatment",
            "Follow-up Visit 1", "Follow-up Visit 2", "Follow-up Visit 3",
            "End of Study"),
  TVSTRL = c(0, 1, 3, 5, 7, 8, 14, 21, 28, 35)
)

tv_df <- visits %>%
  mutate(STUDYID = study_id, DOMAIN = "TV")

# --- DM (subjects) ------------------------------------------------------------
n_subjects <- 50
races <- c("WHITE", "BLACK OR AFRICAN AMERICAN", "ASIAN", "OTHER")

n_arms <- nrow(arms)
base_per_arm <- n_subjects %/% n_arms
remainder <- n_subjects %% n_arms

# Balance subjects across arms (+ remainder random)
subjects_base <- arms %>%
  slice(rep(1:n_arms, each = base_per_arm)) %>%
  bind_rows(slice_sample(arms, n = remainder, replace = FALSE)) %>%
  mutate(SUBJID = sprintf("SUBJ%03d", 1:n_subjects))

# Site and demographics
country_lookup <- setNames(site_df$COUNTRY, site_df$SITEID)

dm_df <- subjects_base %>%
  mutate(
    SITEID  = sample(site_df$SITEID, n(), replace = TRUE),
    COUNTRY = unname(country_lookup[SITEID]),
    AGE     = sample(18:65, n(), replace = TRUE),
    SEX     = sample(c("M","F"), n(), replace = TRUE),
    RACE    = sample(races, n(), replace = TRUE)
  ) %>%
  mutate(
    STUDYID = study_id,
    DOMAIN  = "DM",
    USUBJID = paste0(SITEID, "-", SUBJID)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, SUBJID, SITEID, COUNTRY, AGE, SEX, RACE, ARMCD, ARM)

# --- AE (adverse events) ------------------------------------------------------
set.seed(42)
ae_terms     <- c("Headache","Nausea","Dizziness","Fatigue","Insomnia","Anxiety",
                  "Back pain","Nasopharyngitis","Photophobia","Phonophobia")
ae_severity  <- c("Mild","Moderate","Severe")
ae_outcome   <- c("Recovered/Resolved","Recovering/Resolving","Not Recovered","Unknown")
aerel_options<- c("Related","Not Related","Unknown")

study_start <- as.Date("2025-01-01")

ae_df <- dm_df %>%
  mutate(
    n_ae = sample(0:3, n(), replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1))
  ) %>%
  filter(n_ae > 0) %>%
  uncount(weights = n_ae, .id = "AESEQ") %>%
  mutate(
    STUDYID = STUDYID,
    DOMAIN  = "AE",
    AETERM  = sample(ae_terms, n(), replace = TRUE),
    AEDECOD = toupper(AETERM),
    AESTDTC = format(study_start + sample(1:35, n(), replace = TRUE), "%Y-%m-%d"),
    AEENDTC = format(as.Date(AESTDTC) + sample(1:10, n(), replace = TRUE), "%Y-%m-%d"),
    AESEV   = sample(ae_severity, n(), replace = TRUE, prob = c(0.6, 0.3, 0.1)),
    AEOUT   = sample(ae_outcome,  n(), replace = TRUE, prob = c(0.7, 0.2, 0.05, 0.05)),
    AEREL   = sample(aerel_options, n(), replace = TRUE, prob = c(0.5, 0.4, 0.1)),
    AESER   = ifelse(AESEV == "Severe" & runif(n()) < 0.5, "Y", "")
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AESTDTC, AEENDTC, AESEV, AEOUT, AEREL, AESER)

# --- EX (exposure) ------------------------------------------------------------
ex_map <- tibble(
  ARMCD   = c("PLACEBO","MIGL","MIGM","MIGH"),
  EXTRT   = c("Placebo Gum","Low Dose Gum","Medium Dose Gum","High Dose Gum"),
  EXDOSE  = c(0, 50, 100, 150),
  EXDOSU  = "mg",
  EXDOSFRM= "Gum",
  EXDOSFRQ= "QD",
  EXROUTE = "Oral"
)

ex_df <- dm_df %>%
  left_join(ex_map, by = "ARMCD") %>%
  mutate(
    DOMAIN  = "EX",
    EXSTDTC = study_start,
    EXENDTC = study_start + 27,
    EXDUR   = as.integer(EXENDTC - EXSTDTC + 1) # 28 days
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, ARMCD, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ,
         EXROUTE, EXSTDTC, EXENDTC, EXDUR)

# --- quick overview --------------------------------------------------------------
head(site_df)
head(dm_df)
head(ae_df)
head(ex_df)
head(ts_df)
head(ta_df)
head(te_df)
head(tv_df)

setwd("/Users/liyaung/Desktop/MIG666")

# Set output directory (optional)
output_dir <- "mig666_data"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Export each dataframe as CSV
write.csv(site_df, file = file.path(output_dir, "site_df.csv"), row.names = FALSE)
write.csv(dm_df, file = file.path(output_dir, "dm_df.csv"), row.names = FALSE)
write.csv(ae_df, file = file.path(output_dir, "ae_df.csv"), row.names = FALSE)
write.csv(ex_df, file = file.path(output_dir, "ex_df.csv"), row.names = FALSE)
write.csv(ts_df, file = file.path(output_dir, "ts_df.csv"), row.names = FALSE)
write.csv(ta_df, file = file.path(output_dir, "ta_df.csv"), row.names = FALSE)
write.csv(te_df, file = file.path(output_dir, "te_df.csv"), row.names = FALSE)
write.csv(tv_df, file = file.path(output_dir, "tv_df.csv"), row.names = FALSE) 