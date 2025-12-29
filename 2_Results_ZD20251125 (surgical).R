# -------------------------------------------------------------------
# Payments analysis by college group
# Author: Zak Doherty
# Date: November 2025
#
# This script:
#  - Loads cleaned Medicines Australia payments data
#  - Cleans specialty / profession classifications
#  - Allocates specialties to college groups
#  - WPI-adjusts payment amounts
#  - Produces:
#      * Table E1: Summary of payments by specialty group
#      * Table E2: Proportion of fellows receiving ≥1 payment by college (paragraph 1 values from here).
#      * Figure 1: Proportion with 95% CI receiving ≥1 payment by college & speciality (RACS only). 
# -------------------------------------------------------------------

# --- LOAD PACKAGES & DATA ----------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(stringr)
  library(lubridate)
  library(knitr)
  library(scales)
  library(binom)
  library(writexl)
  library(DescTools)
  library(RColorBrewer)
  library(ggrepel)
  library(gt)
  library(readabs)
  library(patchwork)
library(stringr)
library(grid)
})


# --- LOAD DATA ---------------------------------------------------------------

payments <- read_csv("payments_cleaned_updated.csv")

# --- INDIVIDUAL DOCTOR CLEANING ----------------------------------------------

# Nuclear medicine recodes (overlapping specialties by college)
nuc_med <- payments %>%
  filter(specialty == "NUCLEAR MEDICINE") %>%
  select(name) %>%
  distinct(name) %>%
  mutate(
    speciality_new = case_when(
      name == "Chikatamarla, Venkata" ~ "RANZCR",
      name == "Emmett, Louise"        ~ "RACP_ADULT",
      name == "Lenzo, Nat"            ~ "RACP_ADULT",
      name == "Rowe, Christopher"     ~ "RACP_ADULT",
      name == "Ahluwalia, Uday"       ~ "RACP_ADULT",
      name == "Bernard, Elizabeth"    ~ "RACP_ADULT",
      name == "Dhiantravan, Nattakorn"~ "RACP_ADULT",
      name == "Groombridge, Laura K." ~ "RACP_ADULT",
      name == "Hicks, Rodney"         ~ "RACP_ADULT",
      name == "Hofman, Michael"       ~ "RACP_ADULT",
      name == "Hsiao, Edward"         ~ "RANZCR",
      name == "Kong, Grace"           ~ "RACP_ADULT",
      name == "Law, Andrew"           ~ "RANZCR",
      name == "Patterson, David"      ~ "RACP_ADULT",
      name == "Pocock, Nicholas"      ~ "RACP_ADULT",
      name == "Singh, Dalveer"        ~ "RANZCR",
      name == "Thomas, Paul Anthony"  ~ "RANZCR",
      name == "Wong, Joseph C."       ~ "RACP_ADULT",
      name == "Yung, Grace"           ~ "RACP_ADULT"
    )
  )

# Recode original specialty labels for these doctors
payments <- payments %>%
  mutate(
    specialty = case_when(
      name == "Chikatamarla, Venkata" ~ "NUCLEAR MEDICINE RADIOLOGY",
      name == "Emmett, Louise"        ~ "NUCLEAR MEDICINE RACP",
      name == "Lenzo, Nat"            ~ "NUCLEAR MEDICINE RACP",
      name == "Rowe, Christopher"     ~ "NUCLEAR MEDICINE RACP",
      name == "Ahluwalia, Uday"       ~ "NUCLEAR MEDICINE RACP",
      name == "Bernard, Elizabeth"    ~ "NUCLEAR MEDICINE RACP",
      name == "Dhiantravan, Nattakorn"~ "NUCLEAR MEDICINE RACP",
      name == "Groombridge, Laura K." ~ "NUCLEAR MEDICINE RACP",
      name == "Hicks, Rodney"         ~ "NUCLEAR MEDICINE RACP",
      name == "Hofman, Michael"       ~ "NUCLEAR MEDICINE RACP",
      name == "Hsiao, Edward"         ~ "NUCLEAR MEDICINE RADIOLOGY",
      name == "Kong, Grace"           ~ "NUCLEAR MEDICINE RACP",
      name == "Law, Andrew"           ~ "NUCLEAR MEDICINE RADIOLOGY",
      name == "Patterson, David"      ~ "NUCLEAR MEDICINE RACP",
      name == "Pocock, Nicholas"      ~ "NUCLEAR MEDICINE RACP",
      name == "Singh, Dalveer"        ~ "NUCLEAR MEDICINE RADIOLOGY",
      name == "Thomas, Paul Anthony"  ~ "NUCLEAR MEDICINE RADIOLOGY",
      name == "Wong, Joseph C."       ~ "NUCLEAR MEDICINE RACP",
      name == "Yung, Grace"           ~ "NUCLEAR MEDICINE RACP",
      TRUE                            ~ specialty
    )
  )

# --- ALLOCATION OF SPECIALTIES TO COLLEGES -----------------------------------

RACP_ADULT <- c(
  "ADDICTION MEDICINE", "CARDIOLOGY", "CLINICAL GENETICS", "CLINICAL PHARMACOLOGY",
  "ENDOCRINOLOGY", "GASTROENTEROLOGY AND HEPATOLOGY", "GENERAL MEDICINE",
  "GERIATRIC MEDICINE", "HAEMATOLOGY AND ONCOLOGY", "INFECTIOUS DISEASES",
  "IMMUNOLOGY AND ALLERGY", "MICROBIOLOGY", "NUCLEAR MEDICINE RACP",
  "NEPHROLOGY", "NEUROLOGY", "PALLIATIVE MEDICINE",
  "RESPIRATORY AND SLEEP MEDICINE", "RHEUMATOLOGY",
  "REHABILITATION MEDICINE", "SEXUAL HEALTH MEDICINE",
  "OCCUPATIONAL MEDICINE", "PUBLIC HEALTH MEDICINE"
)

RACP_PAEDS <- c(
  "GENERAL PAEDIATRICS", "PAEDIATRIC CARDIOLOGY", "PAEDIATRIC CLINICAL GENETICS",
  "PAEDIATRIC ENDOCRINOLOGY", "PAEDIATRIC GASTROENTEROLOGY AND HEPATOLOGY",
  "PAEDIATRIC HAEMATOLOGY AND ONCOLOGY", "PAEDIATRIC IMMUNOLOGY AND ALLERGY",
  "PAEDIATRIC INFECTIOUS DISEASES", "PAEDIATRIC NEPHROLOGY",
  "PAEDIATRIC NEUROLOGY", "PAEDIATRIC RESPIRATORY AND SLEEP MEDICINE",
  "PAEDIATRIC RHEUMATOLOGY", "PAEDIATRICS AND CHILD HEALTH"
)

ACEM    <- c("EMERGENCY MEDICINE")
CICM    <- c("INTENSIVE CARE MEDICINE")
ANZCA   <- c("ANAESTHESIA", "PAIN MEDICINE")

NON_MEDICAL <- c(
  "DIETETICS", "EXERCISE PHYSIOLOGY", "PARAMEDICINE",
  "PHARMACY", "PSYCHOLOGY", "RADIOGRAPHY",
  "SCIENTIST", "SOCIAL WORK", "SPEECH PATHOLOGY", "VETERINARY MEDICINE",
  "NURSING", "OPTOMETRY OR ORTHOPTICS",
  "PHYSIOTHERAPY OR OCCUPATIONAL THERAPY",
  "DENTISTRY", "PODIATRY"
)

DIT   <- c("DOCTOR-IN-TRAINING")

RANZCR <- c("NUCLEAR MEDICINE RADIOLOGY", "DIAGNOSTIC RADIOLOGY", "RADIATION ONCOLOGY")
RCPA   <- c("PATHOLOGY")
RACMA  <- c("MEDICAL ADMINISTRATION")
ACSEP  <- c("SPORT AND EXERCISE MEDICINE")
NON_CLIN <- c("NON-CLINICAL")

RACGP_OR_ACRRM <- c("GENERAL PRACTICE")

RACS <- c(
  "CARDIOTHORACIC SURGERY", "GENERAL SURGERY", "NEUROSURGERY",
  "ORAL AND MAXILLOFACIAL SURGERY", "ORTHOPAEDIC SURGERY",
  "OTOLARYNGOLOGY - HEAD AND NECK SURGERY", "PAEDIATRIC SURGERY",
  "PLASTIC SURGERY", "UROLOGY", "VASCULAR SURGERY"
)

RANZCP <- c("PSYCHIATRY")
RANZCO <- c("OPHTHALMOLOGY")
RANZCOG <- c("OBSTETRICS AND GYNAECOLOGY")
ACD <- c("DERMATOLOGY")

# Apply allocation to college groups
payments <- payments %>%
  mutate(
    SPECIALITY_GROUP = case_when(
      specialty %in% RACP_ADULT        ~ "RACP_ADULT",
      specialty %in% RACP_PAEDS        ~ "RACP_PAEDS",
      specialty %in% ACEM              ~ "ACEM",
      specialty %in% CICM              ~ "CICM",
      specialty %in% ANZCA             ~ "ANZCA",
      specialty %in% NON_MEDICAL       ~ "NON_MEDICAL",
      specialty %in% DIT               ~ "DIT",
      specialty %in% RANZCR            ~ "RANZCR",
      specialty %in% RCPA              ~ "RCPA",
      specialty %in% RACMA             ~ "RACMA",
      specialty %in% ACSEP             ~ "ACSEP",
      specialty %in% NON_CLIN          ~ "NON_CLIN",
      specialty %in% RACS              ~ "RACS",
      specialty %in% RANZCP            ~ "RANZCP",
      specialty %in% RANZCO            ~ "RANZCO",
      specialty %in% RANZCOG           ~ "RANZCOG",
      specialty %in% ACD               ~ "ACD",
      specialty %in% RACGP_OR_ACRRM    ~ "RACGP_OR_ACRRM",
      TRUE                             ~ "UNASSIGNED"
    )
  )

# --- INCORRECT PROFESSION ALLOCATION FIXES -----------------------------------

payments <- payments %>%
  mutate(
    profession = case_when(
      name == "Dunn, Anna"         & profession == "Nurses"      ~ "Medical Professions",
      name == "Shahzad, Anwar"     & profession == "Nurses"      ~ "Medical Professions",
      name == "Bailey, Christopher"& profession == "Nurses"      ~ "Medical Professions",
      name == "McMahon, Lawrence"  & profession == "Nurses"      ~ "Medical Professions",
      name == "Moore, Melissa"     & profession == "Nurses"      ~ "Medical Professions",
      name == "Wang, Ying"         & profession == "Nurses"      ~ "Medical Professions",
      name == "Nguyen, Yvonne"     & profession == "Pharmacists" ~ "Medical Professions",
      name == "Jones, Karen"      & profession == "Medical Professionals" ~ "Other",
      TRUE ~ profession
    )
  )

# --- COHORT CREATION (DOCTORS ONLY) ------------------------------------------

payments_doctor <- payments %>%
  filter(profession == "Medical Professionals") %>%   # NOTE: check this matches "Medical Professions"/"Medical Professionals"
  filter(SPECIALITY_GROUP != "NON_MEDICAL") %>%
  filter(SPECIALITY_GROUP != "UNASSIGNED") %>%
  filter(SPECIALITY_GROUP != "DIT")

# --- INFLATION ADJUSTMENT (WPI) ----------------------------------------------

# Approximate event date as mid-point of period
payments_doctor <- payments_doctor %>%
  mutate(
    start_raw = str_extract(period, "^[A-Za-z]+ \\d{4}"),
    end_raw   = str_extract(period, "[A-Za-z]+ \\d{4}$"),
    start     = dmy(paste0("01 ", start_raw)),
    end       = dmy(paste0("01 ", end_raw)),
    mid_date  = start + (end - start) / 2
  ) %>%
  select(-start_raw, -end_raw, -start, -end)

# Load WPI series (ABS)
wpi <- read_abs(series_id = "A2603609J") %>%
  select(date, value) %>%
  arrange(date)

# Map each payment to nearest WPI quarter
idx <- findInterval(payments_doctor$mid_date, wpi$date)

payments_doctor$wpi_date <- wpi$date[idx]
payments_doctor$wpi      <- wpi$value[idx]

# Use latest WPI as base
base_wpi <- max(payments_doctor$wpi, na.rm = TRUE)

cols_to_std <- c("registration_fees", "travel_costs", "fees_for_service", "total_payment")

payments_doctor <- payments_doctor %>%
  mutate(
    across(
      all_of(cols_to_std),
      ~ .x * (base_wpi / wpi),
      .names = "{.col}_wpi_std"
    )
  )

# --- TABLE E1: SUMMARY BY SPECIALTY GROUP -------------------------------------

summary_tbl <- payments_doctor %>%
  group_by(SPECIALITY_GROUP) %>%
  summarise(
    n_obs              = n(),
    n_unique_hcps      = n_distinct(name, specialty),
    n_unique_hcps_narm = n_distinct(name, specialty, na.rm = TRUE),
    n_specialties      = n_distinct(specialty, na.rm = TRUE),
    n_companies        = n_distinct(company, na.rm = TRUE),
    n_services         = n_distinct(service, na.rm = TRUE),
    n_events           = n_distinct(event, na.rm = TRUE),
    n_payments         = n_distinct(payment_to, na.rm = TRUE),
    total_payments     = sum(total_payment_wpi_std, na.rm = TRUE),
    min_total_payment  = min(total_payment_wpi_std, na.rm = TRUE),
    q1_total_payment   = unname(quantile(total_payment_wpi_std, 0.25, na.rm = TRUE)),
    median_indi_payment= median(total_payment_wpi_std, na.rm = TRUE),
    q3_total_payment   = unname(quantile(total_payment_wpi_std, 0.75, na.rm = TRUE)),
    max_total_payment  = max(total_payment_wpi_std, na.rm = TRUE),
    .groups = "drop"
  )

total_row <- payments_doctor %>%
  summarise(
    SPECIALITY_GROUP    = "TOTAL",
    n_obs               = n(),
    n_unique_hcps       = n_distinct(name, specialty),
    n_unique_hcps_narm  = n_distinct(name, specialty, na.rm = TRUE),
    n_specialties       = n_distinct(specialty, na.rm = TRUE),
    n_companies         = n_distinct(company, na.rm = TRUE),
    n_services          = n_distinct(service, na.rm = TRUE),
    n_events            = n_distinct(event, na.rm = TRUE),
    n_payments          = n_distinct(payment_to, na.rm = TRUE),
    total_payments      = sum(total_payment_wpi_std, na.rm = TRUE),
    min_total_payment   = min(total_payment_wpi_std, na.rm = TRUE),
    q1_total_payment    = unname(quantile(total_payment_wpi_std, 0.25, na.rm = TRUE)),
    median_indi_payment = median(total_payment_wpi_std, na.rm = TRUE),
    q3_total_payment    = unname(quantile(total_payment_wpi_std, 0.75, na.rm = TRUE)),
    max_total_payment   = max(total_payment_wpi_std, na.rm = TRUE)
  )

final_tbl <- bind_rows(summary_tbl, total_row) %>%
  arrange(desc(n_obs), SPECIALITY_GROUP) %>%
  gt(rowname_col = "SPECIALITY_GROUP") %>%
  fmt_number(columns = where(is.numeric), decimals = 0) %>%
  tab_header(
    title    = "Summary of Payments by Specialty Group",
    subtitle = "All amounts in AUD"
  )

# View GT table 1
final_tbl

# --- TABLE E2: PROPORTION OF FELLOWS WITH ≥1 PAYMENT --------------------------

# Load AHPRA denominators
ahpra_xlsx <- "ahpra_202324.xlsx"

normalise_up <- \(x) x |>
  str_replace_all("[\u2013\u2014]", "-") |>
  str_squish() |>
  str_to_upper()

r9 <- read_excel(
  ahpra_xlsx,
  sheet = "R9",
  skip  = 2,
  .name_repair = "universal"
) %>%
  transmute(
    PROF_UP        = normalise_up(Profession),
    Total.2023.24
  ) %>%
  filter(!is.na(PROF_UP), !is.na(Total.2023.24))

# Numerators: unique doctors with ≥1 payment
numerators <- payments_doctor %>%
  semi_join(r9, by = c("specialty" = "PROF_UP")) %>%
  summarise(
    Unique_with_payment = n_distinct(name),
    .by = specialty
  )

# Merge with denominators
base <- numerators %>%
  left_join(r9, by = c("specialty" = "PROF_UP")) %>%
  transmute(
    specialty,
    Unique_with_payment,
    Denominator_2023_24 = Total.2023.24,
    Proportion_pct      = round(100 * Unique_with_payment / Denominator_2023_24, 1)
  )

# Allocate to college groups (same logic as above)
base <- base %>%
  mutate(
    SPECIALITY_GROUP = case_when(
      specialty %in% RACP_ADULT        ~ "RACP_ADULT",
      specialty %in% RACP_PAEDS        ~ "RACP_PAEDS",
      specialty %in% ACEM              ~ "ACEM",
      specialty %in% CICM              ~ "CICM",
      specialty %in% ANZCA             ~ "ANZCA",
      specialty %in% NON_MEDICAL       ~ "NON_MEDICAL",
      specialty %in% DIT               ~ "DIT",
      specialty %in% RANZCR            ~ "RANZCR",
      specialty %in% RCPA              ~ "RCPA",
      specialty %in% RACMA             ~ "RACMA",
      specialty %in% ACSEP             ~ "ACSEP",
      specialty %in% NON_CLIN          ~ "NON_CLIN",
      specialty %in% RACS              ~ "RACS",
      specialty %in% RANZCP            ~ "RANZCP",
      specialty %in% RANZCO            ~ "RANZCO",
      specialty %in% RANZCOG           ~ "RANZCOG",
      specialty %in% ACD               ~ "ACD",
      specialty %in% RACGP_OR_ACRRM    ~ "RACGP_OR_ACRRM",
      TRUE                             ~ "UNASSIGNED"
    )
  )

# Wilson CI helper
add_wilson <- function(df) {
  ci <- binom::binom.confint(
    x       = df$unique_with_payment,
    n       = df$denom_23_24,
    methods = "wilson"
  )

  df %>%
    mutate(
      ci_lower_pct = round(100 * ci$lower, 1),
      ci_upper_pct = round(100 * ci$upper, 1),
      prop_with_ci = sprintf(
        "%.1f%% (%.1f–%.1f)",
        prop_pct, ci_lower_pct, ci_upper_pct
      )
    )
}

# College-level collapsed numerators/denominators
df_collapsed_college <- base %>%
  group_by(SPECIALITY_GROUP) %>%
  summarise(
    unique_with_payment = sum(Unique_with_payment, na.rm = TRUE),
    denom_23_24         = sum(Denominator_2023_24, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    prop_pct = 100 * unique_with_payment / denom_23_24
  ) %>%
  add_wilson()

# Total row (all colleges)
df_collapsed_college_total <- base %>%
  summarise(
    SPECIALITY_GROUP    = "TOTAL",
    unique_with_payment = sum(Unique_with_payment, na.rm = TRUE),
    denom_23_24         = sum(Denominator_2023_24, na.rm = TRUE),
    .groups             = "drop"
  ) %>%
  mutate(
    prop_pct = 100 * unique_with_payment / denom_23_24
  ) %>%
  add_wilson()

# Bind together
final_tbl_2 <- bind_rows(
  df_collapsed_college,
  df_collapsed_college_total
) %>%
  arrange(desc(prop_pct), SPECIALITY_GROUP)

#Generate table 2
table_2_view <- final_tbl_2 %>%
  arrange(desc(prop_pct), SPECIALITY_GROUP) %>%
  gt(rowname_col = "SPECIALITY_GROUP") %>%
  fmt_number(columns = where(is.numeric), decimals = 1) %>%
  tab_header(
    title    = "Proportion of fellows receiving a payment by college",
  )

# View GT table 2
table_2_view





# SURGICAL ANALYSIS ------------------------------------------------------
# 1. DATA PREP ------------------------------------------------------
college_data <- base %>%
  group_by(SPECIALITY_GROUP) %>%
  summarise(
    unique_with_payment = sum(Unique_with_payment, na.rm = TRUE),
    denom_23_24         = sum(Denominator_2023_24, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(prop_pct = 100 * (unique_with_payment / denom_23_24)) %>%
  add_wilson() %>%
  mutate(
    DISPLAY_NAME = case_when(
      SPECIALITY_GROUP == "RACP_ADULT" ~ "RACP (ADULT)",
      SPECIALITY_GROUP == "RACP_PAEDS" ~ "RACP (PAEDS)",
      SPECIALITY_GROUP == "RACGP_OR_ACRRM" ~ "GP / ACRRM",
      TRUE ~ str_to_upper(str_replace_all(SPECIALITY_GROUP, "_", " "))
    ),
    DISPLAY_NAME = reorder(DISPLAY_NAME, prop_pct),
    is_racs      = ifelse(SPECIALITY_GROUP == "RACS", "Highlight", "Normal")
  )

# Surgical Data (Right Panel) - Title Case
surgical_data <- base %>%
  filter(SPECIALITY_GROUP == "RACS") %>%
  filter(specialty != "ORAL AND MAXILLOFACIAL SURGERY") %>%
  mutate(specialty = ifelse(specialty == "OTOLARYNGOLOGY - HEAD AND NECK SURGERY", 
                            "ENT Surgery", str_to_title(specialty))) %>%
  rename(prop_pct = Proportion_pct) %>%
  mutate(
    unique_with_payment = Unique_with_payment,
    denom_23_24         = Denominator_2023_24
  ) %>%
  add_wilson() %>% 
  mutate(specialty = reorder(specialty, prop_pct))

# 2. GENERATE FIGURE ------------------------------------------------------
# Left Plot: College Overall
p_left <- ggplot(college_data, aes(x = DISPLAY_NAME, y = prop_pct, color = is_racs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), width = 0.1, linewidth = 0.6) +
  scale_color_manual(values = c("Highlight" = "#E41A1C", "Normal" = "gray60")) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Proportion with ≥1 payment (%)") +
  theme_bw(base_family = "Montserrat") +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# Main Panel: Surgical Specialties (Title Case)
p_right <- ggplot(surgical_data, aes(x = specialty, y = prop_pct)) +
  geom_point(color = "#E41A1C", size = 3.5) +
  geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                width = 0.1, linewidth = 0.7, color = "#E41A1C") +
  geom_text(
    aes(label = paste0(round(prop_pct, 1), "%")), 
    vjust = -1.8, # Increased buffer to move text further above the dot
    size = 4.2, 
    fontface = "bold", 
    family = "Montserrat"
  ) +
  coord_flip(clip = "off") + 
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(x = NULL, y = "Proportion receiving a payment (%)") +
  theme_bw(base_family = "Montserrat") +
  theme(
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 30, r = 30, b = 10, l = 10)
  )

# Define the professional blue theme color
theme_blue <- "#2E5A88"

# --- . UPDATED LEFT PLOT (P_LEFT) ---
p_left_updated <- p_left + 
  # Use the blue for the highlight point and error bar
  scale_color_manual(values = c("Highlight" = theme_blue, "Normal" = "gray70")) +
  # Add a subtle blue highlight box behind the RACS label
  annotate("rect", 
           xmin = as.numeric(college_data$DISPLAY_NAME[college_data$SPECIALITY_GROUP == "RACS"]) - 0.45, 
           xmax = as.numeric(college_data$DISPLAY_NAME[college_data$SPECIALITY_GROUP == "RACS"]) + 0.45, 
           ymin = -Inf, ymax = Inf, fill = theme_blue, alpha = 0.1) +
  labs(title = "All Medical Colleges") +
  theme(
    # Centered (hjust=0.5) and larger (size=16) title
    plot.title = element_text(face = "bold", size = 16, family = "Montserrat", hjust = 0.5),
    panel.grid.minor = element_blank()
  )

# --- 2. UPDATED RIGHT PLOT (P_RIGHT) ---
p_right_updated <- ggplot(surgical_data, aes(x = specialty, y = prop_pct)) +
  geom_point(color = theme_blue, size = 3.5) +
  geom_errorbar(aes(ymin = ci_lower_pct, ymax = ci_upper_pct), 
                width = 0.1, linewidth = 0.7, color = theme_blue) +
  geom_text(
    aes(label = paste0(round(prop_pct, 1), "%")), 
    vjust = -1.8, 
    size = 4.2, 
    fontface = "bold", 
    family = "Montserrat",
    color = "black"
  ) +
  coord_flip(clip = "off") + 
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(title = "RACS Sub-speciality Breakdown", x = NULL, y = "Proportion with ≥1 payment (%)") +
  theme_bw(base_family = "Montserrat") +
  theme(
    # Centered (hjust=0.5) and larger (size=16) title
    plot.title = element_text(face = "bold", size = 16, family = "Montserrat", hjust = 0.5),
    axis.text = element_text(size = 11, color = "black"),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    # Light blue tint for the subset zone
    panel.background = element_rect(fill = paste0(theme_blue, "08"), color = NA),
    plot.margin = margin(t = 30, r = 30, b = 10, l = 10)
  )

# --- 3. COMBINE PANELS ---
final_viz <- p_left_updated + p_right_updated + 
  plot_layout(widths = c(1, 1.8))

#View
final_viz

R.version


