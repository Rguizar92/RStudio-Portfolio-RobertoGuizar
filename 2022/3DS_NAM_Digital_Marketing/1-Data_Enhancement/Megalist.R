###############################################################################
# Megalist - Weekly Lead Consolidation and Enrichment (2022)
#
# Purpose:
#   Replace manual weekly spreadsheet work with a scripted pipeline that:
#     1) Merges multiple lead CSVs into one standardized table
#     2) Enriches JobTitle into JobLevel and Department using a picklist
#     3) Normalizes US state names
#     4) Extracts OP or MKTCONF identifiers from any text column
#     5) Builds weekly and vendor reports ready to share
#
# Inputs (place synthetic samples in 2022/megalist/data-sample/):
#   - leads_2021_2022H1_sample.csv
#   - leads_2022H2_sample.csv
#   - picklist_sample.csv
#   - state_abbs_sample.csv
#   - op_catalog_2022_sample.csv
#
# Usage (from repo root):
#   Rscript 2022/megalist/megalist.R 20220909
#   If no date is provided, today's date is used.
#
# Outputs (to 2022/megalist/outputs/):
#   - <date>_Leads_processed_merge.csv
#   - <date>_Leads_Report.csv
#   - <date>_Leads_Report_OP.csv
#   - VendorsReportV1.csv  (optional companion table)
#
# Notes:
#   Replace only direct client or vendor names in sample data. Keep generic
#   identifiers like OP12345 or MKTCONF12345 so the logic is demonstrable.
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(reshape2)
  library(tibble)
  library(here)
})

# ---------- 0) Parameters and paths ----------
args <- commandArgs(trailingOnly = TRUE)
report_date <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")

in_leads_h1 <- here("2022","megalist","data-sample","leads_2021_2022H1_sample.csv")
in_leads_h2 <- here("2022","megalist","data-sample","leads_2022H2_sample.csv")
in_picklist <- here("2022","megalist","data-sample","picklist_sample.csv")
in_states   <- here("2022","megalist","data-sample","state_abbs_sample.csv")
in_opcat    <- here("2022","megalist","data-sample","op_catalog_2022_sample.csv")

out_dir     <- here("2022","megalist","outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Helper: null-safe text
nz_chr <- function(x) ifelse(is.na(x), "", x)

# ---------- 1) Read and align inputs ----------
db1 <- read_csv(in_leads_h1, show_col_types = FALSE)
db2 <- read_csv(in_leads_h2, show_col_types = FALSE) %>% select(-any_of("X"))

all_cols <- union(names(db1), names(db2))

pad_to <- function(df, cols) {
  missing <- setdiff(cols, names(df))
  if (length(missing)) {
    df <- bind_cols(df, as_tibble(setNames(replicate(length(missing), NA, simplify = FALSE), missing)))
  }
  df[, cols]
}

db1 <- pad_to(db1, all_cols)
db2 <- pad_to(db2, all_cols)

df <- bind_rows(db1, db2)
df$consecutive_numbers <- seq_len(nrow(df))

# ---------- 2) Job title enrichment (JobLevel, Department) ----------
Pick <- read_csv(in_picklist, show_col_types = FALSE)

df$JobTitle <- as.character(df$JobTitle)

# Build regex patterns from picklist
job_lvls <- as.character(Pick$Job.Level)
job_det  <- as.character(Pick$Job.Details)
pat_job  <- gsub("\\s*/\\s*", "|", job_det)
pat_job  <- gsub("^[1-3]\\s*\\.\\s*", "", pat_job)

puesto <- rep(NA_character_, nrow(df))
for (i in seq_along(job_lvls)) {
  if (!is.na(pat_job[i]) && nzchar(pat_job[i])) {
    idx <- grep(pat_job[i], df$JobTitle, ignore.case = TRUE)
    if (length(idx)) puesto[idx] <- job_lvls[i]
  }
}

dept     <- as.character(Pick$Department)
dept_det <- as.character(Pick$Department.Details)
pat_dep  <- gsub("\\s*/\\s*", "|", dept_det)
pat_dep  <- gsub("^\\s+|\\s+$", "", pat_dep)

categoria <- rep(NA_character_, nrow(df))
for (i in seq_along(dept)) {
  if (!is.na(pat_dep[i]) && nzchar(pat_dep[i])) {
    idx <- grep(pat_dep[i], df$JobTitle, ignore.case = TRUE)
    if (length(idx)) categoria[idx] <- dept[i]
  }
}

if (!"JobLevel"   %in% names(df)) df$JobLevel   <- NA_character_
if (!"Department" %in% names(df)) df$Department <- NA_character_

df$JobLevel   <- ifelse(is.na(df$JobLevel),   puesto,   df$JobLevel)
df$Department <- ifelse(is.na(df$Department), categoria, df$Department)

# ---------- 3) State normalization ----------
states <- read_csv(in_states, show_col_types = FALSE)
names(states)[1] <- "AccountState"

ds0 <- df %>% left_join(states, by = "AccountState")
if ("STATE.NAME" %in% names(ds0)) {
  idx <- which(!is.na(ds0$STATE.NAME))
  ds0$AccountState[idx] <- ds0$STATE.NAME[idx]
  ds0$STATE.NAME <- NULL
}
ds0 <- ds0[order(ds0$consecutive_numbers), ]

# ---------- 4) OP and MKTCONF extraction, then merge with catalog ----------
op_pat <- "(OP\\d{5}|MKTCONF\\d{5})"

extract_first_match <- function(row_vec) {
  # row_vec is character vector for one row
  hits <- grep(op_pat, row_vec, value = TRUE)
  if (length(hits)) str_extract(hits[1], op_pat) else NA_character_
}

# Make a character-only copy for scanning
ds_chr <- as.data.frame(lapply(ds0, function(col) as.character(col)))
ds0$NewMCC <- apply(ds_chr, 1, extract_first_match)

opcat <- read_csv(in_opcat, show_col_types = FALSE)
names(opcat)[1] <- "NewMCC"
dbmerge <- ds0 %>% left_join(opcat, by = "NewMCC")

# ---------- 5) Weekly report (pivot by Week) ----------
# If "Week" column is absent, attempt to derive or set NA to avoid errors
if (!"Week" %in% names(dbmerge)) dbmerge$Week <- NA

# Count occurrences per NewMCC x Campaign.Name x Year x Week
Report <- dcast(
  dbmerge,
  NewMCC + Campaign.Name + Year ~ Week,
  value.var = "NewMCC",
  fun.aggregate = length
)

# TotalOP as row sum across week columns (exclude id columns)
if (ncol(Report) > 3) {
  week_cols <- setdiff(names(Report), c("NewMCC","Campaign.Name","Year"))
  Report$TotalOP <- rowSums(Report[, week_cols], na.rm = TRUE)
} else {
  Report$TotalOP <- 0
}

Report <- Report[order(Report$TotalOP, decreasing = TRUE), ]

# ---------- 6) Vendor analysis (corporate vs personal, source= parsing) ----------
is_personal <- grepl("gmail|hotmail|yahoo", tolower(nz_chr(dbmerge$Email)))
dbmerge$is_corporate <- ifelse(is_personal, "Personal", "Corporate")

src_raw <- str_extract(nz_chr(dbmerge$Traffic.Parameters), "source=[^&]+")
dbmerge$Vendors <- gsub("^source=", "", ifelse(is.na(src_raw), "Other", src_raw))

Email_dcast <- dcast(
  dbmerge,
  Vendors ~ is_corporate,
  value.var = "is_corporate",
  fun.aggregate = length
)

if (!"Personal"  %in% names(Email_dcast)) Email_dcast$Personal  <- 0L
if (!"Corporate" %in% names(Email_dcast)) Email_dcast$Corporate <- 0L
Email_dcast$Total <- Email_dcast$Personal + Email_dcast$Corporate

# Optional: join JobLevel counts by vendor, if present
if ("Job.Level" %in% names(dbmerge)) {
  Job_dcast <- dcast(
    dbmerge,
    Vendors ~ Job.Level,
    value.var = "Job.Level",
    fun.aggregate = length
  )
  Report_vendors <- cbind(Email_dcast, Job_dcast[ , setdiff(names(Job_dcast), "Vendors"), drop = FALSE])
} else {
  Report_vendors <- Email_dcast
}

# ---------- 7) OP-centric report with email split ----------
Email_dcast1 <- dcast(
  dbmerge,
  NewMCC ~ is_corporate,
  value.var = "is_corporate",
  fun.aggregate = length
)

if (!"Personal"  %in% names(Email_dcast1)) Email_dcast1$Personal  <- 0L
if (!"Corporate" %in% names(Email_dcast1)) Email_dcast1$Corporate <- 0L

Report_op <- dcast(
  dbmerge,
  NewMCC + Campaign.Name + Brand + Digital.Type + Program.Type + Vendors ~ Week,
  value.var = "NewMCC",
  fun.aggregate = length
)

# Total per OP row
week_cols_op <- setdiff(names(Report_op), c("NewMCC","Campaign.Name","Brand","Digital.Type","Program.Type","Vendors"))
Report_op$TotalOP <- if (length(week_cols_op)) rowSums(Report_op[, week_cols_op], na.rm = TRUE) else 0

FinalReport <- Email_dcast1 %>%
  left_join(Report_op, by = "NewMCC") %>%
  arrange(desc(TotalOP))

# ---------- 8) Write artifacts ----------
write_csv(dbmerge, file.path(out_dir, paste0(report_date, "_Leads_processed_merge.csv")))
write_csv(Report,  file.path(out_dir, paste0(report_date, "_Leads_Report.csv")))
write_csv(FinalReport, file.path(out_dir, paste0(report_date, "_Leads_Report_OP.csv")))
write_csv(Report_vendors, file.path(out_dir, "VendorsReportV1.csv"))

message("Done. Outputs written to: ", out_dir)
