# =============================================================================
# Project: Connected2Fiber Analytics - Bundles (Closed-Won labeling)
# Author: Roberto Guízar
# Purpose: Label Closed Won deals by presence of predefined product bundles.
#          Bundle codes:
#            B1  = bundle1 partial   | B1C = bundle1 complete
#            B2  = bundle2 partial   | B2C = bundle2 complete
#            B1- B2, B1C-B2, B1-B2C, B1C-B2C = combinations
#            NB  = no bundle items present
# Inputs (bring your own data):
#   CSV with at least: Company.ID, Deal.ID, Deal.Stage, Products (semicolon-delimited)
# Expected Outputs (local-only, user-driven): optional CSV of labeled deals.
# How to run:
#   1) Replace `input_path` with your local CSV path.
#   2) Optionally set bundle1_items / bundle2_items (vectors of product names).
#   3) Source the script.
# Notes: This repository is data-free. Do not commit CSVs or outputs.
# =============================================================================

# --- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(tidyr)
})

# --- Parameters -------------------------------------------------------------
input_path <- "your_local_path/hubspot-export-summary.csv"  # <-- replace locally
product_sep <- ";"   # separator inside Products

# Define your bundle contents here (edit to match your catalog).
# If you prefer loading from a text file (one product per line), you can read it
# locally and subset, but keep files out of the repo.
bundle1_items <- c("ProdA","ProdB","ProdC","ProdD","ProdE","ProdF","ProdG")
bundle2_items <- c("ProdH","ProdI","ProdJ","ProdG")

# --- Load & basic clean -----------------------------------------------------
options(stringsAsFactors = FALSE)
db <- read.csv(input_path)

# Remove missing Company.ID and duplicate deals
db <- db[db$Company.ID != "(No value)", ]
db$Company.ID <- as.factor(as.character(db$Company.ID))
db <- db[!duplicated(db$Deal.ID), ]

# Drop non-sales / post-sale / early qualification stages
drop_stages <- c(
  "Planned","Customer Engagement","Customer Welcome Email","Data Ingestion & Implementation",
  "Internal Engagement","Internal Sales Transition","Training & Production Launch","Project Completed",
  "Closed Lost","Contract Review","Qualified","Identified","Process Confirmed","Solution Validation"
)
db <- db[!(db$Deal.Stage %in% drop_stages), ]

# Keep Closed Won only (per original logic)
db <- dplyr::filter(db, Deal.Stage == "Closed Won")

# --- Helpers ----------------------------------------------------------------
split_products <- function(x, sep = product_sep) {
  if (is.na(x) || !nzchar(x)) return(character(0))
  # split on ; with optional surrounding spaces
  parts <- unlist(strsplit(x, "\\s*;\\s*"))
  str_trim(parts[nzchar(parts)])
}

bundle_label_for <- function(products_chr, b1, b2) {
  # products_chr: character vector of products in the deal
  in_b1 <- intersect(products_chr, b1)
  in_b2 <- intersect(products_chr, b2)
  
  if (length(in_b1) == 0 && length(in_b2) == 0) return("NB")
  
  code_b1 <- NULL
  code_b2 <- NULL
  if (length(in_b1) > 0) {
    code_b1 <- if (length(setdiff(b1, in_b1)) == 0) "B1C" else "B1"
  }
  if (length(in_b2) > 0) {
    code_b2 <- if (length(setdiff(b2, in_b2)) == 0) "B2C" else "B2"
  }
  
  if (!is.null(code_b1) && !is.null(code_b2)) return(paste0(code_b1, "-", code_b2))
  if (!is.null(code_b1)) return(code_b1)
  if (!is.null(code_b2)) return(code_b2)
  "NB"
}

# --- Label deals ------------------------------------------------------------
labels <- vector("character", nrow(db))
for (i in seq_len(nrow(db))) {
  p <- split_products(db$Products[i], product_sep)
  labels[i] <- bundle_label_for(p, bundle1_items, bundle2_items)
}
db$bundle <- labels

# --- Results (safe previews) ------------------------------------------------
# dim(db)
# table(db$bundle)
# head(db[, c("Company.ID","Deal.ID","bundle")])

# --- Optional export (local-only) -------------------------------------------
# Example output (uncomment to write locally):
# write.csv(db, "your_local_output/bundles_labeled_deals.csv", row.names = FALSE)
