# =============================================================================
# Project: Connected2Fiber Analytics - Market Basket (Association Rules)
# Author: Roberto Guízar
# Purpose: Discover product co-occurrence rules and generate per-company
#          product recommendations (antecedent pair -> consequent).
# Inputs (bring your own data): CSV with columns:
#   Company.ID, Deal.ID, Deal.Stage, Products (semicolon-delimited list)
# Expected Outputs (local-only, user-driven): optional CSV of recommendations.
# How to run: Open in RStudio, install packages (see repo README), replace the
#             example path below with your local file path.
# Notes: This repository is data-free. Do not commit any CSVs or outputs.
# =============================================================================

# --- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)     # separate, pivot_longer
  library(stringr)   # str_trim
  library(arules)
  library(arulesViz) # optional plots
})

# --- Parameters -------------------------------------------------------------
input_path <- "your_local_path/hubspot-export-summary.csv"  # <-- replace locally
product_sep <- ";"          # separator used inside Products
support_threshold <- 0.10   # for apriori()
confidence_threshold <- 0.50
minlen_rules <- 3           # 2 antecedents + 1 consequent
maxlen_rules <- 3
min_plot_support <- 0.10    # for optional item frequency plot

# --- Load & basic clean -----------------------------------------------------
options(stringsAsFactors = FALSE)
db <- read.csv(input_path)

# Remove empty Company.ID and duplicate deals
db <- db[db$Company.ID != "(No value)", ]
db$Company.ID <- as.factor(as.character(db$Company.ID))
db <- db[!duplicated(db$Deal.ID), ]

# Drop post-sale / implementation pipeline stages
drop_stages <- c(
  "Planned","Customer Engagement","Customer Welcome Email","Data Ingestion & Implementation",
  "Internal Engagement","Internal Sales Transition","Training & Production Launch","Project Completed"
)
db <- db[!(db$Deal.Stage %in% drop_stages), ]
# unique(db$Deal.Stage)  # optional check

# --- Split Products to long form -------------------------------------------
# Cap at 16 products; expand if your data needs more
dbproducts <- db %>%
  separate(
    col = Products,
    into = paste0("Product", 1:16),
    sep = fixed(product_sep),
    fill = "right",
    remove = FALSE
  )

# Keep Company + wide product cols; make long and clean values
dat <- dbproducts %>%
  select(Company.ID, starts_with("Product")) %>%
  pivot_longer(cols = starts_with("Product"), names_to = "slot", values_to = "value") %>%
  transmute(
    Company.ID = Company.ID,
    value = str_trim(as.character(value))
  ) %>%
  filter(!is.na(value), value != "")

# --- Transactions per Company ----------------------------------------------
# Each company = basket of unique products
baskets <- split(dat$value, dat$Company.ID)
baskets <- lapply(baskets, function(x) unique(x[!is.na(x) & nzchar(x)]))
trans <- as(baskets, "transactions")

# --- Optional: barplot of frequent items (commented for data-free repo) ----
# itemFrequencyPlot(trans, support = min_plot_support)

# --- Mine association rules -------------------------------------------------
rules <- apriori(
  trans,
  parameter = list(
    support   = support_threshold,
    confidence= confidence_threshold,
    minlen    = minlen_rules,
    maxlen    = maxlen_rules
  )
)

# Keep only rules with 2-item LHS and 1-item RHS (given our min/max length)
rules_df <- as(rules, "data.frame")               # rules, support, confidence, lift, etc.
split_rules <- strsplit(rules_df$rules, " => ")

parse_items <- function(s) {
  # turn "{A,B}" into c("A","B")
  s <- gsub("\\{|\\}", "", s)
  if (!nzchar(s)) character(0) else strsplit(s, ",\\s*")[[1]]
}

lhs_list <- lapply(split_rules, function(x) parse_items(x[1]))
rhs_list <- lapply(split_rules, function(x) parse_items(x[2]))

# Filter to LHS length==2 and RHS length==1
keep_idx <- which(vapply(lhs_list, length, 0L) == 2 & vapply(rhs_list, length, 0L) == 1)
lhs_list <- lhs_list[keep_idx]
rhs_list <- rhs_list[keep_idx]
rules_df <- rules_df[keep_idx, , drop = FALSE]

# --- Build company-level recommendations ------------------------------------
make_recs_for_company <- function(cid, items_vec) {
  # items_vec = character vector of this company's products
  has_lhs <- vapply(lhs_list, function(L) all(L %in% items_vec), logical(1))
  rhs_item <- vapply(rhs_list, `[`, character(1), 1)
  rhs_absent <- !(rhs_item %in% items_vec)
  idx <- which(has_lhs & rhs_absent)
  if (length(idx) == 0) return(NULL)
  
  data.frame(
    Company.ID     = cid,
    Product_1      = vapply(lhs_list[idx], `[`, character(1), 1),
    Product_2      = vapply(lhs_list[idx], `[`, character(1), 2),
    Recommendation = rhs_item[idx],
    support        = rules_df$support[idx],
    confidence     = rules_df$confidence[idx],
    lift           = rules_df$lift[idx],
    stringsAsFactors = FALSE
  )
}

rec_list <- lapply(names(baskets), function(cid) make_recs_for_company(cid, baskets[[cid]]))
DF_fin <- dplyr::bind_rows(rec_list)

# Remove duplicate recommendations per company
DF_fin <- DF_fin %>%
  distinct(Company.ID, Recommendation, .keep_all = TRUE)

# --- Results preview (safe) -------------------------------------------------
# head(DF_fin)
# dplyr::count(DF_fin, Recommendation, sort = TRUE)

# --- Optional export (local-only) -------------------------------------------
# Example output (uncomment to write locally):
# write.csv(DF_fin, "your_local_output/market_basket_recommendations.csv", row.names = FALSE)
