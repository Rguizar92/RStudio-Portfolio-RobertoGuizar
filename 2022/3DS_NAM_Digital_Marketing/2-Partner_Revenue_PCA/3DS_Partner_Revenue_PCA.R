###############################################################################
# CPE Partner Analysis — Feature Engineering + PCA/HCPC (Visualization Only)
#
# Purpose
#   Demonstrate how I engineered partner-level features and explored structure
#   with PCA + hierarchical clustering (HCPC) to surface patterns across:
#   - Total revenue (2016–2020)
#   - Cloud revenue (2016–2020)
#   - Desktop revenue = Total - Cloud
#   - Brand mix (CATIA, ENOVIA, BIOVIA, DELMIA, EXALEAD, SIMULIA, OTHER)
#   - Size mix (Small, Medium, Large, Big)
#
# Context
#   Built during my 2022 NAM Digital Marketing internship to reduce manual
#   analysis time and generate deeper partner insights for internal dashboards.
#
# Data
#   This script is for visualization purposes only. It will look for
#   `data-sample/CPE_partners_data.csv`. If not found, it creates a small
#   in-memory demo dataset to show the full workflow end-to-end.
#
# Usage (from repo root)
#   Rscript 2022/CPE_Partner_Analysis/CPE_Partner_Analysis.R
#   # optional args:
#   #   <min_total_revenue>   (default 7000)
#
# Outputs (to console + optional CSV)
#   - Prints engineered feature tables and cluster label head()
#   - Optionally writes `outputs/CPEanalysisTopTier.csv` (commented by default)
###############################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(reshape2)    # kept for parity with original; mostly using tidyr/dplyr
  library(FactoMineR)
  library(factoextra)
  library(here)
})

# ------------------------- Parameters & paths --------------------------------
args <- commandArgs(trailingOnly = TRUE)
min_total_rev <- if (length(args) >= 1) as.numeric(args[1]) else 7000

in_file  <- here("2022","CPE_Partner_Analysis","data-sample","CPE_partners_data.csv")
out_dir  <- here("2022","CPE_Partner_Analysis","outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------- Demo data (if needed) -----------------------------
make_demo <- function() {
  # Minimal columns needed by the pipeline
  partners <- paste0("Partner_", LETTERS[1:8])
  tibble(
    Partner = partners,
    # Total revenue components (TALC/TPLC/TYLC) by year
    !!!set_names(replicate(3*5, sample(c(0, 1000:6000), 8, TRUE), simplify = FALSE),
                 c(outer(c("TALC","TPLC","TYLC"), 2016:2020, paste, sep = "_"))),
    # Cloud revenue components (X3DALC/X3DPLC/X3DYLC) by year
    !!!set_names(replicate(3*5, sample(c(0, 200:2000), 8, TRUE), simplify = FALSE),
                 c(outer(c("X3DALC","X3DPLC","X3DYLC"), 2016:2020, paste, sep = "_"))),
    # Brand yearly numbers (toy)
    !!!set_names(replicate(7*5, sample(0:100, 8, TRUE), simplify = FALSE),
                 c(outer(c("BIOVIA","CATIA","DELMIA","ENOVIA","EXALEAD","SIMULIA","OTHER"),
                         2016:2020, paste, sep = "_"))),
    # Size yearly counts (toy)
    !!!set_names(replicate(4*5, sample(0:50, 8, TRUE), simplify = FALSE),
                 c(outer(c("Small","Medium","Large","Big"), 2016:2020, paste, sep = "_")))
  )
}

db <- if (file.exists(in_file)) {
  readr::read_csv(in_file, show_col_types = FALSE)
} else {
  message("No CSV found. Using in-memory demo dataset.")
  make_demo()
}

# ------------------------- 1) Yearly totals & sums ---------------------------
# Total revenue per year
for (yr in 2016:2020) {
  db[[paste0("TotalRevenue", yr)]] <-
    rowSums(db[, c(paste0("TALC_", yr), paste0("TPLC_", yr), paste0("TYLC_", yr))],
            na.rm = TRUE)
}

# Cloud revenue per year
for (yr in 2016:2020) {
  db[[paste0("TotalCloudRevenue", yr)]] <-
    rowSums(db[, c(paste0("X3DALC_", yr), paste0("X3DPLC_", yr), paste0("X3DYLC_", yr))],
            na.rm = TRUE)
}

# Growth and sums
db$GRate_2020 <- with(db,
                      (TotalRevenue2020 - TotalRevenue2016) / pmax(TotalRevenue2016, 1)
)
db$TotalRevenueSum <- rowSums(db[paste0("TotalRevenue", 2016:2020)], na.rm = TRUE)
db$TotalCloudRevenueSum <- rowSums(db[paste0("TotalCloudRevenue", 2016:2020)], na.rm = TRUE)

# Filter by threshold (visual intent: top-tier partners)
db <- db |> filter(TotalRevenueSum > min_total_rev)

# ------------------------- 2) Feature blocks ---------------------------------
# Helper: ensure Partner exists
stopifnot("Partner" %in% names(db))

# Total revenue (wide by year)
db1_TTRev <- db |> select(Partner, starts_with("TotalRevenue"))
# Cloud revenue (wide by year)
db2_CloudRev <- db |> select(Partner, starts_with("TotalCloudRevenue"))
# Desktop revenue = Total - Cloud (align by year)
db3_DesktopRev <- db1_TTRev
db3_DesktopRev[-1] <- db1_TTRev[-1] - db2_CloudRev[-1]

# Brand mix: sum across years by brand
brand_keys <- c("BIOVIA","CATIA","DELMIA","ENOVIA","EXALEAD","SIMULIA","OTHER")
brand_cols <- names(db)[str_detect(names(db), paste0("^(", paste(brand_keys, collapse="|"), ")_\\d{4}$"))]
db4_Brand <- db |>
  select(Partner, all_of(brand_cols)) |>
  pivot_longer(-Partner, names_to = "brand_year", values_to = "value") |>
  mutate(brand = sub("_\\d{4}$", "", brand_year)) |>
  group_by(Partner, brand) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = brand, values_from = value, values_fill = 0) |>
  relocate(Partner)

db4_Brand$TotalBrandSum <- rowSums(db4_Brand[brand_keys], na.rm = TRUE)

# Size mix: sum across years by size
size_keys <- c("Small","Medium","Large","Big")
size_cols <- names(db)[str_detect(names(db), paste0("^(", paste(size_keys, collapse="|"), ")_\\d{4}$"))]
db5_Size <- db |>
  select(Partner, all_of(size_cols)) |>
  pivot_longer(-Partner, names_to = "size_year", values_to = "value") |>
  mutate(size = sub("_\\d{4}$", "", size_year)) |>
  group_by(Partner, size) |>
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = size, values_from = value, values_fill = 0) |>
  relocate(Partner)

db5_Size$TotalSizeSum <- rowSums(db5_Size[size_keys], na.rm = TRUE)

# Merge engineered features (for exporting with clusters later)
df <- db1_TTRev |>
  left_join(db2_CloudRev, by = "Partner") |>
  left_join(db3_DesktopRev, by = "Partner", suffix = c("", "_Desktop")) |>
  left_join(db4_Brand, by = "Partner") |>
  left_join(db5_Size,  by = "Partner")

# ------------------------- 3) Log-transform matrices -------------------------
log_plus1 <- function(x) log10(x + 1)

as_log_matrix <- function(tbl, drop_cols) {
  m <- tbl |> column_to_rownames("Partner")
  m <- m[, setdiff(names(m), drop_cols), drop = FALSE]
  as.matrix(log_plus1(m))
}

Log_TTRev   <- as_log_matrix(db1_TTRev,   drop_cols = "TotalRevenueSum")
Log_Cloud   <- as_log_matrix(db2_CloudRev, drop_cols = "TotalCloudRevenueSum")
Log_Desktop <- as_log_matrix(db3_DesktopRev, drop_cols = c("TotalDesktopRevSum","Partner"))
Log_Brand   <- as_log_matrix(db4_Brand,   drop_cols = "TotalBrandSum")
Log_Size    <- as_log_matrix(db5_Size,    drop_cols = "TotalSizeSum")

# ------------------------- 4) PCA + HCPC per feature block -------------------
run_pca_hcpc <- function(X, ncp = 3) {
  # If the matrix has fewer than 2 columns with variance, skip gracefully
  if (is.null(dim(X)) || ncol(X) < 2 || all(apply(X, 2, sd, na.rm = TRUE) == 0)) {
    return(list(clust = setNames(rep(1L, nrow(X)), rownames(X))))
  }
  rp <- FactoMineR::PCA(X, ncp = ncp, graph = FALSE)
  hc <- FactoMineR::HCPC(rp, graph = FALSE)
  # cluster labels returned as a named vector (rownames -> cluster id)
  cl <- hc$data.clust$clust
  names(cl) <- rownames(hc$data.clust)
  list(clust = cl)
}

blocks <- list(
  TTRev   = Log_TTRev,
  Cloud   = Log_Cloud,
  Desktop = Log_Desktop,
  Brand   = Log_Brand,
  Size    = Log_Size
)

cluster_cols <- list()
for (nm in names(blocks)) {
  res <- run_pca_hcpc(blocks[[nm]], ncp = 3)
  cl  <- res$clust
  # attach to df using Partner rownames
  df[[paste0("Cluster_", nm)]] <- cl[match(df$Partner, names(cl))]
  cluster_cols[[nm]] <- paste0("Cluster_", nm)
}

# ------------------------- 5) Results & (optional) export --------------------
message("\n=== Engineered features (head) ===")
print(df |> select(Partner, starts_with("TotalRevenue20")) |> head())

message("\n=== Cluster labels (head) ===")
print(df |> select(Partner, all_of(unlist(cluster_cols))) |> head())

# Optional export (uncomment to write)
# readr::write_csv(df, file.path(out_dir, "CPEanalysisTopTier.csv"))

message("\nDone. (Visualization only — no real data required.)")
