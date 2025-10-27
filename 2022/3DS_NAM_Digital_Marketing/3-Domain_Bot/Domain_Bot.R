###############################################################################
# Domain Bot — Company → Homepage URL helper (Visualization Only, 2022)
#
# Purpose
#   Demonstrate how I automated tedious domain lookups from company names.
#   Default behavior is simulation-only (no network calls) for portfolio use.
#
# Usage (from repo root)
#   Rscript 2022/3-Domain_Bot/Domain_Bot.R
#   # optional args:
#   #   --simulate=true|false   (default true; false will attempt web lookups)
#   #   --outfile=domains_search.csv
#
# Notes
#   - This script is for visualization only. It does not include real CSVs.
#   - When simulate=FALSE, you accept responsibility to follow site ToS/robots.
#   - Prefer official partner lists/APIs or your own curated CSV for production.
###############################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(rvest)
  library(here)
})

# -------------------------- CLI args & params --------------------------------
args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", key, "="), "", hit[1]) else default
}

simulate <- tolower(get_arg("simulate", "true")) %in% c("true","1","yes","y")
outfile  <- get_arg("outfile", "domains_search.csv")

out_dir  <- here("2022","3-Domain_Bot","outputs")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# -------------------------- Demo companies -----------------------------------
demo_companies <- c(
  "Acme Robotics", "Northwind Traders", "Globex Corporation",
  "Initech", "Umbrella Research", "Vandelay Industries"
)

# -------------------------- Core function ------------------------------------
# NOTE: Default simulate=TRUE to avoid hitting search engines in portfolio.
domain_bot <- function(terms, simulate = TRUE, provider = c("duckduckgo","google"),
                       pause_sec = 3, write_output = TRUE, outfile = NULL) {
  
  provider <- match.arg(provider)
  
  if (simulate) {
    # Create clean, deterministic demo domains
    df <- tibble(
      Company = terms,
      URL = paste0("https://", gsub("[^a-z0-9]+", "", tolower(terms)), ".example.com")
    )
    if (write_output && !is.null(outfile)) readr::write_csv(df, outfile)
    message("Simulation mode: wrote ", nrow(df), " rows.")
    return(df)
  }
  
  # If you disable simulation, you are responsible for ToS/robots compliance.
  # For demonstration, we show a cautious approach with DuckDuckGo HTML.
  # Many search engines prohibit scraping; use official sources when possible.
  
  ua <- "Mozilla/5.0 (Portfolio Visualization; R rvest)"
  get_first_result <- function(query) {
    q <- URLencode(query)
    url <- switch(provider,
                  duckduckgo = paste0("https://html.duckduckgo.com/html/?q=", q),
                  google     = paste0("https://www.google.com/search?q=", q))  # often disallowed
    # Fetch and parse
    pg <- tryCatch({
      ht <- read_html(httr::GET(url, httr::add_headers(`User-Agent` = ua)))
      ht
    }, error = function(e) NULL)
    
    if (is.null(pg)) return(NA_character_)
    
    if (provider == "duckduckgo") {
      # Links are in .result__a elements (HTML variant)
      links <- pg %>% html_elements(".result__a") %>% html_attr("href")
    } else {
      # Google markup changes frequently; this is illustrative only
      links <- pg %>% html_elements("a") %>% html_attr("href")
    }
    
    # Grab the first plausible HTTP(S) URL
    hit <- links[grepl("^https?://", links)]
    if (length(hit)) hit[1] else NA_character_
  }
  
  res <- vector("list", length(terms))
  for (i in seq_along(terms)) {
    message(sprintf("[%d/%d] %s", i, length(terms), terms[i]))
    res[[i]] <- tibble(Company = terms[i], URL = get_first_result(terms[i]))
    Sys.sleep(pause_sec)  # be polite
  }
  
  df <- bind_rows(res)
  if (write_output && !is.null(outfile)) readr::write_csv(df, outfile)
  df
}

# -------------------------- Run (portfolio default) --------------------------
companies <- demo_companies   # in real use, read from a CSV column
# e.g., real: bomb <- read_csv(here("...","google domains.csv")); companies <- bomb$Company

out_path <- file.path(out_dir, outfile)
results <- domain_bot(terms = companies,
                      simulate = simulate,
                      provider = "duckduckgo",
                      pause_sec = 3,
                      write_output = TRUE,
                      outfile = out_path)

print(head(results))

message("Done. Outputs (if any) are under: ", out_dir)
