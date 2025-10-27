# =============================================================================
# Project: Connected2Fiber Analytics - Logistic Regression (Win/Loss)
# Author: Roberto Guízar
# Purpose: Train and evaluate a binary logistic model to predict Closed Won vs Closed Lost.
#          Reviewer can expect: input schema hints, safe placeholders, no data or writes.
# Inputs (bring your own data): CSV with at least columns:
#   Company.ID, Deal.ID, Deal.Stage, Products, Create.Date...Monthly, Close.Date...Monthly,
#   Annual.Recurring.Revenue..ARR., Contract.Term
# Expected Outputs (local-only, user-driven): optional CSV of fresh predictions.
# How to run: Open in RStudio, install packages (see repo README), replace the example path
#             in the "Load data" section with your local file path.
# Notes: This repository is data-free. Do not commit any CSVs or outputs.
# =============================================================================

# --- Packages ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)     # mutate, case_when, if_else, etc.
  library(gmodels)   # CrossTable for confusion matrix
})

# --- Parameters & Seeds -----------------------------------------------------
cutoff <- 0.8     # classification cutoff for predicted probabilities
set.seed(123)     # deterministic seed for reproducibility
RNGkind(sample.kind = "Rounding")

# --- Load data --------------------------------------------------------------
# Replace with your local file path. Keep inputs on your machine only.
# Example:
# db <- read.csv("your_local_path/deals.csv", stringsAsFactors = FALSE)

db <- read.csv("your_local_path/deals.csv", stringsAsFactors = FALSE)

options(scipen = 999) # avoid scientific notation in prints

# --- Light cleaning ---------------------------------------------------------
# Remove missing Company IDs and duplicate deals
db <- db[db$Company.ID != "(No value)", ]
db$Company.ID <- as.factor(as.character(db$Company.ID))
db <- db[!duplicated(db$Deal.ID), ]

# --- Won/Lost counts by Company --------------------------------------------
# (No intermediate file writes; do everything in-memory)
matrix_won_lost <- table(db$Company.ID, db$Deal.Stage)
WLcounts <- as.data.frame.matrix(matrix_won_lost)
WLcounts$Company.ID <- rownames(WLcounts)

lost_col <- colnames(WLcounts)[grepl("Lost", colnames(WLcounts), ignore.case = TRUE)]
won_col  <- colnames(WLcounts)[grepl("Won",  colnames(WLcounts), ignore.case = TRUE)]

Won_lost_counts <- data.frame(
  Company.ID = WLcounts$Company.ID,
  Lost = if (length(lost_col)) WLcounts[[lost_col[1]]] else 0,
  Won  = if (length(won_col))  WLcounts[[won_col[1]]]  else 0,
  row.names = NULL
)

db_WLcounts <- merge(db, Won_lost_counts, by = "Company.ID", all.x = TRUE)

# --- Sum of Products per Deal ----------------------------------------------
RepProduct <- data.frame(
  Deal.ID    = db_WLcounts$Deal.ID,
  Company.ID = db_WLcounts$Company.ID,
  Products   = db_WLcounts$Products,
  stringsAsFactors = FALSE
)

# Helper: count separators (“;”) to estimate number of listed products
count_seps_plus_one <- function(x, sep = ";") {
  ifelse(is.na(x) | x == "", 0L, lengths(regmatches(x, gregexpr(sep, x))) + 1L)
}

RepProduct$TotalProducts <- count_seps_plus_one(RepProduct$Products)

# --- Dates: cycle time & bins ----------------------------------------------
Dates <- data.frame(
  Deal.ID    = db_WLcounts$Deal.ID,
  Company.ID = db_WLcounts$Company.ID,
  Start.Date = as.Date(db_WLcounts$Create.Date...Monthly, format = "%m/%d/%Y"),
  End.Date   = as.Date(db_WLcounts$Close.Date...Monthly,   format = "%m/%d/%Y")
)

Dates$difference <- as.numeric(difftime(Dates$End.Date, Dates$Start.Date, units = "days"))

Dates <- Dates %>%
  mutate(Date.Bin = case_when(
    difference <  90                     ~ "Less than 3 months",
    difference >=  90 & difference < 180 ~ "Less than 6 months",
    difference >= 180 & difference < 270 ~ "Less than 9 months",
    difference >= 270 & difference < 360 ~ "Less than 12 months",
    difference >= 360 & difference < 450 ~ "Less than 15 months",
    difference >= 450 & difference < 540 ~ "Less than 18 months",
    difference >= 540 & difference < 630 ~ "Less than 21 months",
    difference >= 630 & difference < 720 ~ "Less than 24 months",
    difference >= 720                    ~ "More than 24 months",
    TRUE                                 ~ "Unknown"
  ))

# --- Revenue bins -----------------------------------------------------------
Revenue <- data.frame(
  Deal.ID    = db_WLcounts$Deal.ID,
  Company.ID = db_WLcounts$Company.ID,
  Revenue    = suppressWarnings(as.numeric(db_WLcounts$Annual.Recurring.Revenue..ARR.))
)

Revenue <- Revenue %>%
  mutate(Revenue.Bin = case_when(
    !is.na(Revenue) & Revenue <  20000 ~ "Less than $20,000",
    !is.na(Revenue) & Revenue <  40000 ~ "Less than $40,000",
    !is.na(Revenue) & Revenue <  60000 ~ "Less than $60,000",
    !is.na(Revenue) & Revenue <  80000 ~ "Less than $80,000",
    !is.na(Revenue) & Revenue < 100000 ~ "Less than $100,000",
    !is.na(Revenue) & Revenue >=100000 ~ "More than $100,000",
    TRUE                               ~ "Unknown"
  ))

# --- Assemble model frame ---------------------------------------------------
Data_Final <- RepProduct %>%
  left_join(db_WLcounts[, c("Deal.ID","Deal.Stage","Lost","Won","Contract.Term")], by = "Deal.ID") %>%
  left_join(Dates[, c("Deal.ID","difference","Date.Bin")], by = "Deal.ID") %>%
  left_join(Revenue[, c("Deal.ID","Revenue","Revenue.Bin")], by = "Deal.ID")

# Outcome from Deal.Stage
Data_Final <- Data_Final %>%
  mutate(Outcome = if_else(Deal.Stage == "Closed Won", "Won",
                           if_else(Deal.Stage == "Closed Lost", "Lost", NA_character_)))

# Contract.Term to (approx) years, coerced to integer ceiling
Contract <- as.character(Data_Final$Contract.Term)
Contract <- gsub(" |year|years", "", Contract)
idx_month <- grepl("month|months", Contract, ignore.case = TRUE)
years_convert <- suppressWarnings(as.numeric(gsub("month|months", "", Contract[idx_month])) / 12)
Contract[idx_month] <- ifelse(is.na(years_convert), NA, years_convert)
Contract <- suppressWarnings(ceiling(as.numeric(Contract)))
Data_Final$Contract.Term <- factor(ifelse(is.na(Contract), NA, Contract))

# --- Filter out non win/loss pipeline stages & define "fresh" ---------------
# Remove post-sale / implementation stages
drop_stages <- c(
  "Planned","Customer Engagement","Customer Welcome Email","Data Ingestion & Implementation",
  "Internal Engagement","Internal Sales Transition","Training & Production Launch","Project Completed"
)
Data_Final <- Data_Final[!(Data_Final$Deal.Stage %in% drop_stages), ]

# Fresh = everything not yet closed (for future scoring)
Data_Fresh <- Data_Final[!(Data_Final$Deal.Stage %in% c("Closed Won","Closed Lost")), ]

# --- Prepare modeling data ---------------------------------------------------
Data_Final2 <- Data_Final %>%
  transmute(
    Company.Products = as.integer(TotalProducts),
    Date.Bins        = factor(Date.Bin),
    Revenue.Bins     = factor(Revenue.Bin),
    Contract.Term    = factor(Contract.Term),
    Outcome          = Outcome
  )

# Binary target: Won = 1, Lost = 0
Data_Final2$myresponse <- factor(as.numeric(Data_Final2$Outcome == "Won"))
Data_Final2$Outcome <- NULL

# --- Quick self-checks (safe) -----------------------------------------------
# str(Data_Final2); summary(Data_Final2)
# table(Data_Final2$myresponse); table(Data_Final2$Contract.Term)

# --- Descriptive model -------------------------------------------------------
logistic_desc <- glm(myresponse ~ ., family = binomial, data = Data_Final2)
# summary(logistic_desc)

# --- Train/Test split (stratified) ------------------------------------------
numpredictors <- ncol(Data_Final2) - 1
numfac <- sum(vapply(Data_Final2[ , 1:numpredictors, drop = FALSE], is.factor, logical(1)))

nobs <- nrow(Data_Final2)

prop_tbl <- prop.table(table(Data_Final2$myresponse))
length.vector <- round(0.8 * nobs * prop_tbl)
train_size <- sum(length.vector)
test_size  <- nobs - train_size
class.names <- names(prop_tbl)
numb.class <- length(class.names)

resample <- 1
while (resample == 1) {
  train_index <- integer(0)
  for (i in seq_len(numb.class)) {
    idx_class <- which(Data_Final2$myresponse == class.names[i])
    train_index <- c(train_index, sample(idx_class, length.vector[i], replace = FALSE))
  }
  mydata_train <- Data_Final2[train_index, ]
  mydata_test  <- Data_Final2[-train_index, ]
  
  right_fac <- 0
  for (i in seq_len(numpredictors)) {
    if (is.factor(mydata_train[, i])) {
      if (setequal(
        intersect(unique(mydata_train[, i]), unique(mydata_test[, i])),
        unique(Data_Final2[, i])
      )) right_fac <- right_fac + 1
    }
  }
  resample <- if (right_fac == numfac) 0 else 1
}

# --- Fit predictive model ----------------------------------------------------
logistic_fit <- glm(myresponse ~ ., family = binomial, data = mydata_train)

# --- Evaluate on test --------------------------------------------------------
predicted <- predict(logistic_fit, mydata_test, type = "response")
pred_tbl <- data.frame(
  id = as.numeric(rownames(mydata_test)),
  predicted = as.numeric(predicted),
  myresponse = as.numeric(mydata_test$myresponse)
)
pred_tbl$predicted_class <- as.numeric(pred_tbl$predicted >= cutoff)

percent_correct_logistic <-
  100 * mean(pred_tbl$myresponse == pred_tbl$predicted_class)
print(paste("Percentage of Correct Classifications for the Logistic Regression Model is:",
            round(percent_correct_logistic, 2), "percent"))

# Confusion matrix (True vs Predicted)
Confusion_Matrix <- CrossTable(
  pred_tbl$myresponse, pred_tbl$predicted_class,
  dnn = c("True Class", "Predicted Class"),
  prop.chisq = FALSE, prop.t = FALSE, prop.c = FALSE, prop.r = FALSE
)

# --- Fresh scoring (optional, local-only) -----------------------------------
# If you have new, non-closed records in Data_Fresh and want local predictions:
# full.data.model <- glm(myresponse ~ ., family = binomial, data = Data_Final2)
# fresh_classifications <- data.frame(predicted = predict(full.data.model, Data_Fresh, type = "response"))
# table_with_classifications <- cbind(Data_Fresh, fresh_classifications)
# table_with_classifications$predicted_class <- as.numeric(table_with_classifications$predicted >= cutoff)
# Example output (uncomment to write locally):
# write.csv(table_with_classifications, "your_local_output/FinalpredictionFresh2.csv", row.names = FALSE)
