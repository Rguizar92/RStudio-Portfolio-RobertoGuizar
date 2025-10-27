# Connected2Fiber Analytics — Logistic Regression (Win/Loss)

**Goal**  
Train and evaluate a binary logistic model to predict Closed Won vs Closed Lost.

**Stakeholder**  
Sales leadership and RevOps, for pipeline health and prioritization.

**How to run**  
1. Install packages (see below).  
2. Open `C2F_LogReg.R` in RStudio.  
3. Replace the example input path with your local CSV path.  
4. Source the script.

**Input schema (bring your own data)**  
`Company.ID, Deal.ID, Deal.Stage, Products, Create.Date...Monthly, Close.Date...Monthly, Annual.Recurring.Revenue..ARR., Contract.Term`

**Outputs**  
Printed accuracy and confusion matrix. Optional local CSV for fresh records (commented in code).

**Packages**  
`dplyr, gmodels`

**Notes**  
This repository is data-free. Do not commit CSVs or outputs.
