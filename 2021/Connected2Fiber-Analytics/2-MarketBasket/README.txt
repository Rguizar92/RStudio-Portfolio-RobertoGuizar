Connected2Fiber Analytics — Market Basket (Association Rules)

Goal
Discover product co-occurrence rules and generate per-company recommendations
(two antecedents -> one consequent).

Stakeholder
Sales leadership / RevOps for cross-sell playbooks.

How to run
1) Install packages: dplyr, tidyr, stringr, arules, arulesViz
2) Open C2F_Mkt_Basket.R and replace input_path with your local CSV path.
3) Source the script. Optional: uncomment the local write.csv at the end.

Input schema (bring your own data)
Company.ID, Deal.ID, Deal.Stage, Products (semicolon-delimited list)

Outputs
Data frame DF_fin with columns: Company.ID, Product_1, Product_2,
Recommendation, support, confidence, lift. Optional local CSV export.

Notes
This repository is data-free. Do not commit CSVs or outputs.
