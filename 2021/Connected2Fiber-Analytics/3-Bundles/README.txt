Connected2Fiber Analytics — Bundles (Closed-Won labeling)

Goal
Label Closed Won deals by presence of predefined product bundles.
Codes: B1/B1C for bundle1 (partial/complete), B2/B2C for bundle2, combos like B1-B2C,
NB when no bundle items appear.

Stakeholder
Sales leadership / RevOps to quantify attach and completeness of strategic bundles.

How to run
1) Install packages: dplyr, stringr, tidyr
2) Open C2F_Bundles.R. Set input_path to your local CSV.
3) Edit bundle1_items and bundle2_items to match your product catalog.
4) Source the script. Optional: uncomment the local write.csv at the end.

Input schema (bring your own data)
Company.ID, Deal.ID, Deal.Stage, Products (semicolon-delimited)

Outputs
Adds column 'bundle' per deal with codes: B1, B1C, B2, B2C, B1-B2, B1C-B2C, NB.
