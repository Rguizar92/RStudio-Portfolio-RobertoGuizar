# Megalist — Weekly Lead Consolidation and Enrichment (2022)

**Goal**  
Show how recurring data-cleaning and reporting work can be automated with R.  
The script demonstrates a reproducible pipeline that:
1. Merges multiple lead CSVs into one standardized dataset  
2. Enriches job titles into levels and departments using a picklist  
3. Normalizes U.S. state names  
4. Extracts OP or MKTCONF identifiers with regex  
5. Generates weekly and vendor-level summary tables

---

## Context
Created during my **NAM Digital Marketing Internship (Dassault Systèmes, 2022)**.  
This project automated a previously manual weekly workflow and served as a foundation for later KPI dashboards.

---

## ⚠️ Data Note
This repository is **for visualization purposes only**.  
No real or synthetic CSVs are included.  
All file names and examples are placeholders to illustrate structure and reproducibility.

---

## Expected Inputs (described for reference)
| File name (placeholder) | Purpose | Key columns |
|--------------------------|----------|--------------|
| `leads_2021_2022H1_sample.csv` | Prior half-year lead export | `JobTitle`, `AccountState`, `Email`, etc. |
| `leads_2022H2_sample.csv` | Current half-year export | same schema |
| `picklist_sample.csv` | Job-level and department lookup | `Job.Level`, `Job.Details`, `Department`, `Department.Details` |
| `state_abbs_sample.csv` | U.S. state abbreviation map | `AccountState`, `STATE.NAME` |
| `op_catalog_2022_sample.csv` | OP/MKTCONF master catalog | `NewMCC`, …  |

---

## How to Run (demonstration only)
```bash
Rscript 2022/megalist/megalist.R 20220909
