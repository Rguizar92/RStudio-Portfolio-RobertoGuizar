# CPE Partner Analysis — Feature Engineering and PCA (2022)

**Goal**  
Show how recurring partner performance data can be engineered into analytical features and explored with dimensionality-reduction techniques to uncover revenue and brand patterns.  
The script demonstrates:  
- Creation of yearly and cumulative **revenue features** (total, cloud, desktop)  
- Aggregation of **brand** and **size** data  
- Transformation and scaling of variables for PCA  
- Application of **Principal Component Analysis (PCA)** and **Hierarchical Clustering (HCPC)** to group partners by behavior and portfolio mix  

---

## Context
Developed during my **NAM Digital Marketing Internship (Dassault Systèmes, 2022)**.  
The objective was to automate exploratory partner segmentation and highlight dependencies between **brands**, **revenue type**, and **size**, supporting internal strategic dashboards.

---

## ⚠️ Data Note
This project is **for visualization purposes only**.  
No actual or synthetic CSVs are included.  
The script generates a small in-memory demo dataset when run so that the logic and visualization steps are reproducible without confidential data.

---

## How It Works
1. Reads (or creates) a dataset of partner revenues by year and category  
2. Computes total, cloud, and desktop revenues and growth rates  
3. Aggregates brand and size indicators across years  
4. Log-transforms numeric features for stability  
5. Runs PCA and HCPC clustering for each feature block  
6. Prints example tables of engineered features and resulting cluster labels  

---

## Quick Start

From the repository root:

```bash
Rscript 2022/CPE_Partner_Analysis/CPE_Partner_Analysis.R
# optional: set a revenue threshold
Rscript 2022/CPE_Partner_Analysis/CPE_Partner_Analysis.R 7000
