# Domain Bot — company name to homepage URL (2022)

**Goal**  
Show how repetitive domain lookups can be automated safely and reproducibly.

**Context**  
Built during my 2022 internship to reduce manual search time when enriching partner/company records.

---

## ⚠️ Data & Ethics Note

This project is **for visualization purposes only**.  
By default, the script runs in **simulation mode** and does **not** hit any search engine.  
If you disable simulation, you are responsible for respecting website **Terms of Service**, **robots.txt**, and rate limits. Prefer official data sources or your own curated lists.

---

## How it works

- Accepts a vector of company names  
- In **simulation mode** (default), returns deterministic `example.com` URLs so reviewers can run it without data or network access  
- In real-world mode (optional), queries a web search page and extracts the first plausible URL with a polite delay

---

## Quick start

From the repository root:

```bash
# Simulation (default) — safe for portfolio
Rscript 2022/3-Domain_Bot/Domain_Bot.R

# Explicit simulation flag
Rscript 2022/3-Domain_Bot/Domain_Bot.R --simulate=true

# (Optional) attempt real lookups — use responsibly
Rscript 2022/3-Domain_Bot/Domain_Bot.R --simulate=false --outfile=domains_search.csv
