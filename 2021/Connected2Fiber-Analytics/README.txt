Connected2Fiber Analytics (Consulting Project)
January 2021 – May 2021
Milford, MA, USA

About Connected2Fiber (now Connectbase)
Connected2Fiber, headquartered in Milford, Massachusetts, is a SaaS company providing a location-based intelligence and automation platform for network service providers. Its platform enables telecom and broadband vendors to identify, price, and market connectivity opportunities with precision, combining geospatial data with CRM insights. (Learn more at https://www.connectbase.com)

Project Context
This folder contains three R modules developed as part of an academically sponsored consulting engagement with Connected2Fiber Inc. The goal was to analyze CRM and deal-level data to improve sales and marketing funnel efficiency, quantify bundle adoption, and design repeatable analytical models. The project combined data engineering, statistical modeling, and process improvement consulting.

My Role
Data analytics and machine learning consultant. Responsibilities included exploratory data analysis, logistic regression modeling for win/loss prediction, association rule mining for cross-sell recommendations, and design of bundle completeness labeling. Also contributed to reporting and presentation of model insights to Connected2Fiber’s management team.

Repository Structure
Each numbered subfolder corresponds to a key analytical module:
1-LogReg — Predictive model estimating the probability of a deal being "Closed Won" using logistic regression.
2-MarketBasket — Market basket analysis of product co-occurrence and cross-sell recommendations per company.
3-Bundles — Rule-based classification of Closed Won deals into product bundle categories (B1/B2 completeness).

How to Reproduce
1) Clone the repo and restore packages with renv (see repo root README).
2) Each subfolder includes its own README.txt describing required columns, parameters, and safe local paths.
3) These modules are data-free. Users must provide their own anonymized CSVs following the input schema noted in each module.

Stakeholder Relevance
The three scripts demonstrate delivery-focused analytics relevant to SaaS sales optimization:
- Predictive modeling (lead scoring)
- Recommendation systems (cross-sell/upsell)
- Business rule automation (bundle identification)

All code is reproducible, commented, and designed for clarity and data privacy.
