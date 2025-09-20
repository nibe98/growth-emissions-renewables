# Growth, Emissions, and Renewables: A Cross-Country Analysis (1960–2022)
This project examines the relationship between economic growth, CO₂ emissions, and renewable energy adoption across countries from 1960–2022. Using World Bank indicators, it explores whether wealthier nations can decouple prosperity from environmental harm, and how selected countries (China, Germany, Colombia) compare over time.

## Project Summary
**Data source:** World Bank Indicators (1965-2022)

**Main questions:**
  1. Is there a relationship between GDP per capita and CO₂ emissions?
  2. Which countries invest the most in renewable energy?
  3. How do China, Germany, and Colombia compare over time?

## Methods
- Data cleaning (removed regional aggregates, dropped invalid rows, standardized variable names, merged datasets, etc.)
- OLS regressions with and without year fixed effects
- Comparative analysis and visualizations (scatter plots and panel time trends across countries and years)

## Key Findings
- Higher income is strongly associated with higher CO₂ emissions.
- Energy use per capita almost proportionally drives emissions.
- Wealthier countries do not necessarily rely more on renewables.
- Renewable leaders include both high-income (Iceland, Norway) and middle-income countries (Brazil, Colombia, Vietnam).
- Germany shows a policy-driven energy transition, Colombia relies on hydropower, and China shows late but rapid renewable adoption.

## Repository Structure
```
├── README.md # Project description
├── r_script.R # R script with data cleaning & analysis
├── project_summary.pdf #  Report
└── data/ #contains the raw World Bank datasets (CSV format)
```
