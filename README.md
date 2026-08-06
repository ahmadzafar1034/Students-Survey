# University Student Survey Analysis

A comprehensive data cleaning and analysis project of 1,946 university student responses from Pakistani institutions, focusing on career readiness, academic quality, facilities, health & wellbeing, and institutional performance.

**[→ View Interactive Dashboard](DASHBOARD_V3.html)** | **[→ Read Full Report](Report.html)**

---

## Overview

This project processes raw Google Forms survey data through an 18-step cleaning pipeline and presents findings via an interactive executive dashboard. The analysis covers **1,777 validated responses** across **88 survey items**, with standardized variables and quality assurance checks.

### Key Metrics
- **Total Respondents:** 1,777 students analyzed
- **Data Coverage:** 88 survey items across 5 core pillars
- **Overall Student Index:** 62.8/100 (mean)
- **Gender Distribution:** 74.7% Female, 25.3% Male

---

## Quick Start

### View the Dashboard
1. Open `DASHBOARD_V3.html` in any modern web browser
2. Navigate between tabs for different analyses
3. Use filters to explore by institution, gender, or field of study

### Reproduce the Analysis

**Requirements:**
- R 4.0+
- RStudio (recommended)

**Steps:**
```r
# Install dependencies (if needed)
# install.packages(c("tidyverse", "ggplot2", "rmarkdown"))

# Load and explore cleaned data
dat <- readRDS("data/cleaned_data.rds")

# View structure
head(dat)
str(dat)

# Regenerate report
rmarkdown::render("Report.Rmd")
```

---

## Project Structure

```
├── data/
│   ├── cleaned_data.rds          # Final cleaned dataset (R format)
│   └── cleaned_data.csv          # Final cleaned dataset (CSV format)
├── scripts/
│   ├── COMPLETE_SCRIPT.R         # Full data cleaning pipeline
│   └── dashboard_functions.R     # Dashboard-related functions
├── outputs/
│   ├── DASHBOARD_V3.html         # Interactive executive dashboard
│   ├── Report.Rmd                # Full analysis report (R Markdown)
│   ├── Report.html               # Compiled report
│   └── README.md                 # This file
└── docs/
    └── METHODOLOGY.md            # Detailed cleaning methodology
```

---

## Data Cleaning Pipeline

The analysis implements a robust 18-step cleaning process:

| Phase | Steps | Focus |
|-------|-------|-------|
| **Load & Validate** | 1–2 | Data import, structure integrity |
| **Quality Assurance** | 3–6 | Missing values, duplicates, timestamps |
| **Standardization** | 7–10 | University names, fields of study, coding |
| **Derivation** | 11–13 | Pillar scores, composite indices |
| **Finalization** | 14–18 | Validation, export, archival |

**Key Improvements:**
- ✓ University name standardization (100% mapped)
- ✓ Removal of duplicate responses
- ✓ Consistent Likert scale encoding
- ✓ Pillar score computation with validation
- ✓ CSV + RDS export formats

---

## Core Findings

### Pillar Performance
| Pillar | Mean Score | Status |
|--------|-----------|--------|
| Career Readiness | ~65 | Critical |
| Academic Quality | ~68 | At Risk |
| Facilities & Campus | ~60 | At Risk |
| Health & Wellbeing | ~58 | Stable |
| **Overall Index** | **62.8** | Mixed |

### Recommendations
1. **Career Readiness:** Industry partnerships, internship programs
2. **Facilities:** Infrastructure investment and modernization
3. **Health & Wellbeing:** Expand counseling and support services
4. **Academic Quality:** Teaching quality assurance initiatives

For detailed findings and recommendations, see **[Full Report](Report.html)**.

---

## Key Variables

The cleaned dataset includes:

- `university` – Standardized institution name
- `gender` – Respondent gender
- `field_of_study` – Academic discipline
- `career_readiness_score` – Pillar score (0–100)
- `academic_quality_score` – Pillar score (0–100)
- `facilities_score` – Pillar score (0–100)
- `health_wellbeing_score` – Pillar score (0–100)
- `overall_index` – Overall student index (0–100)

See full data dictionary in the report.

---

## Files Guide

| File | Purpose |
|------|---------|
| `DASHBOARD_V3.html` | Interactive dashboard (open in browser) |
| `Report.Rmd` / `Report.html` | Comprehensive analysis report |
| `cleaned_data.rds` | Final dataset (R format, recommended for analysis) |
| `cleaned_data.csv` | Final dataset (CSV format, for Excel/SQL) |
| `COMPLETE_SCRIPT.R` | Full reproducible data cleaning pipeline |

---

## Usage

### For Stakeholders
1. Open `DASHBOARD_V3.html` to explore results interactively
2. Download filtered data from the dashboard's "Data Explorer" tab
3. Share dashboard link or embedded version in reports

### For Analysts
1. Load `cleaned_data.rds` in R
2. Refer to `COMPLETE_SCRIPT.R` for data cleaning logic
3. Run `Report.Rmd` to generate fresh analysis

### For Developers
1. Review `COMPLETE_SCRIPT.R` for standardization functions
2. Adapt university/field mappings for your context
3. Reference error handling patterns for survey data

---

## Data Quality

- **Completeness:** 91.3% response rate (1,777 of 1,946 records retained)
- **Consistency:** 99.8% valid Likert responses, 100% standardized institutions
- **Limitations:** Self-selection bias, social desirability, temporal variation

See report for detailed quality assessment.

---

## Publishing & Sharing

### Share the Dashboard
- **RPubs:** [Publish easily on RPubs.com](https://rpubs.com)
- **GitHub Pages:** Enable in repository settings (free static hosting)
- **Posit Connect Cloud:** Professional deployment with features

### Share the Report
- Compile to HTML: `rmarkdown::render("Report.Rmd")`
- Share `.html` file directly or embed in websites
- Generate PDF for archival

---

## FAQ

**Q: Can I update the analysis with new data?**  
A: Yes. Load new raw data, run `COMPLETE_SCRIPT.R`, regenerate dashboard and report.

**Q: How do I adapt this for a different survey?**  
A: Modify mapping vectors in Steps 7–10 of `COMPLETE_SCRIPT.R` for your field names and categories.

**Q: What if my cleaned data doesn't match?**  
A: Check the before/after verification output in the script. Missing assignments (`dat <-`) are common culprits.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026 | Initial release; 18-step pipeline; dashboard v3 |

---

## Contact & Support

- **Project Repository:** [GitHub]
- **Report Issues:** [GitHub Issues]
- **Questions:** Contact the data analysis team

---

**Classification:** Internal Use  
**Last Updated:** August 2026

