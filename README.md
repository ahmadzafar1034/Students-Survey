# University Student Survey Analysis

A comprehensive data cleaning and analysis project of 1,946 university student responses from Pakistani institutions, focusing on career readiness, academic quality, facilities, health & wellbeing, and institutional performance.

**[→ View Interactive Dashboard](https://019fd689-dabf-8d98-d8e7-2b3e14fb13b8.share.connect.posit.cloud/)** | **[→ Read Full Report](Report.html)**

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
Click the link above to access the interactive dashboard. Navigate between tabs for different analyses and use filters to explore by institution, gender, or field of study.

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
| Dashboard | [Published on Posit Connect Cloud](https://019fd689-dabf-8d98-d8e7-2b3e14fb13b8.share.connect.posit.cloud/) |
| Report.html | Comprehensive analysis report with visualizations |
| cleaned_data.rds / .csv | Final cleaned dataset for analysis |
| COMPLETE_SCRIPT.R | Full data cleaning pipeline |

---

## Usage

### For Stakeholders
1. Access the [interactive dashboard](https://019fd689-dabf-8d98-d8e7-2b3e14fb13b8.share.connect.posit.cloud/) to explore results
2. Use filters to drill down by institution, gender, or field of study
3. Share the dashboard link with colleagues and stakeholders

### For Analysts
Refer to the full report and cleaned datasets in the project repository for deeper analysis and methodology details.

### For Developers
Review the project repository for data cleaning scripts and standardization functions to adapt for your own survey work.

---

## Data Quality

- **Completeness:** 91.3% response rate (1,777 of 1,946 records retained)
- **Consistency:** 99.8% valid Likert responses, 100% standardized institutions
- **Limitations:** Self-selection bias, social desirability, temporal variation

See report for detailed quality assessment.

---

## FAQ

**Q: How do I access the dashboard?**  
A: Click the dashboard link at the top of this page. It's hosted on Posit Connect Cloud and accessible from any browser.

**Q: Can I download the data?**  
A: Yes, the cleaned datasets (RDS and CSV formats) are available in the project repository.

**Q: Where can I find the detailed analysis?**  
A: See the Full Report link at the top for comprehensive findings, visualizations, and recommendations.

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2026 | Initial release; 18-step pipeline; dashboard v3 |

---

**Dashboard:** [Posit Connect Cloud](https://019fd689-dabf-8d98-d8e7-2b3e14fb13b8.share.connect.posit.cloud/)  
**Last Updated:** August 2026

