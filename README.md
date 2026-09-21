# falls-data-linkage-project

Linking NIAS Ambulance Dispatch Data with HSC Encompass ED Records

An analysis of outcomes for patients aged 50+ presenting to Emergency Departments across Northern Ireland following a fall

MSc Data Analytics dissertation project, Queen's University Belfast, in partnership with the Health and Social Care (HSC) Belfast Trust and the DHCNI Data Institute. Data and analysis scope: all HSC Regional Trusts.

# Overview

Falls in adults aged 50+ are a major driver of both Northern Ireland Ambulance Service (NIAS) callouts and Emergency Department (ED) attendances — yet the two systems that respond to them have never been linked at a population level. NIAS dispatch records capture the point of first contact (call category, response and handover timings, conveyance decisions); HSC's Encompass system captures the clinical course that follows ED arrival. Analysed in isolation, neither tells the full story of what happens to an older person from the moment they fall to the moment they're discharged.

To the authors' knowledge, no published study has linked NIAS dispatch data with Encompass ED records for the over-50 falls population across HSC Regional Trusts. This project builds and evaluates a deterministic linkage that closes that gap, then uses it to characterise outcomes — admission, length of stay, and repeat attendance — for this population.

# Key Findings
Finding	Result
Confirmed linked records	12,212 (62.4% of eligible NIAS falls incidents)
Strongest predictor of admission	Care setting — 3.57× odds of admission for falls outside a care/residential setting vs. within one (95% CI 3.15–4.05, p < 0.001) — a variable only available through linkage
Predictive value of linkage	Admission model AUC improved from 0.65 (ED-only) to 0.72 (ED + NIAS variables), consistent across 4 modelling techniques
Trust-level variation	27-percentage-point spread in admission rate (67.3% South Eastern vs. 39.8% Northern)
Repeat attendance	Weakest-performing outcome (best AUC 0.64, XGBoost); tree-based methods clearly outperformed linear methods
Sex effect on admission	Not significant (OR 1.09, p = 0.099)

Full methodology, results, statistical detail, and discussion are in the dissertation report (not included in this repository — see Data & Reports below).

Data Sources
Source	Description	Scale
Encompass (HSC)	ED attendance extract: demographics, triage acuity, disposition, arrival/discharge timing, free-text clinical fields	72,499 attendances, 1 year
NIAS	Falls-specific ambulance dispatch extract: incident number, call/dispatch data, handover timings, hospital attended, care setting, patient sex	19,575 falls-related incidents (patients aged 40+ at incident, as extracted by NIAS)

Linkage used deterministic matching on Ambulance Incident Number (Encompass) against Incident Number (NIAS) — no probabilistic matching and no patient identifiers required.

⚠️ Data & Privacy — Not Included

Neither the raw data nor any patient-level extract is included in, or was ever intended to be included in, this repository. Both source datasets contain confidential patient-level information and are subject to HSC information governance policy and UK GDPR. Data cannot be shared, published, or provided to any party outside the approved project team under any circumstances. Access for verification purposes must be requested through HSC's Honest Broker Service.

This repository contains analysis code only. All figures, tables and statistics quoted here and in the accompanying report were generated from disclosure-controlled outputs (see Governance below).

Repository Structure
├── R/
│   └── NIAS_ED_Linkage_Script.R   # Full analysis pipeline (cleaning, linkage, modelling, suppression)
├── figures/                        # Disclosure-controlled chart outputs (no patient-level data)
├── README.md
└── LICENSE
Methodology Summary
Data cleaning & quality assessment — field-level completeness audit (e.g. Health & Care Number 99.2% complete, Ambulance Incident Number 88.4% complete); duplicate column resolution; implausible-value flagging (not silent removal).
Falls cohort identification — tiered free-text keyword search of Encompass clinical fields, distinguishing high-confidence direct matches from lower-confidence associated-symptom matches.
Linkage — deterministic ID matching, with a corrected eligibility approach after Arrival Method was found unreliable as a filter during development.
Validation — independent system concordance check (NIAS vs. Encompass timestamps agree to a median of 5 minutes across all linked records) and representativeness checks comparing the linked subset against the wider falls cohort.
Modelling — logistic regression, random forest, elastic net, and XGBoost, applied to three outcomes: admission, prolonged length of stay, and repeat attendance; ED-only models compared against models incorporating linked NIAS variables.
Governance & disclosure control — complementary (secondary) low-number suppression applied throughout, so that no single suppressed cell (<10) can be back-calculated from visible totals.

See the R script for full implementation detail and inline documentation.

Requirements
r
# Core packages used throughout the pipeline
install.packages(c(
  "readxl", "janitor", "dplyr", "tidyr", "stringr", "lubridate",
  "ggplot2", "forcats", "randomForest", "glmnet", "xgboost",
  "pROC", "broom"
))

R version 4.x recommended. The script expects Encompass and NIAS extracts in the format described in the Methodology section; as these datasets are not distributable (see above), the script cannot be run end-to-end without equivalent, appropriately governed source data.

# Limitations
Falls cohort identification relies on free-text keyword classification, not a validated clinical code
Linkage covers only ambulance-conveyed incidents; the true population-level NIAS–ED overlap may be broader
No deprivation measure was available for inclusion
Predictive performance remains moderate overall; clinical severity data (ED diagnosis field) was not incorporated within this project's timeframe
Handover delay and concordance analyses assume synchronised system clocks between NIAS and Encompass, not independently verified
Data & Reports

The full dissertation report, including complete methodology, results, discussion, and reference list, is available on request.

# Author

Rachel Naveena E J — MSc Data Analytics, Queen's University Belfast Placement hosted by HSC Belfast Trust

Acknowledgments
Queen's University Belfast, School of Mathematics and Physics
DHCNI Data Institute
HSC Belfast Trust and Northern Ireland Ambulance Service (NIAS)
