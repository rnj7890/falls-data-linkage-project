#getwd()
rm(list = ls())
## ============================================================================
## NIAS-ED FALLS LINKAGE PROJECT — FULL CONSOLIDATED PIPELINE (v3 — HSC GOVERNANCE REVIEW)
## ============================================================================
## Changes in v3 (this version):
##   - Low-number suppression (SDC) added across all EDA charts, printed
##     tables and CSV exports: any category cell with n < N_SUPPRESS_THRESHOLD
##     (currently 10, per HSC Data Governance) now displays as "<10" instead
##     of the exact count, and its paired percentage is blanked too (see
##     suppress_n()/suppress_pct(), Part 0). Headline population totals are
##     NOT suppressed, only category/subgroup breakdowns — see the comment
##     above suppress_n() for scope.
##   - Fixed count/percentage labels being clipped on wide horizontal bar
##     charts (hospital volume, arrival method, care setting, sex, age
##     band): added scale_y_continuous() expansion headroom and widened
##     the affected ggsave() image dimensions so long labels fully display.
##   - Fixed Model A2's random forest failing with "Type of predictors in
##     new data do not match that of the training data": randomForest
##     handles "y ~ . - x" formula exclusion inconsistently between fit
##     and predict; los_hours is now dropped from the data itself before
##     fitting rather than excluded via formula syntax.
##   - Added elastic net (glmnet) and gradient boosting (XGBoost) alongside
##     the existing logistic regression + random forest comparison, for
##     all four outcomes (Models A1, B1, A2, A3) — see fit_glmnet_xgboost().
##   - CRITICAL FIX: "ED length of stay" was being calculated as
##     difftime(disch_dt, arr_dt), but investigation showed disch_date_time
##     records the date a patient left the WHOLE HOSPITAL for admitted
##     patients (often weeks later), not when they left the ED — so this
##     was measuring hospital-episode length, not ED LOS, for anyone
##     admitted (worst_los_discrepancy_records.csv: 98% of the largest
##     mismatches were "Admit" records, off by 100+ days). los_hours is now
##     defined from Encompass's own arrival_to_depart field (minutes),
##     which correctly measures ED-only time. The old recalculation is kept
##     as a diagnostic (los_hours_check) and, in falls_50plus, as
##     hospital_episode_hours — a distinct, separately interesting metric,
##     not a substitute for ED LOS. This affects the LOS histograms,
##     prolonged-stay figures, and Model A2 throughout.
##
## Changes from v1 to v3:
##   - Fixed column name bugs: despatch_code (not dispatch_code), 
##     hospital_attended (not hostpital_attended)
##   - Removed unnecessary date re-parsing — Arrv Date/Time, Date of Birth,
##     Date of Call, Time at Hospital, Time of Handover are ALL already
##     proper datetime (dttm) objects from read_excel(); re-parsing them via
##     text was unnecessary and risky
##   - REMOVED arrival_method as a linkage-eligibility filter (unreliable —
##     confirmed cases exist with a genuine incident number but incorrect
##     Arrival Method text, e.g. "Private"). Eligibility is now based purely
##     on having a usable Ambulance Incident Number.
##   - Added Care Setting as its OWN descriptive variable (NOT a conveyance
##     indicator — confirmed binary "Care Setting"/"Non-Care Setting",
##     describing the incident LOCATION type, not the outcome)
##   - Added sex-wise EDA (NIAS-wide, and ED-outcome-wise for the linked
##     cohort — the latter only possible now linkage exists, since
##     Encompass alone has no sex field)
##   - Model B1 (admission, ED+NIAS features) now includes Patient Sex
##
## Run top to bottom in ONE clean session (rm(list = ls()) first if
## re-running after debugging) — later parts depend on objects from earlier.
## ============================================================================

## ==== 0. Package setup + shared helpers =====================================
required_packages <- c("readxl","janitor","dplyr","tidyr","stringr","lubridate",
                       "ggplot2","scales","forcats","readr","randomForest","pROC","broom",
                       "glmnet","xgboost")
installed <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed)
if (length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, dependencies = TRUE)
} else {
  message("All required packages already installed.")
}
invisible(lapply(required_packages, function(pkg) library(pkg, character.only = TRUE)))
set.seed(42)

missing_tokens <- c("", " ", "NA", "N/A", "n/a", "not given", "Not Given",
                    "NOT GIVEN", "missing", "Missing", "MISSING",
                    "unknown", "Unknown", "UNKNOWN", "null", "NULL")

# Only reparses a column if it ISN'T already a proper date/datetime type —
# safe to apply everywhere, avoids the earlier redundant reparsing bug.
ensure_datetime <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "Date")) return(x)
  parse_date_time(as.character(x), orders = c("dmy HM","dmy HMS","ymd HMS","ymd HM","dmy","ymd"))
}


eval_binary_model <- function(probs, actual_labels, positive_label, model_name) {
  roc_obj <- pROC::roc(response = actual_labels, predictor = probs, levels = rev(levels(actual_labels)), quiet = TRUE)
  auc_val <- pROC::auc(roc_obj)
  pred_class <- factor(if_else(probs > 0.5, positive_label, levels(actual_labels)[levels(actual_labels) != positive_label]),
                       levels = levels(actual_labels))
  cm <- table(Predicted = pred_class, Actual = actual_labels)
  cat("\n---", model_name, "---\nAUC:", round(auc_val, 3), "| Accuracy:", percent(sum(diag(cm))/sum(cm)), "\n")
  print(cm)
  list(roc = roc_obj, auc = auc_val)
}

## ==== Low-number suppression (HSC Data Governance requirement) =============
## Statistical disclosure control for every EDA chart/table/export below: any
## cell (a count within a breakdown/category, e.g. by hospital, arrival
## method, sex, acuity, disposition) below N_SUPPRESS_THRESHOLD is displayed
## as "<" followed by the threshold (currently 10) rather than the true
## value. This is a DISPLAY-ONLY transformation
## — it is always applied AFTER percentages/rates are calculated from the
## real count, never before, so pct figures remain accurate. It is NOT
## applied to headline population totals (e.g. "Linked records: n"), which
## standard SDC practice does not require suppressing — only to the
## subgroup/category breakdowns beneath them. Sheet-collation and linkage
## validity-check counts (Parts 1 and 6) are deliberately left unsuppressed,
## since those exist specifically to be cross-checked exactly against source
## file row counts.
N_SUPPRESS_THRESHOLD <- 10

## ---- Complementary (secondary) suppression --------------------------------
## Every category breakdown in this analysis sums to a known total (the
## group is 100% of something). That means if exactly ONE category is
## suppressed, its exact value can be back-calculated by subtracting every
## other (visible) category from that known total — the suppression would
## be cosmetic, not real. Both suppress_n() and suppress_pct() below now
## check for this automatically: whenever exactly one value in the group
## falls below threshold, the next-smallest VISIBLE value is suppressed too,
## so the first suppressed cell can no longer be deduced by arithmetic. If
## two or more cells are already below threshold, nothing further happens —
## subtraction can no longer isolate a single value.
##
## No call sites elsewhere in this script need to change for this: every
## existing suppress_n(n) / suppress_pct(n, pct) call already passes the
## full n column for its table (or, for a grouped tibble, the full column
## for that group via dplyr's per-group evaluation), which is exactly the
## vector this check needs.
##
## Residual-risk note for governance: this uses the standard "suppress the
## next-smallest visible cell" heuristic. If the two suppressed cells end up
## both small (e.g. a suppressed 5 and a suppressed 8 summing to a
## known 13), the true pair can still be narrowed to a short list of
## possibilities from the known partial sum, even though neither exact
## value is stated. Flag to HSC Data Governance if a stricter rule (e.g.
## always suppressing a larger secondary cell) is required for any specific
## table.
.complementary_mask <- function(n, threshold = N_SUPPRESS_THRESHOLD) {
  below <- n < threshold
  if (sum(below, na.rm = TRUE) == 1) {
    candidates <- which(!below)
    if (length(candidates) > 0) {
      second <- candidates[which.min(n[candidates])]
      below[second] <- TRUE
    }
  }
  below
}

suppress_n <- function(n, threshold = N_SUPPRESS_THRESHOLD) {
  mask <- .complementary_mask(n, threshold)
  if_else(mask, paste0("<", threshold), comma(n))
}

suppress_pct <- function(n, pct, threshold = N_SUPPRESS_THRESHOLD) {
  mask <- .complementary_mask(n, threshold)
  if_else(mask, "N/A", pct)
}

## ---- Visual (plotted-value) suppression -----------------------------------
## suppress_n()/suppress_pct() above only ever mask the PRINTED TEXT label —
## the actual value driving a bar's length (or a tile's fill shade) was
## still the true count, meaning two categories both labelled "<10" could
## still be visually compared by eye (a reader can see one bar is longer
## than the other even if neither number is printed). Per HSC Data
## Governance: "all bars/gradients for a small count should look the same
## in your graphs... this includes counts of 0." These two helpers replace
## the PLOTTED value (never the label, and never a real n used in pct/rate
## calculations, which must stay accurate) with a fixed, uniform stand-in
## for every suppressed category, so suppressed bars render identically
## regardless of true magnitude.
##
## Applies to count-based bar charts (geom_col with y = n). NOT applied to
## the hour x day-of-week heatmap by explicit decision — continuous
## gradient-cell suppression was assessed separately and judged unnecessary
## for that chart.
suppress_n_for_plot <- function(n, threshold = N_SUPPRESS_THRESHOLD) {
  mask <- .complementary_mask(n, threshold)
  uniform_height <- threshold * 0.6   # fixed nominal bar length for every suppressed cell
  if_else(mask, uniform_height, as.numeric(n))
}

## For RATE/percentage bar charts (e.g. admission rate by hospital), the bar
## encodes a rate, not a count — "uniform bar length" instead means forcing
## every suppressed category's bar to a fixed nominal rate, so its true rate
## can't be read off the chart (which, combined with a volume figure shown
## elsewhere, could otherwise let a reader back into the suppressed count).
## FIX: nominal was 0.5 (50%) — this can EXCEED a real, disclosed hospital's
## true rate (e.g. Antrim at 39.8%), making a suppressed bar look longer
## than a genuine one and sort above it. A suppressed bar should always be
## unambiguously the shortest/lowest on the chart, regardless of what the
## true rate happens to be, so nominal is now a small fixed value near zero
## instead. The label still shows "<10 (N/A)", so the bar's near-zero
## length is never read as "close to 0% admitted" — it's a visual "this is
## suppressed" marker, not a plotted value.
suppress_rate_for_plot <- function(n, rate, threshold = N_SUPPRESS_THRESHOLD, nominal = 0.02) {
  mask <- .complementary_mask(n, threshold)
  if_else(mask, nominal, as.numeric(rate))
}

# ============================================================================
# PART 1 — Encompass ED import, cleaning, completeness audit
# ============================================================================
ed_file_path <- "ED_Arrival_encompass_data_COLLATED.xlsx"   # <-- update path

raw <- read_excel(ed_file_path, sheet = 1, guess_max = 100000) %>% clean_names()
cat("Encompass rows read:", nrow(raw), " | Columns:", ncol(raw), "\n")

# Fix the duplicate "Arrival Method" columns — column 7 confirmed as the
# real one to use.
raw <- raw %>% rename(arrival_method = arrival_method_15)
raw <- raw %>% rename(arrival_method_unreliable = arrival_method_7)

raw_clean <- raw %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), ""))) %>%
  mutate(across(where(is.character), ~ ifelse(.x %in% missing_tokens, NA, .x)))

completeness_full_ED <- raw_clean %>%
  summarise(across(everything(), ~ mean(!is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "field", values_to = "pct_complete") %>%
  arrange(pct_complete) %>%
  mutate(pct_complete_lab = percent(pct_complete, accuracy = 0.1))
write_csv(completeness_full_ED, "Full_ED_completeness_audit.csv")

p_completeness_full_ED <- completeness_full_ED %>%
  mutate(field = fct_reorder(field, pct_complete)) %>%
  ggplot(aes(x = field, y = pct_complete)) +
  geom_col(fill = "#028090") + geom_text(aes(label = pct_complete_lab), hjust = -0.1, size = 3) +
  coord_flip(clip = "off") + scale_y_continuous(labels = percent, limits = c(0, 1.05)) +
  labs(title = "Field completeness — Encompass Full ED extract",
       subtitle = paste0("n = ", nrow(raw_clean), " records"), x = NULL, y = "% non-missing") +
  theme_minimal(base_size = 12)
ggsave("Full_ED_completeness_by_field.png", p_completeness_full_ED, width = 9, height = 6, dpi = 150)
cat("NOTE: the completeness % above is STRUCTURAL completeness (blank vs.\n",
    "non-blank) only. For ambulance_incident_number specifically, a non-blank\n",
    "value is NOT the same as a USABLE value for linkage — see the validity\n",
    "check in Part 6, which found some 'present' values are unusable junk\n",
    "(e.g. letters, wrong length). Do not quote the completeness % below as a\n",
    "linkage-usability figure without checking Part 6's output.\n")

# Dates are ALREADY proper datetimes from read_excel() — ensure_datetime()
# just renames them to convenient working names, reparsing only if needed.
raw_clean <- raw_clean %>%
  mutate(arr_dt = ensure_datetime(arrv_date_time), disch_dt = ensure_datetime(disch_date_time),
         dob = ensure_datetime(date_of_birth))

cat("PART 1 complete. arr_dt class:", class(raw_clean$arr_dt)[1], "\n\n")

# ============================================================================
# PART 2 — Trust/Hospital derivation, Age (AT ED ARRIVAL), general overview
# ============================================================================
dep_parts <- str_split_fixed(raw_clean$arrival_dep, "\\s+", n = 3)
raw_clean <- raw_clean %>%
  mutate(trust_code = dep_parts[, 1], hospital_code = dep_parts[, 2], department = dep_parts[, 3])

## Both "SET" and "SE" are mapped to South Eastern Trust — the actual code
## appearing in arrival_dep for this Trust was found to be
## "SE", not "SET" as originally assumed. Both are kept so this survives
## either code appearing in a future extract.
trust_lookup <- c("BT"="Belfast Trust","NT"="Northern Trust","SET"="South Eastern Trust","SE"="South Eastern Trust","ST"="Southern Trust","WT"="Western Trust")
hospital_lookup <- c("RVH"="Royal Victoria Hospital","MIU"="Minor Injuries Unit","AAH"="Antrim Area Hospital", "UH" = "Ulster Hospital",
                     "CAH"="Craigavan Area Hospital", "ALT"="Altnagelvin Area Hospital", "DHH"="Daisy Hill Hospital",
                     "MIH"="Mater Hospital", "SWA"="South West Acute Hospital", "DWN"="Downe Hospital")

raw_clean <- raw_clean %>%
  mutate(trust_name    = recode(trust_code, !!!trust_lookup, .default = trust_code),
         hospital_name = recode(hospital_code, !!!hospital_lookup, .default = hospital_code))

## UNMAPPED-CODE AUDIT — catches this class of bug automatically from now
## on: any trust_code or hospital_code that fell through to its raw .default
## value (i.e. wasn't in the lookup) gets flagged here, so a mismatch shows
## up as a warning in the console instead of silently appearing as a raw
## code on a chart meant for governance/client review.
unmapped_trust_codes <- raw_clean %>% filter(!trust_code %in% names(trust_lookup)) %>%
  distinct(trust_code) %>% pull(trust_code)
if (length(unmapped_trust_codes) > 0) {
  cat("WARNING: trust_code value(s) not found in trust_lookup — displaying as raw code:\n ",
      paste(unmapped_trust_codes, collapse = ", "), "\n",
      "Add these to trust_lookup above if they are genuine Trust codes.\n")
} else {
  cat("Trust code lookup check: all trust_code values mapped to a full name.\n")
}
unmapped_hospital_codes <- raw_clean %>% filter(!hospital_code %in% names(hospital_lookup)) %>%
  distinct(hospital_code) %>% pull(hospital_code)
if (length(unmapped_hospital_codes) > 0) {
  cat("NOTE: hospital_code value(s) not found in hospital_lookup — displaying as raw code:\n ",
      paste(unmapped_hospital_codes, collapse = ", "), "\n",
      "This is expected if hospital_lookup only lists a subset of hospitals so far;\n",
      "add any that should be spelled out in full.\n")
}

## AGE — explicitly at ED arrival (arr_dt), not today, not any other timestamp.
raw_clean <- raw_clean %>%
  mutate(
    age_years_exact_at_arrival = as.numeric(difftime(arr_dt, dob, units = "days")) / 365.25,
    age_years_at_arrival       = floor(age_years_exact_at_arrival),
    age_flag_implausible       = age_years_exact_at_arrival < 0 | age_years_exact_at_arrival > 125
  ) %>%
  relocate(age_years_at_arrival, age_years_exact_at_arrival, .after = dob)

raw_clean <- raw_clean %>%
  mutate(age_band = case_when(
    age_flag_implausible               ~ "Implausible/Unknown",
    is.na(age_years_exact_at_arrival)  ~ "Missing",
    age_years_exact_at_arrival < 50    ~ "<50",
    age_years_exact_at_arrival < 65    ~ "50-64",
    age_years_exact_at_arrival < 75    ~ "65-74",
    age_years_exact_at_arrival < 85    ~ "75-84",
    TRUE                                ~ "85+"
  ))

## ==== Under-50 vs 50+ split — FULL Encompass extract, ALL reasons/ages ====
## NOT the falls cohort — this is every ED attendance in the raw extract,
## split simply by whether the patient was under 50 or 50+ at arrival.
## Implausible/missing ages are kept in their own row rather than silently
## dropped, so the three counts always sum to nrow(raw_clean).
age_50plus_split_all_ed <- raw_clean %>%
  mutate(age_50plus_group = case_when(
    age_flag_implausible | is.na(age_years_exact_at_arrival)  ~ "Implausible/Unknown",
    age_years_exact_at_arrival < 50                            ~ "<50",
    TRUE                                                        ~ "50+"
  )) %>%
  count(age_50plus_group, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
cat("\n==== FULL ENCOMPASS EXTRACT — Under 50 vs 50+ (all reasons, all attendances) ====\n")
print(age_50plus_split_all_ed %>% mutate(n = suppress_n(n)))
write_csv(age_50plus_split_all_ed %>% mutate(n = suppress_n(n)), "age_50plus_split_full_encompass.csv")

## Same population, full 5-band breakdown (<50, 50-64, 65-74, 75-84, 85+),
## for when you need more granularity than the binary split above.
age_band_split_all_ed <- raw_clean %>%
  count(age_band, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
cat("\nFull age-band breakdown, same population:\n")
print(age_band_split_all_ed %>% mutate(n = suppress_n(n)))
write_csv(age_band_split_all_ed %>% mutate(n = suppress_n(n)), "age_band_split_full_encompass.csv")

## Sample of records within each age band — especially the implausible
## ones — for spot-checking data-entry errors (e.g. DOB after arrival date,
## DOB far in the future/past) before trusting the band split. Exported to
## CSV rather than printed to console — contains DOB and arrival datetime,
## which is more identifying than an aggregate count.
age_band_samples <- raw_clean %>%
  group_by(age_band) %>%
  slice_head(n = 10) %>%
  ungroup() %>%
  select(age_band, dob, arr_dt, age_years_exact_at_arrival, age_flag_implausible)
write_csv(age_band_samples, "age_band_sample_records_for_review.csv")
cat("\nPer-age-band sample records (up to 10 per band) written to\n",
    "age_band_sample_records_for_review.csv for spot-checking — not printed\n",
    "to console (contains DOB and arrival datetime).\n")

## Implausible records specifically — full detail, not just a sample, since
## these are the ones needing manual review/correction.
implausible_age_records <- raw_clean %>%
  filter(age_flag_implausible) %>%
  select(dob, arr_dt, age_years_exact_at_arrival, age_band, ed_reason_for_attendance, ambulance_incident_number)
cat("\n==== ALL implausible-age records (n =", nrow(implausible_age_records), ") ====\n")
print(implausible_age_records, n = Inf)
write_csv(implausible_age_records, "implausible_age_records_for_review.csv")


p_age_50plus_all_ed <- age_50plus_split_all_ed %>%
  filter(age_50plus_group %in% c("<50", "50+")) %>%
  mutate(age_50plus_group = factor(age_50plus_group, levels = c("<50", "50+"))) %>%
  ggplot(aes(x = age_50plus_group, y = suppress_n_for_plot(n))) +
  geom_col(fill = "#028090") + geom_text(aes(label = suppress_n(n)), vjust = -0.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("All ED attendances — Under 50 vs 50+ (n = ", comma(nrow(raw_clean)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("age_50plus_split_full_encompass.png", p_age_50plus_all_ed, width = 6, height = 5, dpi = 150)

## ---- Hospital volume — CSV (n & pct both suppressed) then chart from it ----
hosp_volume_all <- raw_clean %>% count(hospital_name, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

hosp_volume_all_export <- hosp_volume_all %>%
  mutate(pct = suppress_pct(n, pct),   # must run on the real n, before n is overwritten
         n   = suppress_n(n))
write_csv(hosp_volume_all_export, "all_volume_by_hospital.csv")

p_hosp_all <- hosp_volume_all %>%
  ggplot(aes(x = fct_reorder(hospital_name, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#028090") +
  geom_text(aes(label = suppress_n(n)), hjust = -0.1) + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("All ED attendances — volume by Hospital (n = ", comma(sum(hosp_volume_all$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("all_volume_by_hospital.png", p_hosp_all, width = 9.5, height = 5, dpi = 150)

## ---- Acuity mix by Trust — CSV (n & pct both suppressed) then chart from it ----
## pct here is each acuity's SHARE WITHIN its Trust (matches the stacked-to-
## 100% chart), so it's computed per trust_name group, not over the grand total.
acuity_by_trust_all <- raw_clean %>%
  mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(trust_name, acuity_cat) %>%
  group_by(trust_name) %>%
  mutate(pct = percent(n / sum(n))) %>%
  ungroup()

acuity_by_trust_export <- acuity_by_trust_all %>%
  group_by(trust_name) %>%
  mutate(pct = suppress_pct(n, pct),
         n   = suppress_n(n)) %>%
  ungroup()
write_csv(acuity_by_trust_export, "acuity_by_trust.csv")

p_acuity_by_trust <- acuity_by_trust_all %>%
  group_by(trust_name) %>%
  mutate(n_for_plot = suppress_n_for_plot(n)) %>%
  ungroup() %>%
  ggplot(aes(x = trust_name, y = n_for_plot, fill = acuity_cat)) + geom_col(position = "fill") +
  scale_y_continuous(labels = percent) + coord_flip() +
  labs(title = paste0("Urgency (Acuity) mix by Trust (n = ", comma(sum(acuity_by_trust_all$n)), ")"), x = NULL, y = "Share", fill = "Acuity") + theme_minimal(base_size = 12)
ggsave("acuity_by_trust.png", p_acuity_by_trust, width = 8, height = 5, dpi = 150)

cat("PART 2 complete.\n\n")


# ============================================================================
# PART 3 — Falls cohort identification (TIERED keyword approach)
# ============================================================================
tier_a_direct <- c("\\bfall(s|en)?\\b","\\bfell\\b","\\bslipped\\b","\\bslip\\b","\\btripped\\b","\\btrip\\b",
                   "\\bmechanical fall\\b","\\bunwitnessed fall\\b","\\bwitnessed fall\\b",
                   "\\bfound on (the )?floor\\b","\\bfound on (the )?ground\\b",
                   "\\boff (a |the )?(ladder|chair|bed|stool|steps?)\\b",
                   "\\b\\?#\\s?nof\\b","\\b#nof\\b","\\bfractured neck of femur\\b",
                   "\\b#\\s?neck of femur\\b","\\bhip fracture\\b")
tier_b_associated <- c("\\bfaint(ed|ing)?\\b","\\bsyncop(e|al)\\b","\\bcollapse[d]?\\b","\\bdizz(y|iness)\\b",
                       "\\bblack\\s?out\\b","\\bloss of balance\\b","\\boff balance\\b","\\blightheaded(ness)?\\b")
pattern_a <- regex(paste(tier_a_direct, collapse = "|"), ignore_case = TRUE)
pattern_b <- regex(paste(tier_b_associated, collapse = "|"), ignore_case = TRUE)

raw_clean <- raw_clean %>%
  mutate(
    text_combined = paste(coalesce(ed_reason_for_attendance, ""), coalesce(ed_dx, ""), coalesce(comments, "")),
    matched_direct     = str_detect(text_combined, pattern_a),
    matched_associated = str_detect(text_combined, pattern_b),
    fall_category = case_when(
      matched_direct                       ~ "Direct fall mention",
      matched_associated & !matched_direct ~ "Possible fall (faint/dizzy/collapse) — needs review",
      TRUE                                  ~ "Not fall-related"
    ),
    is_fall_flag = matched_direct   # official cohort flag — direct matches only
  )

## NOTE: this table is ALL AGES, ALL Encompass ED attendances — it exists
## only to show what share of the WHOLE ED dataset falls into each
## fall-category tier, as context. It is NOT the study cohort and should
## NEVER be quoted as "the falls cohort" number in a report — that is
## category_counts_50plus (Section 14) or falls_50plus (this Part, below).
category_counts <- raw_clean %>% count(fall_category, sort = TRUE) %>% mutate(pct = percent(n / sum(n)))
category_counts <- category_counts %>%
  mutate(pct = suppress_pct(n, pct),   # real n, before n is overwritten
         n   = suppress_n(n))
cat("\n[CONTEXT ONLY — ALL AGES, NOT the study cohort] Fall-category tier breakdown,\n",
    "whole Encompass extract:\n")
print(category_counts)
write_csv(category_counts, "falls_category_counts_ALL_AGES_context_only.csv")

falls_50plus <- raw_clean %>% filter(is_fall_flag, age_years_exact_at_arrival >= 50, !age_flag_implausible)
cat("Falls cohort (Direct mentions, 50+):", nrow(falls_50plus), "records\n")
cat("PART 3 complete.\n\n")

# ============================================================================
# PART 4 — Falls-cohort EDA
# ============================================================================
arrival_method_falls50plus <- falls_50plus %>% count(arrival_method, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

arrival_method_falls50plus_export <- arrival_method_falls50plus %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(arrival_method_falls50plus_export, "arrival_method_falls50plus_beforeLinkage.csv")

## Individual-record detail — INTERNAL QA/review only, NOT for the governance
## report: record-level listings aren't protected by count suppression the
## way aggregates are, and hcn is a direct patient identifier.
arrival_method_falls50plus_records <- falls_50plus %>%
  select(hcn, arr_dt, arrival_method, hospital_name, trust_name, acuity, dispo) %>%
  arrange(arrival_method, arr_dt)
write_csv(arrival_method_falls50plus_records, "arrival_method_falls50plus_beforeLinkage_records.csv")

## Collapse all "Emergency Road Ambulance <reference code>" variants into a
## single uniform label — the trailing code fragments the category into many
## near-duplicate values in the raw field.
arrival_method_falls50plus_grouped <- arrival_method_falls50plus %>%
  # Collapse every "Emergency Road Ambulance ..." PRF sub-code into one label
  mutate(arrival_method = if_else(str_detect(arrival_method, "^Emergency Road Ambulance"),
                                  "Emergency Road Ambulance", arrival_method)) %>%
  group_by(arrival_method) %>%
  summarise(n = sum(n), .groups = "drop") %>%   # SUM the real counts, don't re-count rows
  # Lump anything still under threshold (after the ambulance collapse) into "Others"
  mutate(arrival_method = if_else(n < N_SUPPRESS_THRESHOLD, "Others", arrival_method)) %>%
  group_by(arrival_method) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  mutate(pct = percent(n / sum(n)))   # recompute pct on the final grouped denominator

arrival_method_falls50plus_grouped_export <- arrival_method_falls50plus_grouped %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(arrival_method_falls50plus_grouped_export, "arrival_method_falls50plus_beforeLinkage_grouped.csv")

p_arrival_method_grouped <- arrival_method_falls50plus_grouped %>%
  ggplot(aes(x = fct_reorder(arrival_method, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#00A896") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Falls cohort (50+) — Arrival Method (n = ", comma(sum(arrival_method_falls50plus_grouped$n)), ")"), x = NULL, y = "Attendances") +
  theme_minimal(base_size = 12)
ggsave("grouped_arrival_method_falls50plus_beforeLinkage.png", p_arrival_method_grouped, width = 9.5, height = 5, dpi = 150)


repeat_attenders <- falls_50plus %>% filter(!is.na(hcn)) %>%
  count(hcn, name = "attendances_in_year") %>% count(attendances_in_year, name = "n_patients") %>%
  mutate(pct = percent(n_patients / sum(n_patients)))

repeat_attenders_export <- repeat_attenders %>%
  mutate(pct = suppress_pct(n_patients, pct), n_patients = suppress_n(n_patients))
print(repeat_attenders_export)
write_csv(repeat_attenders_export, "repeat_attenders_summary.csv")

## Individual-record detail — INTERNAL ONLY, contains hcn (direct patient
## identifier). 
repeat_attenders_records <- falls_50plus %>% filter(!is.na(hcn)) %>%
  count(hcn, name = "attendances_in_year") %>%
  arrange(desc(attendances_in_year))
write_csv(repeat_attenders_records, "repeat_attenders_records_by_hcn.csv")

p_repeat_attenders <- repeat_attenders %>%
  mutate(attendances_group = if_else(attendances_in_year >= 5, "5+", as.character(attendances_in_year))) %>%
  group_by(attendances_group) %>%
  summarise(n_patients = sum(n_patients), .groups = "drop") %>%
  mutate(pct = percent(n_patients / sum(n_patients)),
         attendances_group = factor(attendances_group, levels = c("1","2","3","4","5+"))) %>%
  ggplot(aes(x = attendances_group, y = suppress_n_for_plot(n_patients))) +
  geom_col(fill = "#01474F") +
  geom_text(aes(label = paste0(suppress_n(n_patients), " (", suppress_pct(n_patients, pct), ")")),
            vjust = -0.4, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Falls cohort (50+) — Attendances per patient in year (n = ", comma(sum(repeat_attenders$n_patients)), " patients)"),
       x = "Attendances in year", y = "Patients") +
  theme_minimal(base_size = 12)
ggsave("repeat_attenders_falls50plus.png", p_repeat_attenders, width = 7, height = 5, dpi = 150)


acuity_falls50plus <- falls_50plus %>% mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(acuity_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

acuity_falls50plus_export <- acuity_falls50plus %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(acuity_falls50plus_export, "acuity_distribution_falls50plus_beforeLinkage.csv")

## Individual-record detail — INTERNAL QA/review only, contains hcn.
acuity_falls50plus_records <- falls_50plus %>%
  mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  select(hcn, arr_dt, acuity_cat, hospital_name, trust_name, arrival_method, dispo) %>%
  arrange(acuity_cat, arr_dt)
write_csv(acuity_falls50plus_records, "acuity_distribution_falls50plus_beforeLinkage_records.csv")

p_acuity_falls <- acuity_falls50plus %>%
  ggplot(aes(x = fct_reorder(acuity_cat, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#02C39A") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Falls cohort (50+) — Acuity (n = ", comma(sum(acuity_falls50plus$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("acuity_distribution_falls50plus_beforeLinkage.png", p_acuity_falls, width = 7, height = 5, dpi = 150)

## ==== ED LENGTH OF STAY — using Encompass's own arrival_to_depart field ===
## IMPORTANT: los_hours here is NOT simply difftime(disch_dt, arr_dt).
## Investigation (worst_los_discrepancy_records.csv, Section 6) showed that
## disch_date_time records the date the patient left the WHOLE HOSPITAL
## episode for admitted patients (often weeks later), not when they left
## the ED — so difftime(disch_dt, arr_dt) measures hospital-stay length,
## not ED length of stay, for anyone admitted. Encompass's own
## arrival_to_depart field (minutes) measures ED-only time correctly (98%
## of the worst mismatches were "Admit" records where arrival_to_depart was
## a plausible few hours/days and the recalculated version was 100+ days).
## los_hours is therefore now DEFINED FROM arrival_to_depart. The old
## recalculation is kept as hospital_episode_hours — a different, genuinely
## interesting metric (total hospital stay following a fall) but NOT a
## substitute for ED length of stay.
falls_50plus <- falls_50plus %>%
  mutate(
    hospital_episode_hours     = as.numeric(difftime(disch_dt, arr_dt, units = "hours")),
    arrival_to_depart_numeric  = suppressWarnings(readr::parse_number(as.character(arrival_to_depart))),
    los_hours                  = arrival_to_depart_numeric / 60,  # source field is in MINUTES
    los_flag_implausible       = is.na(los_hours) | los_hours < 0 | los_hours > 24 * 14
  )

LOS_FOOTNOTE <- "ED-only time (Encompass 'Arrival to Depart' field) — see Methodology 3.6.6 / Limitations for validation"

p_los <- falls_50plus %>% filter(!los_flag_implausible, !is.na(los_hours)) %>%
  ggplot(aes(x = los_hours)) + geom_histogram(binwidth = 2, fill = "#028090", colour = "white") +
  labs(title = paste0("ED length of stay — falls cohort (50+) (n = ", comma(sum(!falls_50plus$los_flag_implausible & !is.na(falls_50plus$los_hours))), ")"), subtitle = LOS_FOOTNOTE,
       x = "LOS (hours)", y = "Attendances") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"))
ggsave("los_distribution_falls50plus_beforeLinkage.png", p_los, width = 8, height = 5, dpi = 150)

## ==== HOSPITAL EPISODE DURATION — a DISTINCT metric, Admit patients only ==
## hospital_episode_hours (= difftime(disch_dt, arr_dt)) is NOT ED length of
## stay for admitted patients — disch_date_time records eventual hospital
## discharge, not ED departure (see the fix above). Once correctly labelled,
## it's a genuinely useful secondary metric in its own right: total hospital
## episode duration following a fall, relevant to bed/rehab capacity
## planning. Scoped to Admit dispositions only, where the distinction from
## ED-only time actually applies (see the by-disposition validation:
## non-admitted categories showed ~0h discrepancy between the two methods).
hospital_episode_admit_only <- falls_50plus %>%
  filter(str_detect(tolower(coalesce(dispo, "")), "^admit$"),
         !is.na(hospital_episode_hours), hospital_episode_hours >= 0,
         hospital_episode_hours <= 24 * 90)  # 90-day sanity cap, generous for a hospital stay
cat("\n==== HOSPITAL EPISODE DURATION — Admitted falls patients (50+) ====\n")
cat("DISTINCT from ED length of stay above — this is total time from ED\n",
    "arrival to eventual hospital discharge, for patients who were admitted.\n")
hospital_episode_summary <- hospital_episode_admit_only %>%
  summarise(n = n(),
            median_days = round(median(hospital_episode_hours) / 24, 1),
            p90_days    = round(quantile(hospital_episode_hours, 0.9) / 24, 1),
            max_days    = round(max(hospital_episode_hours) / 24, 1))
print(hospital_episode_summary %>% mutate(n = suppress_n(n)))
write_csv(hospital_episode_summary %>% mutate(n = suppress_n(n)), "hospital_episode_duration_admit_only.csv")

## FIX: geom_histogram() auto-bins with no hook to suppress individual thin
## bins, and had no CSV backing it at all — meaning nobody could check
## whether any bar represented fewer than the suppression threshold. Bins
## are now computed manually so the same suppress_n_for_plot() treatment
## used everywhere else applies here too, and there's a real CSV to check
## exact bin counts against. Widened from 2-day to 5-day bins: with the
## long right tail this metric has, 2-day bins left many thin bars needing
## visual capping even after suppression; 5-day bins substantially reduce
## how often that happens while still showing the distribution's shape.
EPISODE_BINWIDTH_DAYS <- 5
hospital_episode_binned <- hospital_episode_admit_only %>%
  mutate(episode_days = hospital_episode_hours / 24,
         bin_start = floor(episode_days / EPISODE_BINWIDTH_DAYS) * EPISODE_BINWIDTH_DAYS,
         bin_label = paste0(bin_start, "-", bin_start + EPISODE_BINWIDTH_DAYS)) %>%
  count(bin_start, bin_label, name = "n") %>%
  arrange(bin_start) %>%
  mutate(pct = percent(n / sum(n)))

hospital_episode_binned_export <- hospital_episode_binned %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n)) %>%
  select(bin_label, n, pct)
write_csv(hospital_episode_binned_export, "hospital_episode_duration_admit_only_binned.csv")

p_hospital_episode <- hospital_episode_binned %>%
  mutate(n_for_plot = suppress_n_for_plot(n),
         bin_label = fct_reorder(bin_label, bin_start)) %>%
  ggplot(aes(x = bin_label, y = n_for_plot)) +
  geom_col(fill = "#5B7A82") +
  labs(title = paste0("Total hospital episode duration — Admitted falls patients (50+) (n = ", comma(nrow(hospital_episode_admit_only)), ")"),
       subtitle = "NOT ED length of stay — arrival to eventual hospital discharge, admitted patients only. Bars <10 patients capped.",
       x = "Days", y = "Patients") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
ggsave("hospital_episode_duration_admit_only.png", p_hospital_episode, width = 9, height = 5, dpi = 150)

## Individual-record breakdown — INTERNAL QA/REVIEW ONLY. Contains hcn (a
## direct patient identifier) and per-patient episode duration; do not
## include in the report or anything shared with governance — record-level
## detail isn't protected by count suppression the way the summary above is.
hospital_episode_admit_only_records <- hospital_episode_admit_only %>%
  mutate(hospital_episode_days = round(hospital_episode_hours / 24, 1)) %>%
  select(hcn, arr_dt, disch_dt, hospital_episode_days, hospital_name, trust_name, dispo, acuity) %>%
  arrange(desc(hospital_episode_days))
cat("\nAdmitted falls patients (50+) — individual episode-duration records (n =",
    nrow(hospital_episode_admit_only_records), "):\n")
write_csv(hospital_episode_admit_only_records, "hospital_episode_duration_admit_only_records.csv")


## ==== PROLONGED STAY — fixed thresholds (descriptive, NOT the model split) ==
## "Prolonged stay" was previously only defined INSIDE Model A2 (Part 9) as
## los_hours > median(los_hours) of that model's own training partition —
## a relative, re-shuffle-dependent split with no fixed meaning, and never
## exposed as a standalone EDA figure. This block adds a clear DESCRIPTIVE
## definition instead, using the two standard HSC/NHS ED performance breach
## thresholds (4-hour and 12-hour total time in ED) so "prolonged stay" means
## the same fixed thing everywhere it's reported. Change PROLONGED_LOS_HOURS_*
## below if a different threshold is wanted; every downstream figure follows.
PROLONGED_LOS_HOURS_STANDARD <- 4   # HSC/NHS ED 4-hour performance standard
PROLONGED_LOS_HOURS_LONGSTAY <- 12  # HSC/NHS ED 12-hour long-stay/breach measure

falls_50plus <- falls_50plus %>%
  mutate(prolonged_4h  = !los_flag_implausible & !is.na(los_hours) & los_hours > PROLONGED_LOS_HOURS_STANDARD,
         prolonged_12h = !los_flag_implausible & !is.na(los_hours) & los_hours > PROLONGED_LOS_HOURS_LONGSTAY)

prolonged_los_summary_falls50plus <- falls_50plus %>%
  filter(!los_flag_implausible, !is.na(los_hours)) %>%
  summarise(
    n_total          = n(),
    n_over_4h        = sum(prolonged_4h),
    pct_over_4h      = percent(mean(prolonged_4h)),
    n_over_12h       = sum(prolonged_12h),
    pct_over_12h     = percent(mean(prolonged_12h))
  )
cat("\n==== PROLONGED STAY — Falls cohort (Direct-tier keyword, 50+) ====\n")
cat("Definition: total ED time (arrival to depart) exceeding a fixed\n",
    "threshold — ", PROLONGED_LOS_HOURS_STANDARD, "h (HSC/NHS standard) and ",
    PROLONGED_LOS_HOURS_LONGSTAY, "h (long-stay/breach measure).\n", sep = "")
print(prolonged_los_summary_falls50plus %>%
        mutate(n_total = suppress_n(n_total), n_over_4h = suppress_n(n_over_4h), n_over_12h = suppress_n(n_over_12h)))
write_csv(prolonged_los_summary_falls50plus %>%
            mutate(n_total = suppress_n(n_total), n_over_4h = suppress_n(n_over_4h), n_over_12h = suppress_n(n_over_12h)),
          "prolonged_los_summary_falls50plus.csv")

## GOVERNANCE FIX: previously there were TWO versions of this chart — an
## unsuppressed one saved under the canonical filename, and a correctly
## suppressed one saved under a different "_suppressed" filename. That
## meant the "safe" filename anyone would expect to open was actually the
## unsuppressed one. Only the suppressed version is built now, and it owns
## the canonical filename.
dispo_falls50plus <- falls_50plus %>% mutate(dispo_cat = coalesce(dispo, "Missing")) %>%
  count(dispo_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

dispo_falls50plus_export <- dispo_falls50plus %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(dispo_falls50plus_export, "dispo_distribution_falls50plus_beforeLinkage.csv")

## Individual-record detail — INTERNAL QA/REVIEW ONLY, contains hcn.
dispo_falls50plus_records <- falls_50plus %>%
  mutate(dispo_cat = coalesce(dispo, "Missing")) %>%
  select(hcn, arr_dt, dispo_cat, hospital_name, trust_name, arrival_method, acuity) %>%
  arrange(dispo_cat, arr_dt)
write_csv(dispo_falls50plus_records, "dispo_distribution_falls50plus_beforeLinkage_records.csv")

p_dispo <- dispo_falls50plus %>%
  ggplot(aes(x = fct_reorder(dispo_cat, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#01474F") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Falls cohort (50+) — ED disposition (n = ", comma(sum(dispo_falls50plus$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("dispo_distribution_falls50plus_beforeLinkage.png", p_dispo, width = 8, height = 5, dpi = 150)


## Time-of-day x day-of-week heatmap. Suppression deliberately NOT applied
## to this chart (reviewed decision, distinct from every other categorical
## chart in this script) — see also the linked-cohort version in Section 15
## for direct before/after comparison.
falls_50plus <- falls_50plus %>%
  mutate(arr_month = floor_date(arr_dt, "month"), arr_wday = wday(arr_dt, label = TRUE, week_start = 1),
         arr_hour = hour(arr_dt))
p_heatmap <- falls_50plus %>% count(arr_wday, arr_hour) %>%
  ggplot(aes(x = arr_hour, y = arr_wday, fill = n)) + geom_tile() +
  scale_fill_gradient(low = "#F4F9F9", high = "#01474F") +
  labs(title = paste0("Falls cohort (50+) — hour x day of week (n = ", comma(nrow(falls_50plus)), ")"), x = "Hour", y = NULL, fill = "Attendances") + theme_minimal(base_size = 12)
ggsave("heatmap_hour_dow_falls50plus_beforeLinkage.png", p_heatmap, width = 9, height = 5.5, dpi = 150)

## ---- Trust admission rate — chart ----
trust_admission <- falls_50plus %>% mutate(admitted = str_detect(tolower(coalesce(dispo, "")), "admit")) %>%
  group_by(trust_name) %>%
  summarise(n = n(), admitted_rate = mean(admitted), .groups = "drop") %>%
  mutate(pct_admitted = percent(admitted_rate))

trust_admission_export <- trust_admission %>%
  mutate(pct_admitted = suppress_pct(n, pct_admitted), n = suppress_n(n)) %>%
  select(trust_name, n, pct_admitted)
write_csv(trust_admission_export, "trust_admission_summary.csv")

p_trust_admission <- trust_admission %>%
  ggplot(aes(x = fct_reorder(trust_name, admitted_rate), y = suppress_rate_for_plot(n, admitted_rate))) +
  geom_col(fill = "#028090") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct_admitted), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = percent) +
  labs(title = paste0("Falls cohort (50+) — Admission rate by Trust (n = ", comma(sum(trust_admission$n)), ")"), x = NULL, y = "Admitted (% of attendances)") +
  theme_minimal(base_size = 12)
ggsave("trust_admission_falls50plus.png", p_trust_admission, width = 8, height = 5, dpi = 150)

## ---- Hospital admission rate — chart ----
hospital_admission <- falls_50plus %>% mutate(admitted = str_detect(tolower(coalesce(dispo, "")), "admit")) %>%
  group_by(hospital_name) %>%
  summarise(n = n(), admitted_rate = mean(admitted), .groups = "drop") %>%
  mutate(pct_admitted = percent(admitted_rate))

hospital_admission_export <- hospital_admission %>%
  mutate(pct_admitted = suppress_pct(n, pct_admitted), n = suppress_n(n)) %>%
  select(hospital_name, n, pct_admitted)
write_csv(hospital_admission_export, "hospital_admission_summary.csv")

p_hospital_admission <- hospital_admission %>%
  ggplot(aes(x = fct_reorder(hospital_name, admitted_rate), y = suppress_rate_for_plot(n, admitted_rate))) +
  geom_col(fill = "#01474F") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct_admitted), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = percent) +
  labs(title = paste0("Falls cohort (50+) — Admission rate by Hospital (n = ", comma(sum(hospital_admission$n)), ")"), x = NULL, y = "Admitted (% of attendances)") +
  theme_minimal(base_size = 12)
ggsave("hospital_admission_falls50plus.png", p_hospital_admission, width = 8, height = 5, dpi = 150)
cat("PART 4 complete.\n\n")

# ============================================================================
# PART 5 — NIAS import, cleaning, Care Setting, Sex EDA
# ============================================================================
## IMPORTANT SCOPE NOTE (confirmed with NIAS): this extract was deliberately
## pulled as a CONVEYED-ONLY dataset — every incident in it resulted in
## conveyance to hospital by design, not because non-conveyance is rare in
## NIAS's full operational data. Do NOT infer a conveyance rate from this
## extract, and do NOT attempt to identify a "non-conveyed" subgroup within
## it — the entire extract is treated as conveyed. Any record with a blank
## Hospital Attended value (there was one in earlier review) is a data entry
## gap on an otherwise-conveyed incident, not a genuine non-conveyance, and
## is handled purely as a missing value below, not as its own category.

nias_file_path <- "DHSCNI_Falls_Data.xlsx"   # <-- update path

nias_raw <- read_excel(nias_file_path, sheet = 1, guess_max = 100000) %>% clean_names()
cat("NIAS rows read:", nrow(nias_raw), " | Columns:", ncol(nias_raw), "\n")

nias_clean <- nias_raw %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), ""))) %>%
  mutate(across(where(is.character), ~ ifelse(.x %in% missing_tokens, NA, .x)))

# Dates already proper datetimes — just rename, no reparsing needed.
nias_clean <- nias_clean %>%
  mutate(call_dt = ensure_datetime(date_of_call), at_hosp_dt = ensure_datetime(time_at_hospital),
         handover_dt = ensure_datetime(time_of_handover))
cat("call_dt class:", class(nias_clean$call_dt)[1], "\n")

# The ENTIRE extract is the conveyed population — no split, no filtering.
# `nias_conveyed` is kept as the working name (= nias_clean) purely so
# later code referencing it doesn't need to change.
nias_conveyed <- nias_clean
cat("NIAS incidents (all treated as conveyed, per confirmed extract scope):", nrow(nias_conveyed), "\n")
if (any(is.na(nias_clean$hospital_attended))) {
  cat("Note:", sum(is.na(nias_clean$hospital_attended)),
      "record(s) have a blank Hospital Attended value — treated as a data\n",
      "entry gap on a conveyed incident, not as non-conveyance.\n")
}

## Care Setting — its own descriptive dimension (institutional/residential
## care setting vs. not), a recognised risk factor in the falls literature.
care_setting_summary <- nias_clean %>% count(care_setting, sort = TRUE) %>% mutate(pct = percent(n / sum(n)))
care_setting_summary <- care_setting_summary %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
print(care_setting_summary)
write_csv(care_setting_summary, "nias_care_setting_summary.csv")

p_care_setting <- nias_clean %>% count(care_setting, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(care_setting, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#00A896") +
  geom_text(aes(label = suppress_n(n)), hjust = -0.1) + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("NIAS falls calls — Care Setting (n = ", comma(nrow(nias_clean %>% filter(!is.na(care_setting)))), ")"), x = NULL, y = "Incidents") + theme_minimal(base_size = 12)
ggsave("nias_care_setting.png", p_care_setting, width = 8.5, height = 4, dpi = 150)

## ==== Sex-wise EDA (NIAS-wide — descriptive, no linkage required) =========
p_sex_volume <- nias_clean %>% count(patient_sex, sort = TRUE) %>%
  ggplot(aes(x = fct_reorder(patient_sex, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#028090") +
  geom_text(aes(label = suppress_n(n)), hjust = -0.1) + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("NIAS falls calls — volume by Sex (n = ", comma(nrow(nias_clean)), ")"), x = NULL, y = "Incidents") + theme_minimal(base_size = 12)
ggsave("nias_sex_volume.png", p_sex_volume, width = 8.5, height = 4, dpi = 150)

p_sex_care_setting <- nias_clean %>% count(patient_sex, care_setting) %>%
  group_by(patient_sex) %>%
  mutate(n_for_plot = suppress_n_for_plot(n)) %>%
  ungroup() %>%
  ggplot(aes(x = patient_sex, y = n_for_plot, fill = care_setting)) + geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(title = paste0("Care Setting mix by Sex — NIAS falls calls (n = ", comma(nrow(nias_clean %>% filter(!is.na(care_setting)))), ")"), x = NULL, y = "Share", fill = "Care Setting") +
  theme_minimal(base_size = 12)
ggsave("nias_sex_by_care_setting.png", p_sex_care_setting, width = 7, height = 5, dpi = 150)

cat("PART 5 complete.\n\n")

# ============================================================================
# PART 6 — Type-safe linkage (NIAS <-> Encompass) — CORRECTED
# ============================================================================
to_id_string <- function(x) {
  if (is.numeric(x)) { x <- sprintf("%.0f", x) } else { x <- as.character(x) }
  x <- toupper(trimws(x))
  str_replace_all(x, "[^A-Z0-9]", "")
}

cat("Original type check — NIAS incident_number:", class(nias_clean$incident_number),
    "| Encompass ambulance_incident_number:", class(raw_clean$ambulance_incident_number), "\n")

nias_conveyed <- nias_conveyed %>% mutate(incident_number_norm = to_id_string(incident_number))

## AMBULANCE INCIDENT NUMBER — VALIDITY CHECK (data-driven, not a blocklist)
## Manual review found the Encompass field contains placeholder/junk values
## beyond simple blanks (e.g. "xxxxxxx", "64502jgk", "00", "w311") that a
## plain !is.na() check would wrongly count as "usable". Rather than
## maintaining a list of known-bad strings (which can never be complete —
## the next junk value won't be on it), validity is defined directly from
## what a REAL NIAS incident number actually looks like: pure digits, at
## one of the lengths genuinely observed in the NIAS extract. Any
## Encompass value containing a letter, or of an implausible length, can
## never match a NIAS record regardless of what it "means" locally, so it
## is treated as unusable for linkage.
nias_id_lengths <- unique(nchar(nias_conveyed$incident_number_norm))
cat("Valid NIAS Incident Number length(s) observed:", paste(nias_id_lengths, collapse = ", "), "\n")

raw_clean <- raw_clean %>%
  mutate(
    ambulance_incident_number_norm_check = to_id_string(ambulance_incident_number),
    ambulance_incident_number_valid_format =
      !is.na(ambulance_incident_number) &
      str_detect(ambulance_incident_number_norm_check, "^[0-9]+$") &
      nchar(ambulance_incident_number_norm_check) %in% nias_id_lengths
  )

n_present   <- sum(!is.na(raw_clean$ambulance_incident_number))
n_valid     <- sum(raw_clean$ambulance_incident_number_valid_format, na.rm = TRUE)
n_junk      <- n_present - n_valid
cat("Ambulance Incident Number — present (non-blank):", n_present,
    " | valid format (usable for linkage):", n_valid,
    " | present but INVALID format (junk, e.g. letters/wrong length):", n_junk,
    " (", percent(n_junk / n_present), "of 'present' values are actually unusable)\n")

junk_examples_all <- raw_clean %>%
  filter(!is.na(ambulance_incident_number), !ambulance_incident_number_valid_format) %>%
  distinct(ambulance_incident_number)
junk_examples <- junk_examples_all %>% slice_sample(n = min(15, nrow(junk_examples_all)))
# Sample no longer printed to console (raw field values) — spot-check via
# ambulance_incident_number_junk_values.csv instead.
write_csv(raw_clean %>% filter(!is.na(ambulance_incident_number), !ambulance_incident_number_valid_format) %>%
            count(ambulance_incident_number, sort = TRUE), "ambulance_incident_number_junk_values.csv")
cat("Junk AIN values written to ambulance_incident_number_junk_values.csv for spot-checking.\n")

## KEY FIX: eligibility now based on a VALID-FORMAT incident number, not
## just "non-blank" — arrival_method text is still NOT used as a filter
## (confirmed unreliable, Section 3.6.2).
encompass_for_linkage <- raw_clean %>%
  filter(age_years_exact_at_arrival >= 50, !age_flag_implausible, ambulance_incident_number_valid_format) %>%
  mutate(incident_number_norm = to_id_string(ambulance_incident_number))

cat("Encompass linkage-eligible (valid-format ID, age 50+):", nrow(encompass_for_linkage),
    "| NIAS conveyed:", nrow(nias_conveyed), "\n")

## ==== DUPLICATE-ID AUDIT — AIN (ED) and Incident Number (NIAS) ============
## inner_join() produces the CARTESIAN PRODUCT of matching rows for any key
## value that appears more than once on either side — so a duplicated ID
## here silently inflates nrow(linked) beyond a true one-to-one match count.
## This checks BOTH sides before the join, and distinguishes a genuine
## FULL-ROW duplicate (the identical record present twice) from an ID that
## repeats while other fields differ (two separate events sharing one ID —
## a data-quality issue needing manual review, not a simple duplicate).

# === ED side: Ambulance Incident Number (AIN) ===
ain_dupe_ids <- encompass_for_linkage %>%
  count(incident_number_norm, name = "n_rows") %>%
  filter(n_rows > 1)
cat("\n==== DUPLICATE AIN CHECK — ED (Encompass) linkage-eligible records ====\n")
cat("Distinct AIN values appearing more than once:", suppress_n(nrow(ain_dupe_ids)),
    "| Total rows involved:", suppress_n(sum(ain_dupe_ids$n_rows)), "\n")

ain_dupe_records <- encompass_for_linkage %>%
  filter(incident_number_norm %in% ain_dupe_ids$incident_number_norm) %>%
  arrange(incident_number_norm)
ain_full_row_dupes <- janitor::get_dupes(ain_dupe_records)

cat("Of those, FULL exact duplicate rows (every column matches):", suppress_n(nrow(ain_full_row_dupes)), "\n",
    "Rows where only the AIN repeats but other fields differ (separate\n",
    "records sharing one ID — needs manual review):",
    suppress_n(nrow(ain_dupe_records) - nrow(ain_full_row_dupes)), "\n")
write_csv(ain_dupe_records, "duplicate_AIN_records_ED.csv")

# === NIAS side: Incident Number ===
inc_dupe_ids <- nias_conveyed %>%
  count(incident_number_norm, name = "n_rows") %>%
  filter(n_rows > 1)
cat("\n==== DUPLICATE INCIDENT NUMBER CHECK — NIAS conveyed records ====\n")
cat("Distinct Incident Number values appearing more than once:", suppress_n(nrow(inc_dupe_ids)),
    "| Total rows involved:", suppress_n(sum(inc_dupe_ids$n_rows)), "\n")

inc_dupe_records <- nias_conveyed %>%
  filter(incident_number_norm %in% inc_dupe_ids$incident_number_norm) %>%
  arrange(incident_number_norm)
inc_full_row_dupes <- janitor::get_dupes(inc_dupe_records)

cat("Of those, FULL exact duplicate rows (every column matches):", suppress_n(nrow(inc_full_row_dupes)), "\n",
    "Rows where only the Incident Number repeats but other fields differ:",
    suppress_n(nrow(inc_dupe_records) - nrow(inc_full_row_dupes)), "\n")
write_csv(inc_dupe_records, "duplicate_incident_number_records_NIAS.csv")
cat("\nFull duplicate record lists saved to duplicate_AIN_records_ED.csv and\n",
    "duplicate_incident_number_records_NIAS.csv for manual review.\n")

linked <- encompass_for_linkage %>%
  inner_join(nias_conveyed, by = "incident_number_norm", suffix = c("_ed", "_nias"))

cat("\n==== LINKAGE RESULT ====\n")
cat("Linked:", nrow(linked), "of", nrow(encompass_for_linkage),
    "(", percent(nrow(linked)/nrow(encompass_for_linkage)), "of Encompass-eligible;",
    percent(nrow(linked)/nrow(nias_conveyed)), "of NIAS-conveyed)\n")
write_csv(linked, "linked_nias_encompass.csv")
cat("PART 6 complete.\n\n")

# ============================================================================
# PART 7 — Handover delay, system concordance, SEX-WISE ED OUTCOMES (linked)
# ============================================================================
linked <- linked %>%
  mutate(handover_delay_mins = as.numeric(difftime(handover_dt, at_hosp_dt, units = "mins")),
         handover_flag_implausible = handover_delay_mins < 0 | handover_delay_mins > 24 * 60)


## Same fix as the hospital-episode-duration histogram above — 10-minute
## bins preserved, but now suppressible per-bin and with a CSV backing it.
handover_raw_binned <- linked %>% filter(!handover_flag_implausible, !is.na(handover_delay_mins)) %>%
  mutate(bin_start = floor(handover_delay_mins / 10) * 10,
         bin_label = paste0(bin_start, "-", bin_start + 10)) %>%
  count(bin_start, bin_label, name = "n") %>%
  arrange(bin_start) %>%
  mutate(pct = percent(n / sum(n)))

handover_raw_binned_export <- handover_raw_binned %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n)) %>%
  select(bin_label, n, pct)
write_csv(handover_raw_binned_export, "handover_delay_falls50plus_afterLinkage_raw_binned.csv")

p_handover <- handover_raw_binned %>%
  mutate(n_for_plot = suppress_n_for_plot(n),
         bin_label = fct_reorder(bin_label, bin_start)) %>%
  ggplot(aes(x = bin_label, y = n_for_plot)) +
  geom_col(fill = "#028090") +
  geom_vline(xintercept = which(sort(unique(handover_raw_binned$bin_start)) == 30), linetype = "dashed", colour = "#E8A33D") +
  labs(title = paste0("Ambulance handover delay — linked falls cohort (50+) (n = ", comma(sum(!linked$handover_flag_implausible & !is.na(linked$handover_delay_mins))), ")"),
       subtitle = "Bars <10 incidents capped",
       x = "Minutes", y = "Incidents") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 6))
ggsave("handover_delay_falls50plus_afterLinkage.png", p_handover, width = 9, height = 5, dpi = 150)


## ---- Handover delay — binned/suppressed version (governance-safe) --------
## Same handover_delay_mins/handover_flag_implausible as above, but bucketed
## into coarse 60-minute bands so each bar represents a real category with a
## suppressible count, instead of a fine-grained histogram where individual
## tail bins can be as small as 1-3 incidents.
handover_breaks <- c(seq(0, 300, by = 60), Inf)
handover_labels <- c("0-60", "60-120", "120-180", "180-240", "240-300", "300+")

handover_delay_binned <- linked %>%
  filter(!handover_flag_implausible, !is.na(handover_delay_mins)) %>%
  mutate(handover_band = cut(handover_delay_mins, breaks = handover_breaks,
                             labels = handover_labels, right = FALSE, include.lowest = TRUE)) %>%
  count(handover_band, .drop = FALSE) %>%
  mutate(pct = percent(n / sum(n)))

handover_delay_binned_export <- handover_delay_binned %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(handover_delay_binned_export, "handover_delay_falls50plus_afterLinkage_binned.csv")

p_handover_binned <- handover_delay_binned %>%
  ggplot(aes(x = handover_band, y = suppress_n_for_plot(n))) +
  geom_col(fill = "#028090") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), vjust = -0.4, size = 3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", colour = "#E8A33D") +  # marks the 0-60 / 30-min-ref band
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Ambulance handover delay — linked falls cohort (50+) (n = ", comma(sum(handover_delay_binned$n)), ")"),
       x = "Delay band (minutes)", y = "Incidents") +
  theme_minimal(base_size = 12)
ggsave("handover_delay_falls50plus_afterLinkage_binned.png", p_handover_binned, width = 8, height = 5, dpi = 150)



linked <- linked %>% mutate(arrival_time_discrepancy_mins = as.numeric(difftime(arr_dt, at_hosp_dt, units = "mins")))
concordance_summary <- linked %>%
  summarise(n = n(), median_discrepancy_mins = median(arrival_time_discrepancy_mins, na.rm = TRUE),
            mean_abs_discrepancy_mins = round(mean(abs(arrival_time_discrepancy_mins), na.rm = TRUE), 1),
            pct_within_15min = percent(mean(abs(arrival_time_discrepancy_mins) <= 15, na.rm = TRUE)))
write_csv(concordance_summary, "system_concordance_summary.csv")

## SEX-WISE ED OUTCOMES — only possible now, using Patient Sex carried
## through from NIAS via the linkage (Encompass alone has no sex field).


## ---- Sex-wise admission rate — n & pct suppressed, CSV + labelled chart ----
sex_admission <- linked %>%
  mutate(admitted = str_detect(tolower(coalesce(dispo, "")), "admit")) %>%
  group_by(patient_sex) %>%
  summarise(n = n(), admitted_rate = mean(admitted), .groups = "drop") %>%
  mutate(pct_admitted = percent(admitted_rate))

sex_admission_export <- sex_admission %>%
  mutate(pct_admitted = suppress_pct(n, pct_admitted), n = suppress_n(n)) %>%
  select(patient_sex, n, pct_admitted)
write_csv(sex_admission_export, "sex_admission_rate_falls50plus_afterLinkage.csv")

p_sex_admission <- sex_admission %>%
  ggplot(aes(x = patient_sex, y = suppress_rate_for_plot(n, admitted_rate))) + geom_col(fill = "#01474F") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct_admitted), ")")), vjust = -0.4, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = percent) +
  labs(title = paste0("Admission rate by Sex — linked falls cohort (n = ", comma(sum(sex_admission$n)), ")"), x = NULL, y = "% admitted") + theme_minimal(base_size = 12)
ggsave("sex_admission_rate_falls50plus_afterLinkage.png", p_sex_admission, width = 6, height = 5, dpi = 150)

## ---- Sex-wise LOS — group n suppressed; whole box HIDDEN if n < threshold --
## A boxplot for a very small group can itself reveal near-individual values
## (min/max whiskers), so unlike a bar count this needs the underlying group
## filtered out entirely when small, not just its label swapped for "<10".
sex_los_data <- linked %>%
  mutate(los_hours = suppressWarnings(readr::parse_number(as.character(arrival_to_depart))) / 60) %>%
  filter(los_hours >= 0, los_hours <= 24 * 14)

sex_los_group_n <- sex_los_data %>% count(patient_sex, name = "n")
sex_los_suppressed_groups <- sex_los_group_n %>% filter(n < N_SUPPRESS_THRESHOLD) %>% pull(patient_sex)
if (length(sex_los_suppressed_groups) > 0) {
  cat("Sex-wise LOS boxplot: omitting", paste(sex_los_suppressed_groups, collapse = ", "),
      "— group n below", N_SUPPRESS_THRESHOLD, "\n")
}

## CSV export — the five-number summary that actually gets DRAWN (median,
## Q1, Q3, and the whisker bounds). Deliberately NOT exporting the raw
## min/max: since outlier points are hidden from the chart specifically
## because they're individual patients' values, exporting the true min/max
## to a CSV would quietly reintroduce that same disclosure risk in tabular
## form instead of visual form.
##
## Whiskers use the 5th/95th PERCENTILES, not the standard Tukey 1.5*IQR
## rule. For data bounded at zero and right-skewed (like LOS), Q1 - 1.5*IQR
## computes to a negative number, so a Tukey "whisker" collapses to the
## sample MINIMUM instead — a single patient's exact value, not an
## aggregate (this is what previously produced a "whisker low" of 0.1
## hours). 5th/95th percentiles are always a genuine aggregate given these
## group sizes (hundreds of patients underlie each cutoff).
sex_los_summary <- sex_los_data %>%
  filter(!patient_sex %in% sex_los_suppressed_groups) %>%
  group_by(patient_sex) %>%
  summarise(
    n            = n(),
    median_hrs   = round(median(los_hours, na.rm = TRUE), 1),
    q1_hrs       = round(quantile(los_hours, 0.25, na.rm = TRUE), 1),
    q3_hrs       = round(quantile(los_hours, 0.75, na.rm = TRUE), 1),
    whisker_low  = round(quantile(los_hours, 0.05, na.rm = TRUE), 1),
    whisker_high = round(quantile(los_hours, 0.95, na.rm = TRUE), 1),
    .groups = "drop"
  )
sex_los_summary_export <- sex_los_summary %>% mutate(n = suppress_n(n))
cat("\n==== ED length of stay by Sex — five-number summary (linked cohort) ====\n")
print(sex_los_summary_export)
write_csv(sex_los_summary_export, "sex_los_summary_falls50plus_afterLinkage.csv")

## Chart built from the same 5th/95th percentile bounds as the CSV above,
## via stat_summary with a custom function, rather than ggplot's default
## Tukey-rule boxplot.
p_sex_los <- sex_los_data %>%
  filter(!patient_sex %in% sex_los_suppressed_groups) %>%
  ggplot(aes(x = patient_sex, y = los_hours)) +
  stat_summary(fun.data = function(x) {
    data.frame(
      ymin = quantile(x, 0.05), lower = quantile(x, 0.25),
      middle = median(x), upper = quantile(x, 0.75), ymax = quantile(x, 0.95)
    )
  }, geom = "boxplot", fill = "#00A896") +
  geom_text(data = sex_los_group_n %>% filter(!patient_sex %in% sex_los_suppressed_groups),
            aes(x = patient_sex, y = -Inf, label = paste0("n=", suppress_n(n))),
            vjust = -0.5, size = 3, inherit.aes = FALSE) +
  labs(title = paste0("ED length of stay by Sex — linked falls cohort (n = ", comma(sum(sex_los_group_n$n[!sex_los_group_n$patient_sex %in% sex_los_suppressed_groups])), ")"), subtitle = LOS_FOOTNOTE,
       x = NULL, y = "LOS (hours), 5th\u201395th percentile whiskers") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"))
ggsave("sex_los_falls50plus_afterLinkage.png", p_sex_los, width = 6, height = 5, dpi = 150)

## ---- sex_acuity — add pct suppression (n suppression already existed) -----
sex_acuity <- linked %>% mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(patient_sex, acuity_cat) %>% group_by(patient_sex) %>% mutate(pct = percent(n / sum(n))) %>% ungroup()
sex_acuity <- sex_acuity %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(sex_acuity, "sex_acuity_linked.csv")

cat("PART 7 complete.\n\n")

# ============================================================================
# PART 8 — Datatype/timestamp consistency verification
# ============================================================================
cat("==== TYPE CONSISTENCY CHECK ====\n")
cat("ambulance_incident_number (should be UNCHANGED):", class(linked$ambulance_incident_number), "\n")
cat("incident_number (should be UNCHANGED):", class(linked$incident_number), "\n")
cat("arr_dt:", class(linked$arr_dt), "| at_hosp_dt:", class(linked$at_hosp_dt), "| handover_dt:", class(linked$handover_dt), "\n")
cat("patient_sex:", class(linked$patient_sex), "| care_setting:", class(linked$care_setting), "\n")
cat("PART 8 complete.\n\n")

# ============================================================================
# PART 9 — Predictive modelling
# ============================================================================
## FIX 1 (root cause): acuity_f now wraps fct_lump_min() BEFORE
## fct_explicit_na(), same as the standalone modelling script — this was
## missing here, which is the main reason rare acuity categories were not
## being grouped before modelling.
##
## FIX 2 (safety net): even with lumping, a 70/30 RANDOM split can still,
## by chance, put every single example of a rare category into the test
## set and none into training — the model then has literally never seen
## that category and errors when asked to score it. align_factor_levels()
## detects this after every split and converts any such "unseen" category
## to NA in the test set (so those specific rows are simply excluded from
## evaluation, rather than crashing the whole model).
##
## FIX 3 (safety net): every model fit + evaluate block is wrapped in
## tryCatch(), so if anything still goes wrong, the script prints a clear
## message and moves on to the next model rather than halting entirely —
## essential when compiling a full run.

align_factor_levels <- function(test_df, train_df) {
  for (col in names(test_df)) {
    if (is.factor(test_df[[col]]) && col %in% names(train_df)) {
      train_levels <- levels(droplevels(train_df[[col]]))
      test_df[[col]] <- factor(as.character(test_df[[col]]), levels = train_levels)
      # any category present in test but absent from train becomes NA here,
      # rather than causing predict() to error later
    }
  }
  test_df
}

## ==== Elastic net (glmnet) and gradient boosting (XGBoost) ================
## Adds two more modelling techniques alongside the existing logistic
## regression + random forest comparison, fitted on the SAME train/test
## split so all four techniques are directly comparable for each outcome.
## glmnet and xgboost both require numeric matrices rather than R's formula
## interface, so predictors are one-hot encoded via model.matrix(). Because
## align_factor_levels() has already forced every factor in the test set to
## share IDENTICAL levels with the training set (see above), model.matrix()
## is guaranteed to produce the same column structure for both — avoiding
## the classic "newdata has different columns" failure seen with Model A2's
## random forest.
fit_glmnet_xgboost <- function(train_df, test_df, response_col, positive_label, model_label) {
  train_df <- as.data.frame(train_df); test_df <- as.data.frame(test_df)
  y_train <- as.numeric(train_df[[response_col]] == positive_label)
  y_test_fac <- test_df[[response_col]]
  
  rhs  <- setdiff(names(train_df), response_col)
  form <- reformulate(rhs)
  x_train <- model.matrix(form, data = train_df)[, -1, drop = FALSE]  # drop intercept column
  x_test  <- model.matrix(form, data = test_df)[, -1, drop = FALSE]
  
  ## Elastic net: alpha = 0.5 blends LASSO (automatic feature selection) and
  ## ridge (handles correlated predictors) in equal measure; lambda chosen
  ## by 10-fold cross-validation rather than picked arbitrarily.
  cv_fit <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0.5, nfolds = 10)
  glmnet_probs <- as.numeric(predict(cv_fit, newx = x_test, s = "lambda.min", type = "response"))
  eval_binary_model(glmnet_probs, y_test_fac, positive_label, paste0(model_label, " (Elastic net)"))
  
  ## Gradient boosting: shallow trees (max_depth 4) with a conservative
  ## learning rate (eta 0.1) — deliberately restrained given the moderate
  ## sample sizes here, to avoid overfitting relative to the RF baseline.
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test)
  xgb_fit <- xgb.train(data = dtrain, nrounds = 200, max_depth = 4, eta = 0.1,
                       objective = "binary:logistic", eval_metric = "auc", verbose = 0)
  xgb_probs <- predict(xgb_fit, dtest)
  eval_binary_model(xgb_probs, y_test_fac, positive_label, paste0(model_label, " (XGBoost)"))
}

# Wraps a fit-and-evaluate block so a failure prints a message and lets
# the rest of the script continue, instead of stopping execution.
safe_run <- function(label, expr) {
  tryCatch(
    expr,
    error = function(e) {
      cat("\n!!! SKIPPED —", label, "failed with error:\n   ", conditionMessage(e), "\n")
      cat("    (Execution continues with the next model.)\n")
      NULL
    }
  )
}

model_data_a <- falls_50plus %>%
  mutate(
    age_band_f       = factor(age_band, levels = c("50-64","65-74","75-84","85+")),
    arrival_method_f = fct_lump_min(factor(arrival_method), min = 30),
    acuity_f         = fct_explicit_na(fct_lump_min(factor(acuity), min = 30), na_level = "Not yet assigned"),
    trust_name_f     = factor(trust_name),
    arr_hour_f       = arr_hour, arr_wday_f = factor(arr_wday, ordered = FALSE),
    arr_month_num    = lubridate::month(arr_month),
    admitted         = factor(if_else(str_detect(tolower(coalesce(dispo, "")), "admit"), "Admitted", "Not admitted"))
  )
if (all(c("Admitted","Not admitted") %in% levels(model_data_a$admitted))) {
  model_data_a <- model_data_a %>% mutate(admitted = relevel(admitted, ref = "Not admitted"))
}

## === Model A1: Admission (ED-only, full falls_50plus before linkage cohort) ===
safe_run("Model A1", {
  mA1 <- model_data_a %>% filter(!is.na(admitted)) %>%
    select(admitted, age_band_f, arrival_method_f, acuity_f, trust_name_f, arr_hour_f, arr_wday_f, arr_month_num) %>% na.omit()
  idxA1 <- sample(seq_len(nrow(mA1)), 0.7 * nrow(mA1))
  mA1_train <- mA1[idxA1, ]; mA1_test <- mA1[-idxA1, ]
  mA1_test  <- align_factor_levels(mA1_test, mA1_train) %>% na.omit()  # drop any row with an unseen-category NA
  
  mA1_logit <- glm(admitted ~ ., data = mA1_train, family = binomial)
  write_csv(tidy(mA1_logit, exponentiate = TRUE, conf.int = TRUE), "modelA1_admission_ed_only_odds_ratios.csv")
  mA1_rf <- randomForest(admitted ~ ., data = mA1_train, ntree = 500, importance = TRUE)
  eval_binary_model(predict(mA1_logit, mA1_test, type = "response"), mA1_test$admitted, "Admitted", "Model A1: Admission (ED-only, logit)")
  eval_binary_model(predict(mA1_rf, mA1_test, type = "prob")[, "Admitted"], mA1_test$admitted, "Admitted", "Model A1: Admission (ED-only, RF)")
  fit_glmnet_xgboost(mA1_train, mA1_test, "admitted", "Admitted", "Model A1: Admission (ED-only)")
})

## === Model B1: Admission (ED + NIAS + SEX, linked subset) ===
model_data_b <- linked %>%
  mutate(
    age_band_f       = factor(age_band, levels = c("50-64","65-74","75-84","85+")),
    acuity_f         = fct_explicit_na(fct_lump_min(factor(acuity), min = 30), na_level = "Not yet assigned"),
    trust_name_f     = factor(trust_name),
    care_setting_f   = factor(care_setting),
    patient_sex_f    = factor(patient_sex),                  # NEW — only available via linkage
    ## FIX: was fct_lump_min(min = 20), which only drops rare codes and does
    ## NOT cap the total number of surviving categories — NIAS despatch/AMPDS
    ## codes can exceed randomForest's hard 53-level ceiling ("Can not handle
    ## categorical predictors with more than 53 categories"). fct_lump_n()
    ## keeps the top 40 most common codes and lumps everything else into
    ## "Other", guaranteeing <=41 levels regardless of the data.
    despatch_code_f  = fct_lump_n(factor(despatch_code), n = 40, other_level = "Other"),
    handover_delay_mins = ifelse(handover_flag_implausible, NA, handover_delay_mins),
    admitted         = factor(if_else(str_detect(tolower(coalesce(dispo, "")), "admit"), "Admitted", "Not admitted"))
  )
# NOTE: conveyance_status is no longer included as a predictor — every
# record in this extract is conveyed by design (see Part 5), so it carries
# no information and was previously only appearing as a constant column.

mB1 <- model_data_b %>% filter(!is.na(admitted)) %>%
  select(admitted, age_band_f, acuity_f, trust_name_f, care_setting_f,
         patient_sex_f, despatch_code_f, handover_delay_mins) %>%
  na.omit()

if (nrow(mB1) < 30) {
  cat("Model Set B skipped: linked sample too small (n =", nrow(mB1), ").\n")
} else if (!all(c("Admitted","Not admitted") %in% levels(mB1$admitted))) {
  cat("Model Set B skipped: linked subset has only one outcome class present.\n")
} else {
  safe_run("Model B1", {
    mB1 <- mB1 %>% mutate(admitted = relevel(admitted, ref = "Not admitted"))
    idxB1 <- sample(seq_len(nrow(mB1)), 0.7 * nrow(mB1))
    mB1_train <- mB1[idxB1, ]; mB1_test <- mB1[-idxB1, ]
    mB1_test  <- align_factor_levels(mB1_test, mB1_train) %>% na.omit()
    
    mB1_logit <- glm(admitted ~ ., data = mB1_train, family = binomial)
    write_csv(tidy(mB1_logit, exponentiate = TRUE, conf.int = TRUE), "modelB1_admission_ed_nias_sex_odds_ratios.csv")
    mB1_rf <- randomForest(admitted ~ ., data = mB1_train, ntree = 500, importance = TRUE)
    eval_binary_model(predict(mB1_logit, mB1_test, type = "response"), mB1_test$admitted, "Admitted", "Model B1: Admission (ED+NIAS+Sex, logit)")
    eval_binary_model(predict(mB1_rf, mB1_test, type = "prob")[, "Admitted"], mB1_test$admitted, "Admitted", "Model B1: Admission (ED+NIAS+Sex, RF)")
    fit_glmnet_xgboost(mB1_train, mB1_test, "admitted", "Admitted", "Model B1: Admission (ED+NIAS+Sex)")
  })
}

## === Model A2: Prolonged LOS (ED-only) ===
safe_run("Model A2", {
  m2_data <- model_data_a %>% filter(!los_flag_implausible, !is.na(los_hours)) %>%
    select(los_hours, age_band_f, arrival_method_f, acuity_f, trust_name_f, arr_hour_f, arr_wday_f, arr_month_num) %>% na.omit()
  idx2 <- sample(seq_len(nrow(m2_data)), 0.7 * nrow(m2_data))
  m2_train <- m2_data[idx2, ]; m2_test <- m2_data[-idx2, ]
  m2_test  <- align_factor_levels(m2_test, m2_train) %>% na.omit()
  
  los_cutoff <- median(m2_train$los_hours)
  m2_train <- m2_train %>% mutate(prolonged = factor(if_else(los_hours > los_cutoff, "Prolonged", "Not prolonged")), prolonged = relevel(prolonged, ref = "Not prolonged"))
  m2_test  <- m2_test  %>% mutate(prolonged = factor(if_else(los_hours > los_cutoff, "Prolonged", "Not prolonged"), levels = levels(m2_train$prolonged)))
  ## FIX: randomForest's formula interface handles "y ~ . - x" exclusion
  ## inconsistently between fit and predict (a documented package quirk) —
  ## it can build a different effective predictor set at each call, which
  ## is what throws "Type of predictors in new data do not match that of
  ## the training data" at predict() time even though fit() succeeds.
  ## glm() handles "." - x" fine, so only the RF step needs los_hours
  ## physically dropped from the data rather than excluded via formula.
  m2_train_rf <- m2_train %>% select(-los_hours)
  m2_test_rf  <- m2_test  %>% select(-los_hours)
  m2_logit <- glm(prolonged ~ . - los_hours, data = m2_train, family = binomial)
  write_csv(tidy(m2_logit, exponentiate = TRUE, conf.int = TRUE), "modelA2_prolonged_los_odds_ratios.csv")
  m2_rf <- randomForest(prolonged ~ ., data = m2_train_rf, ntree = 500, importance = TRUE)
  eval_binary_model(predict(m2_logit, m2_test, type = "response"), m2_test$prolonged, "Prolonged", "Model A2: Prolonged LOS (logit)")
  eval_binary_model(predict(m2_rf, m2_test_rf, type = "prob")[, "Prolonged"], m2_test_rf$prolonged, "Prolonged", "Model A2: Prolonged LOS (RF)")
  ## Reuse the los_hours-free frames (see the RF fix above) — los_hours is
  ## how "prolonged" was defined, so including it here would leak the
  ## answer directly into the model rather than testing genuine predictors.
  fit_glmnet_xgboost(m2_train_rf, m2_test_rf, "prolonged", "Prolonged", "Model A2: Prolonged LOS")
})

## === Model A3: Repeat attendance (ED-only, first-attendance features) ===
safe_run("Model A3", {
  falls_hcn <- model_data_a %>% filter(!is.na(hcn))
  first_attendance <- falls_hcn %>% arrange(hcn, arr_dt) %>% group_by(hcn) %>%
    mutate(attendance_number = row_number(), total_attendances = n()) %>%
    filter(attendance_number == 1) %>% ungroup() %>%
    mutate(repeat_attender = factor(if_else(total_attendances > 1, "Repeat", "Single"), levels = c("Single", "Repeat")))
  m3_data <- first_attendance %>%
    select(repeat_attender, age_band_f, arrival_method_f, acuity_f, trust_name_f, arr_hour_f, arr_wday_f, arr_month_num) %>% na.omit()
  idx3 <- sample(seq_len(nrow(m3_data)), 0.7 * nrow(m3_data))
  m3_train <- m3_data[idx3, ]; m3_test <- m3_data[-idx3, ]
  m3_test  <- align_factor_levels(m3_test, m3_train) %>% na.omit()
  
  minority_n <- min(table(m3_train$repeat_attender))
  m3_train_balanced <- m3_train %>% group_by(repeat_attender) %>% slice_sample(n = minority_n) %>% ungroup()
  m3_logit <- glm(repeat_attender ~ ., data = m3_train_balanced, family = binomial)
  write_csv(tidy(m3_logit, exponentiate = TRUE, conf.int = TRUE), "modelA3_repeat_attendance_odds_ratios.csv")
  m3_rf <- randomForest(repeat_attender ~ ., data = m3_train_balanced, ntree = 500, importance = TRUE)
  eval_binary_model(predict(m3_logit, m3_test, type = "response"), m3_test$repeat_attender, "Repeat", "Model A3: Repeat attendance (logit)")
  eval_binary_model(predict(m3_rf, m3_test, type = "prob")[, "Repeat"], m3_test$repeat_attender, "Repeat", "Model A3: Repeat attendance (RF)")
  fit_glmnet_xgboost(m3_train_balanced, m3_test, "repeat_attender", "Repeat", "Model A3: Repeat attendance")
})

cat("PART 9 complete.\n\n")

# ============================================================================
# PART 10 — Final summary — POPULATION DEFINITIONS, made explicit
# ============================================================================
## Every number below is labelled with EXACTLY which population it refers
## to, to prevent the overall-vs-50+ confusion. Cross-check every figure
## used in a report/slide against this block specifically before presenting.
cat("\n==== POPULATION DEFINITIONS — CHECK THIS BEFORE REPORTING ANY NUMBER ====\n")
cat(sprintf("%-70s %10s\n", "Population", "n"))
cat(sprintf("%-70s %10s\n", "----------", "--"))
cat(sprintf("%-70s %10d\n", "Encompass: ALL ED attendances, all ages, all reasons", nrow(raw_clean)))
cat(sprintf("%-70s %10d\n", "Encompass: ALL fall-related attendances, ALL ages (Direct tier)", sum(raw_clean$is_fall_flag, na.rm = TRUE)))
cat(sprintf("%-70s %10d\n", "Encompass: Falls cohort, AGE 50+ ONLY (Direct tier) — the study cohort", nrow(falls_50plus)))
cat(sprintf("%-70s %10d\n", "Encompass: LINKAGE-ELIGIBLE pool (age 50+ AND VALID-FORMAT Incident Number", nrow(encompass_for_linkage)))
cat(sprintf("%-70s %10s\n", "  — NOT restricted by Arrival Method text, NOT restricted to falls)", ""))
cat(sprintf("%-70s %10d\n", "NIAS: ALL falls incidents in this extract (conveyed by design — Part 5)", nrow(nias_clean)))
cat(sprintf("%-70s %10d\n", "LINKED: confirmed matches between NIAS and Encompass", nrow(linked)))

cat("\nDEFINITION NOTE — the project now uses ONE single, standardised\n",
    "eligibility rule throughout: age 50+ (plausible) AND a usable Ambulance\n",
    "Incident Number. The Arrival Method free-text field is NOT used as an\n",
    "eligibility criterion anywhere, since it was found unreliable (Section\n",
    "3.6.2) — a genuinely ambulance-conveyed record can have an incorrect\n",
    "Arrival Method value (e.g. \"Private\") despite having a real Incident\n",
    "Number. The presence of a real Incident Number is treated as sufficient\n",
    "evidence of ambulance involvement on its own.\n")

cat("\n==== A SUBTLE POINT WORTH UNDERSTANDING BEFORE REPORTING 'linked' ====\n")
cat("Because the NIAS extract is falls-only BY DESIGN (Part 5), every one of\n",
    "the 12,212 'linked' records is a genuine NIAS-confirmed fall, REGARDLESS\n",
    "of whether Encompass's own free-text keyword search (is_fall_flag) also\n",
    "caught it. In other words, 'linked' does not need filtering by\n",
    "is_fall_flag to be a legitimate falls-only number — it already is one,\n",
    "via NIAS's classification rather than Encompass's.\n\n",
    "What IS useful to check is the reverse: of the 12,212 linked (= NIAS-\n",
    "confirmed fall) records, how many did Encompass's OWN keyword search ALSO\n",
    "independently flag as a fall? This is a genuine cross-validation of the\n",
    "keyword search's recall, specifically on a population known to be falls:\n")

falls_agreement <- linked %>% summarise(n = n(), pct_also_flagged_by_ed_keyword = percent(mean(is_fall_flag, na.rm = TRUE)))
print(falls_agreement)
cat("-> A low percentage here would suggest the ED-side keyword search (Part 3)\n",
    "   under-catches genuine falls. Worth checking whether the misses cluster\n",
    "   in a particular fall_category tier (e.g. the excluded 'Possible fall'\n",
    "   tier) rather than being spread randomly, which would point to a\n",
    "   correctable gap in the keyword list rather than a broader recall issue.\n")

## Record-level split behind falls_agreement: which linked (NIAS-confirmed)
## records DID vs did NOT also get caught by the ED keyword search.
## INTERNAL REVIEW ONLY — contains hcn (a direct patient identifier).

falls_agreement_records <- linked %>%
  mutate(agreement_status = if_else(is_fall_flag, "Caught by ED keyword search", "Missed by ED keyword search")) %>%
  select(agreement_status, hcn, ambulance_incident_number, incident_number, arr_dt,
         ed_reason_for_attendance, ed_dx, comments, despatch_code, care_setting, dispo) %>%
  arrange(agreement_status, arr_dt)

cat("\n==== falls_agreement record-level split ====\n")
print(falls_agreement_records %>% count(agreement_status) %>%
        mutate(pct = percent(n / sum(n))) %>%
        mutate(pct = suppress_pct(n, pct), n = suppress_n(n)))
write_csv(falls_agreement_records, "falls_agreement_records_all.csv")

## Same thing as two separate files, if that's easier to work with —
## the "missed" one is identical to linked_missed_by_keyword from earlier.
falls_agreement_caught  <- falls_agreement_records %>% filter(agreement_status == "Caught by ED keyword search")
falls_agreement_missed  <- falls_agreement_records %>% filter(agreement_status == "Missed by ED keyword search")
write_csv(falls_agreement_caught, "falls_agreement_caught_by_keyword.csv")
write_csv(falls_agreement_missed, "falls_agreement_missed_by_keyword.csv")

## ==== RECORD-LEVEL EVIDENCE: NIAS-confirmed falls the keyword search missed ====
## For stakeholder demonstration: specific, identifiable records where a
## genuine NIAS falls dispatch was successfully linked to this Encompass
## attendance, but the ED-side free-text keyword search (Part 3) did NOT
## flag it as a fall. This is the concrete evidence that linkage finds real
## falls the text-mining approach alone would have missed — useful to show
## a client one actual example rather than just an aggregate percentage.
linked_missed_by_keyword <- linked %>%
  filter(!is_fall_flag) %>%
  select(hcn, ambulance_incident_number, incident_number, arr_dt,
         ed_reason_for_attendance, ed_dx, comments, despatch_code, care_setting, dispo) %>%
  arrange(arr_dt)

cat("\n==== RECORDS: NIAS-confirmed fall, but MISSED by the ED keyword search ====\n")
cat("Count:", suppress_n(nrow(linked_missed_by_keyword)), "of", nrow(linked), "linked records\n")
write_csv(linked_missed_by_keyword, "linked_falls_missed_by_keyword_search.csv")

if (nrow(linked_missed_by_keyword) > 0) {
  cat("\nRecord-level examples are not printed to console (contains hcn and\n",
      "free-text ED fields). Full list of", suppress_n(nrow(linked_missed_by_keyword)), "such records saved to:\n",
      "linked_falls_missed_by_keyword_search.csv — open this and pick whichever\n",
      "example reads most clearly for a client presentation (e.g. one where the\n",
      "ED Reason for Attendance text obviously describes a fall in different\n",
      "words than the possible keyword list covers, such as a specific injury\n",
      "description with no explicit \"fell/slipped\" wording).\n")
} else {
  cat("None found — every linked record was already caught by the ED keyword\n",
      "search. This would be a strong result for the keyword search's recall\n",
      "specifically among NIAS-confirmed falls.\n")
}

cat("\n==== HEADLINE NUMBERS (re-verify each against the table above before using) ====\n")
cat("Falls cohort, age 50+, Encompass keyword search (Direct tier):", nrow(falls_50plus), "\n")
cat("Linked records (= NIAS-confirmed falls, age 50+):", nrow(linked), "\n")
cat("Median handover delay, mins (linked records):", median(linked$handover_delay_mins, na.rm = TRUE), "\n")
cat("Median handover delay (mins):", median(linked$handover_delay_mins, na.rm = TRUE), "\n")
cat("Sex breakdown, NIAS falls calls:\n"); print(nias_clean %>% count(patient_sex) %>% mutate(n = suppress_n(n)))
cat("Care Setting breakdown, NIAS falls calls:\n"); print(nias_clean %>% count(care_setting) %>% mutate(n = suppress_n(n)))

# ============================================================================
## ##***## SECTION 11: HCN-INDEPENDENT VERIFICATION OF THE FALLS COHORT ##***##
# ============================================================================
## CONTEXT: It is found that filtering by HCN might be silently excluding
## genuine fall-related records with a blank HCN. This section verifies
## that concern directly against the actual code, and scopes precisely
## WHERE (if anywhere) an HCN requirement affects results.
##
## FINDING (built into the check below, not assumed): the core falls
## classification (is_fall_flag / fall_category, Part 3) and the
## falls_50plus cohort itself NEVER filter on HCN at all — a blank-HCN
## record with fall-related keywords is fully included in falls_50plus
## already. HCN is only ever used DOWNSTREAM, specifically for tracking
## REPEAT attendances by the same patient (Part 4's repeat_attenders
## summary, and Model A3), because identifying "the same patient attended
## twice" is structurally impossible without SOME patient identifier.

cat("\n##***## SECTION 11: HCN-INDEPENDENT FALLS COHORT VERIFICATION ##***##\n")

# 11a. Confirm the core cohort size is IDENTICAL whether or not HCN is
# considered — i.e. prove falls_50plus never depended on HCN to begin with.
falls_50plus_no_hcn_dependency_check <- raw_clean %>%
  filter(is_fall_flag, age_years_exact_at_arrival >= 50, !age_flag_implausible)  # identical filter, HCN never mentioned

cat("falls_50plus (as originally defined):", nrow(falls_50plus), "\n")
cat("Same cohort, re-derived with no reference to HCN at all:", nrow(falls_50plus_no_hcn_dependency_check), "\n")
cat("Identical?", identical(nrow(falls_50plus), nrow(falls_50plus_no_hcn_dependency_check)), "\n")
cat("--> Confirms: blank-HCN records with fall-related keywords ARE already\n",
    "    included in falls_50plus. No HCN filter exists at the cohort-\n",
    "    definition stage.\n")

# 11b. Quantify the blank-HCN group WITHIN the falls cohort, to show the
# scale of what the manual review noticed.
hcn_blank_in_falls <- falls_50plus %>% summarise(
  n_total = n(),
  n_blank_hcn = sum(is.na(hcn)),
  pct_blank_hcn = percent(mean(is.na(hcn)))
)
cat("\nWithin falls_50plus, blank-HCN records:\n")
hcn_blank_in_falls_export <- hcn_blank_in_falls %>%
  mutate(pct_blank_hcn = suppress_pct(n_blank_hcn, pct_blank_hcn),
         n_blank_hcn    = suppress_n(n_blank_hcn))
  # n_total intentionally left as the real value — headline total (= nrow(falls_50plus)), not a subgroup
print(hcn_blank_in_falls_export)
write_csv(hcn_blank_in_falls_export, "hcn_blank_in_falls_summary.csv")

# 11c. Scope EXACTLY which existing analyses are affected by requiring a
# non-blank HCN (i.e. where losing blank-HCN records is a real, structural
# necessity, not an oversight) — repeat-attendance tracking only.
falls_with_hcn    <- falls_50plus %>% filter(!is.na(hcn))
falls_without_hcn <- falls_50plus %>% filter(is.na(hcn))
cat("\nFalls cohort WITH usable HCN (used for repeat-attendance tracking only):", suppress_n(nrow(falls_with_hcn)), "\n")
cat("Falls cohort WITHOUT usable HCN (cannot be checked for repeat attendance,\n",
    " but IS still included in every other falls_50plus analysis in this\n",
    " script — acuity, LOS, disposition, temporal patterns, etc.):", suppress_n(nrow(falls_without_hcn)), "\n")
write_csv(falls_without_hcn %>% select(hcn, ambulance_incident_number, arr_dt, ed_reason_for_attendance, dispo),
          "falls_blank_hcn_records.csv")
cat("--> Exported the blank-HCN fall records themselves to\n",
    "    falls_blank_hcn_records.csv so you can manually spot-check that\n",
    "    they do read as genuine falls, if you want to verify by eye.\n")


# ============================================================================
## ##***## SECTION 12: NEAR-MISS AIN CANDIDATES (e.g. LEADING ZEROS) ##***##
# ============================================================================
## CONTEXT: manual review spotted a plausible near-miss pattern — an
## Encompass AIN like "006254315" that may actually correspond to a NIAS
## incident number "6254315" (same number, different leading-zero
## padding). The existing Tier 1a exact-match logic (Part 6) would NOT
## catch this, since "006254315" and "6254315" are different strings even
## after normalisation. This section identifies CANDIDATE matches of this
## specific type among currently-unmatched records, WITHOUT auto-merging
## them into `linked` — consistent with this project's established
## approach of treating lower-confidence matches as material for manual
## review, not automatic acceptance (see Part 6 candidate-matching notes).

cat("\n##***## SECTION 12: NEAR-MISS AIN CANDIDATES (LEADING ZEROS ETC.) ##***##\n")

# Records currently unmatched on each side.
unmatched_encompass <- encompass_for_linkage %>%
  filter(!incident_number_norm %in% nias_conveyed$incident_number_norm)
unmatched_nias <- nias_conveyed %>%
  filter(!incident_number_norm %in% encompass_for_linkage$incident_number_norm)
cat("Currently unmatched — Encompass (linkage-eligible):", nrow(unmatched_encompass), "\n")
cat("Currently unmatched — NIAS (conveyed):", nrow(unmatched_nias), "\n")

## ---- Diagnosing the 12,212 vs 12,128 gap: duplicate AINs among the MATCHED set ----
## Both matched-side tallies (12,212 Encompass-side / 12,128 NIAS-side) come from
## filter(... %in% ...), which counts ROWS, not distinct AIN values — so if an AIN
## has 2+ rows on one side, every one of those rows gets counted. This checks
## exactly where that's happening.

matched_ain_values <- intersect(encompass_for_linkage$incident_number_norm,
                                nias_conveyed$incident_number_norm)
cat("Distinct AIN values that successfully match on both sides:", length(matched_ain_values), "\n")

# Rows-per-AIN, counted separately on each side, restricted to ONLY the
# AINs that actually matched (irrelevant duplicates elsewhere don't matter here).
ed_rows_per_matched_ain <- encompass_for_linkage %>%
  filter(incident_number_norm %in% matched_ain_values) %>%
  count(incident_number_norm, name = "n_ed_rows")

nias_rows_per_matched_ain <- nias_conveyed %>%
  filter(incident_number_norm %in% matched_ain_values) %>%
  count(incident_number_norm, name = "n_nias_rows")

cat("Total ED rows across matched AINs:  ", sum(ed_rows_per_matched_ain$n_ed_rows), "\n")
cat("Total NIAS rows across matched AINs:", sum(nias_rows_per_matched_ain$n_nias_rows), "\n")

# The AINs actually responsible for the gap — matched AINs with MORE than
# one ED row. This is the "same incident number, two ED rows, other fields
# differ" pattern that was already spotted manually — this confirms how many
# AINs that pattern applies to, and by how much each inflates the count.
ed_dupe_matched_ains <- ed_rows_per_matched_ain %>% filter(n_ed_rows > 1)
cat("\nMatched AINs with >1 ED row:", nrow(ed_dupe_matched_ains),
    "| extra ED rows contributed:", sum(ed_dupe_matched_ains$n_ed_rows - 1), "\n")

# Same check on the NIAS side, for completeness — confirms whether NIAS
# duplication is ALSO contributing (or whether NIAS is clean, as the
# 12,212 == nrow(linked) result suggests).
nias_dupe_matched_ains <- nias_rows_per_matched_ain %>% filter(n_nias_rows > 1)
cat("Matched AINs with >1 NIAS row:", nrow(nias_dupe_matched_ains),
    "| extra NIAS rows contributed:", sum(nias_dupe_matched_ains$n_nias_rows - 1), "\n")

# Sanity check: distinct matched AINs + extra ED-side duplicate rows should
# reconcile to 12,212; distinct matched AINs + extra NIAS-side duplicate
# rows should reconcile to 12,128.
cat("\nReconciliation check:\n")
cat("  Distinct matched AINs + extra ED rows  =",
    length(matched_ain_values) + sum(ed_dupe_matched_ains$n_ed_rows - 1), "(should be 12,212)\n")
cat("  Distinct matched AINs + extra NIAS rows =",
    length(matched_ain_values) + sum(nias_dupe_matched_ains$n_nias_rows - 1), "(should be 12,128)\n")

# Export the actual duplicate-AIN records among the MATCHED set for review —
# this is the specific subset that was already noticed by eye.
write_csv(
  encompass_for_linkage %>% filter(incident_number_norm %in% ed_dupe_matched_ains$incident_number_norm) %>%
    arrange(incident_number_norm),
  "matched_ain_ed_side_duplicates.csv"
)
## ==== RECONCILIATION: why linked + unmatched_nias != nrow(nias_conveyed) ==
## nrow(linked) counts JOINED ROWS, not unique matched NIAS incidents. If
## either side has a duplicated key, one NIAS row can produce MORE THAN ONE
## row in `linked` (matched against multiple ED rows, or vice versa), so
## linked + unmatched_nias will overshoot nrow(nias_conveyed) by exactly the
## number of "extra" rows the duplicate keys created — every NIAS record is
## still accounted for exactly once (matched or unmatched), just not 1:1.
n_matched_nias_rows_expected <- nrow(nias_conveyed) - nrow(unmatched_nias)
cat("\n==== RECONCILIATION: linked-row-count vs NIAS total ====\n")
cat("NIAS conveyed (total):", nrow(nias_conveyed), "\n")
cat("NIAS unmatched:", nrow(unmatched_nias), "\n")
cat("=> NIAS records that DID find at least one match:", n_matched_nias_rows_expected, "\n")
cat("nrow(linked) (joined ROWS, not unique NIAS records):", nrow(linked), "\n")
cat("Excess rows in linked beyond the matched-NIAS-record count:",
    nrow(linked) - n_matched_nias_rows_expected,
    "— should match the duplicate-key row counts printed in Section 6 above.\n")

## Explicit tie-out against Section 6's duplicate-AIN / duplicate-Incident-
## Number findings, rather than leaving "should match" as a manual check.
## IMPORTANT: this must measure duplication WITHIN `linked` itself (i.e.
## incident_number_norm values that appear more than once AFTER the join
## succeeded) — NOT the raw pre-join duplicate counts from Section 6, which
## include duplicate ED/NIAS rows that never matched anything and so never
## contributed a single row to `linked`. Counting those would overstate the
## explanation (as the first version of this check did: 462 pre-join ED
## duplicates vs an actual excess of only 84 — most of those 462 duplicate
## rows simply didn't find a match at all, rather than each multiplying).
excess_rows_actual <- nrow(linked) - n_matched_nias_rows_expected
linked_key_dupes <- linked %>% count(incident_number_norm, sort = TRUE) %>% filter(n > 1)
excess_rows_within_linked <- sum(linked_key_dupes$n) - nrow(linked_key_dupes)
cat("\n==== TIE-OUT: excess rows vs duplication WITHIN linked (post-join) ====\n")
cat("Excess rows in linked (from reconciliation above):", excess_rows_actual, "\n")
cat("incident_number_norm values appearing more than once IN linked:", suppress_n(nrow(linked_key_dupes)), "\n")
cat("Extra rows those duplicated keys contribute (post-join):", suppress_n(excess_rows_within_linked), "\n")
cat(if (excess_rows_actual == excess_rows_within_linked)
  "=> CONFIRMED: excess rows fully explained by keys duplicated within the joined result.\n"
  else
    "=> Still not a clean match — inspect linked_key_dupes directly.\n")
write_csv(linked %>% filter(incident_number_norm %in% linked_key_dupes$incident_number_norm) %>%
            arrange(incident_number_norm), "duplicate_key_records_within_linked.csv")

## ==== CLASSIFYING the duplicate-AIN records (manual review, confirmed
## every duplicate is a distinct row on other fields — none are simple
## full-row re-entries) — splits the 84 into four patterns:
##   (a) Same AIN + same HCN + same arrival DATE — most likely a genuine
##       near-duplicate record (re-entry, correction, or a system re-send).
##   (b) Same AIN + same HCN + DIFFERENT date — the same patient and the
##       same ambulance incident number used on two different dates. This
##       cannot be explained by a multi-casualty callout (that would need
##       different HCNs); it most plausibly reflects the AIN itself being
##       reused/misassigned across two unrelated attendances rather than a
##       genuine single incident — worth flagging to HSC data governance
##       as a possible source-system ID-reuse issue, not just noting.
##   (c) Same AIN + DIFFERENT HCN — As checked manually and found that—  one 
##       ambulance incident (e.g. a road traffic collision, or an assault/
##       domestic incident) genuinely involving multiple people, each
##       triaged as a separate ED patient but sharing one incident number.
##       Plausible, but this is descriptive evidence for a hypothesis, not
##       proof of cause — (for reporting.)
##   (d) HCN missing on one or both sides — can't be classified into (a)/
##       (b)/(c) with confidence; kept as its own category rather than
##       forced into one of the others.
dupe_ain_classified <- linked %>%
  filter(incident_number_norm %in% linked_key_dupes$incident_number_norm) %>%
  group_by(incident_number_norm) %>%
  mutate(
    n_distinct_hcn       = n_distinct(hcn, na.rm = TRUE),
    n_distinct_arr_date  = n_distinct(as.Date(arr_dt)),
    any_hcn_missing      = any(is.na(hcn))
  ) %>%
  ungroup() %>%
  mutate(dupe_pattern = case_when(
    any_hcn_missing                                  ~ "(d) HCN missing on one or both rows",
    n_distinct_hcn == 1 & n_distinct_arr_date == 1    ~ "(a) Same AIN + same HCN + same date",
    n_distinct_hcn == 1 & n_distinct_arr_date > 1     ~ "(b) Same AIN + same HCN + DIFFERENT date",
    n_distinct_hcn > 1                                ~ "(c) Same AIN + different HCN (possible multi-casualty)",
    TRUE                                              ~ "Unclassified"
  ))

dupe_ain_pattern_summary <- dupe_ain_classified %>%
  distinct(incident_number_norm, dupe_pattern) %>%
  count(dupe_pattern, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
cat("\n==== DUPLICATE-AIN PATTERN CLASSIFICATION — Linked cohort ====\n")
cat("Of the", suppress_n(nrow(linked_key_dupes)), "duplicated AIN values, split by HCN and date agreement:\n")
print(dupe_ain_pattern_summary %>% mutate(n = suppress_n(n)))
write_csv(dupe_ain_pattern_summary %>% mutate(n = suppress_n(n)), "duplicate_AIN_pattern_summary.csv")

## The specific pattern found — same AIN & same HCN but different
## dates — exported separately since it's the one most worth a governance
## conversation (the other patterns are either benign or a plausible
## multi-casualty explanation).
same_ain_hcn_different_date <- dupe_ain_classified %>%
  filter(dupe_pattern == "(b) Same AIN + same HCN + DIFFERENT date") %>%
  arrange(incident_number_norm, arr_dt)
cat("\nRecords with same AIN + same HCN but different dates:", suppress_n(nrow(same_ain_hcn_different_date)), "\n")
write_csv(same_ain_hcn_different_date, "duplicate_AIN_same_hcn_different_date.csv")

# Strip ANY leading zeros from the already-normalised ID and re-attempt
# a match on that stripped form. This specifically catches the pattern
# that was found (e.g. "006254315" vs "6254315") without loosening the match
# in any other way (letters, wrong lengths etc. are still excluded, since
# incident_number_norm already went through the digit-only validity check
# in Part 6 before reaching this point).
strip_leading_zeros <- function(x) sub("^0+", "", x)

unmatched_encompass <- unmatched_encompass %>%
  mutate(id_no_leading_zeros = strip_leading_zeros(incident_number_norm))
unmatched_nias <- unmatched_nias %>%
  mutate(id_no_leading_zeros = strip_leading_zeros(incident_number_norm))

candidate_leading_zero_matches <- unmatched_encompass %>%
  inner_join(unmatched_nias, by = "id_no_leading_zeros", suffix = c("_ed", "_nias"))

# Flag ambiguous cases (more than one possible match on either side) —
# these need a human decision, not an automatic pick.
candidate_leading_zero_matches <- candidate_leading_zero_matches %>%
  add_count(id_no_leading_zeros, name = "n_candidates_for_this_stripped_id") %>%
  mutate(ambiguous = n_candidates_for_this_stripped_id > 1)

cat("\nCandidate matches found via leading-zero stripping:", nrow(candidate_leading_zero_matches), "\n")
if (nrow(candidate_leading_zero_matches) > 0) {
  cat("  Unambiguous (1-to-1, safe to review individually):",
      sum(!candidate_leading_zero_matches$ambiguous), "\n")
  cat("  Ambiguous (multiple possible matches — needs manual judgement):",
      sum(candidate_leading_zero_matches$ambiguous), "\n")
  write_csv(candidate_leading_zero_matches, "candidate_matches_leading_zero_stripped.csv")
  cat("--> Exported to candidate_matches_leading_zero_stripped.csv. These are\n",
      "    NOT included in `linked` — review manually and re-run Part 6 with\n",
      "    an adjusted normalisation rule if you confirm this pattern is real\n",
      "    and widespread (rather than a one-off).\n")
} else {
  cat("--> No leading-zero-pattern candidates found among currently unmatched\n",
      "    records. The specific example that was found manually may be an\n",
      "    isolated case rather than a systematic issue — worth locating\n",
      "    that specific record directly to confirm either way.\n")
}


# ============================================================================
## ##***## SECTION 13: WHICH KEYWORD TIER DO NIAS-ONLY FALLS FALL INTO? ##***##
# ============================================================================
## CONTEXT: of the NIAS-confirmed falls (`linked`) that the Encompass
## keyword search did NOT catch as "Direct fall mention" (Section on
## record-level evidence, Part 10), this checks whether they instead fall
## into the "Possible fall — needs review" tier (i.e. the keyword search
## detected SOMETHING fall-adjacent, just not confidently) or the
## "Not fall-related" tier (i.e. the keyword search found no fall-related
## language at all, even indirectly).

cat("\n##***## SECTION 13: TIER BREAKDOWN OF NIAS-CONFIRMED FALLS ##***##\n")

# Overall tier breakdown across ALL linked (= NIAS-confirmed fall) records —
# most should be "Direct fall mention" if the keyword search performs well.
linked_tier_breakdown <- linked %>% count(fall_category, sort = TRUE) %>% mutate(pct = percent(n / sum(n)))
linked_tier_breakdown <- linked_tier_breakdown %>% mutate(n = suppress_n(n))
cat("Fall-category tier breakdown, ALL linked (NIAS-confirmed) records:\n")
print(linked_tier_breakdown)
write_csv(linked_tier_breakdown, "linked_records_tier_breakdown.csv")

# Specifically among linked records the DIRECT-tier keyword search missed
# (is_fall_flag == FALSE), which tier did they actually land in?
linked_missed_tier_breakdown <- linked %>%
  filter(!is_fall_flag) %>%
  count(fall_category, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
linked_missed_tier_breakdown <- linked_missed_tier_breakdown %>% mutate(n = suppress_n(n))
cat("\nOf NIAS-confirmed falls MISSED by the Direct-tier keyword search,\n",
    "breakdown by which tier they actually fell into:\n")
print(linked_missed_tier_breakdown)
write_csv(linked_missed_tier_breakdown, "linked_missed_tier_breakdown.csv")

cat("\n--> Read this as: 'Possible fall' rows here are cases the keyword\n",
    "    search partially caught (associated symptom language) but wasn't\n",
    "    confident enough to auto-include — a good story for a client\n",
    "    ('near misses the tiered approach was designed to flag for review').\n",
    "    'Not fall-related' rows here are cases the keyword search found NO\n",
    "    fall-related language in at all, despite NIAS confirming a genuine\n",
    "    fall — these are the ones worth showing as the clearest evidence\n",
    "    that linkage finds real cases text-mining alone would fully miss.\n")


# ============================================================================
## ##***## SECTION 14: CLARIFYING THE POPULATION IN falls_category_counts ##***##
# ============================================================================
## CONTEXT: falls_category_counts_ALL_AGES_context_only.csv (Part 3) is built directly from
## `raw_clean`, which is the FULL Encompass extract — ALL AGES, not
## restricted to 50+. It answers "of every ED attendance in the whole
## year, what fraction mention a fall?", not "...among the 50+ cohort".
## This section adds the AGE-50+-RESTRICTED equivalent alongside it, so
## both versions exist side by side and neither gets mistaken for the other.

cat("\n##***## SECTION 14: FALLS CATEGORY COUNTS — ALL AGES vs. 50+ ONLY ##***##\n")

cat("EXISTING falls_category_counts_ALL_AGES_context_only.csv (Part 3) — ALL AGES, n =", nrow(raw_clean), ":\n")
print(category_counts)

category_counts_50plus <- raw_clean %>%
  filter(age_years_exact_at_arrival >= 50, !age_flag_implausible) %>%
  count(fall_category, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
category_counts_50plus <- category_counts_50plus %>% mutate(n = suppress_n(n))
cat("\nNEW: same breakdown, AGE 50+ ONLY, n =",
    sum(raw_clean$age_years_exact_at_arrival >= 50 & !raw_clean$age_flag_implausible, na.rm = TRUE), ":\n")
print(category_counts_50plus)
write_csv(category_counts_50plus, "falls_category_counts_50plus_only.csv")

cat("\n--> Use falls_category_counts_ALL_AGES_context_only.csv (Part 3) when describing the WHOLE\n",
    "    ED population's fall-related share. Use\n",
    "    falls_category_counts_50plus_only.csv (this section) when\n",
    "    describing the fall-related share specifically WITHIN age\n",
    "    50+ study population. Do not use the two interchangeably in the\n",
    "    same sentence without saying which one is meant for.\n")

# ============================================================================
## ##***## SECTION 15: FULL EDA ON THE LINKED (POST-LINKAGE) COHORT ##***##
# ============================================================================
## Mirrors the "before linkage" EDA (Part 4) but computed on `linked`
## instead of `falls_50plus` — lets us directly compare whether the
## successfully-linked subset looks representative of the full falls
## cohort, or skewed in some way (e.g. by hospital, by acuity, by time).
## Same "_falls50plus_afterLinkage" naming convention as the existing
## post-linkage charts, for consistency.
##
## v2 changes: (1) low-number/percentage suppression applied consistently
## across all four analyses below, including Acuity which previously had
## none at all; (2) Arrival Method now collapses "Emergency Road Ambulance"
## PRF sub-codes into one label before lumping rare categories into
## "Others", matching the fix applied to the pre-linkage version of this
## chart; (3) a summary CSV (suppressed) and a record-level CSV (INTERNAL
## ONLY, contains hcn) exported for every analysis, not just charted.
cat("\n##***## SECTION 15: POST-LINKAGE EDA (linked cohort) ##***##\n")
cat("Linked cohort size for this EDA:", nrow(linked), "\n")

# ---- Age band (linked) ----
age_band_linked <- linked %>%
  count(age_band) %>%
  filter(!age_band %in% c("Implausible/Unknown", "Missing")) %>%
  mutate(age_band = factor(age_band, levels = c("<50","50-64","65-74","75-84","85+")),
         pct = percent(n / sum(n)))

age_band_linked_export <- age_band_linked %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(age_band_linked_export, "age_band_falls50plus_afterLinkage.csv")

age_band_linked_records <- linked %>%
  filter(!age_band %in% c("Implausible/Unknown", "Missing")) %>%
  select(hcn, arr_dt, age_band, hospital_name, trust_name, arrival_method, acuity, dispo) %>%
  arrange(age_band, arr_dt)
write_csv(age_band_linked_records, "age_band_falls50plus_afterLinkage_records.csv")

p_age_band_linked <- age_band_linked %>%
  ggplot(aes(x = age_band, y = suppress_n_for_plot(n))) +
  geom_col(fill = "#028090") + geom_text(aes(label = suppress_n(n)), vjust = -0.4) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Linked cohort — Age band (n = ", comma(sum(age_band_linked$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("age_band_falls50plus_afterLinkage.png", p_age_band_linked, width = 7, height = 5.5, dpi = 150)

# ---- Hospital volume (linked) ----
hosp_linked <- linked %>% count(hospital_name, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

hosp_linked_export <- hosp_linked %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(hosp_linked_export, "hospital_volume_falls50plus_afterLinkage.csv")

hosp_linked_records <- linked %>%
  select(hcn, arr_dt, hospital_name, trust_name, arrival_method, acuity, dispo) %>%
  arrange(hospital_name, arr_dt)
write_csv(hosp_linked_records, "hospital_volume_falls50plus_afterLinkage_records.csv")

p_hosp_linked <- hosp_linked %>%
  ggplot(aes(x = fct_reorder(hospital_name, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#028090") +
  geom_text(aes(label = suppress_n(n)), hjust = -0.1) + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Linked cohort — volume by Hospital (n = ", comma(sum(hosp_linked$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("hospital_volume_falls50plus_afterLinkage.png", p_hosp_linked, width = 9.5, height = 5, dpi = 150)

# ---- Arrival Method (linked) — PRF collapse + Others-lumping + suppression ----
arrival_method_linked <- linked %>% count(arrival_method, sort = TRUE) %>%
  # Collapse every "Emergency Road Ambulance ..." PRF sub-code into one label
  mutate(arrival_method = if_else(str_detect(arrival_method, "^Emergency Road Ambulance"),
                                  "Emergency Road Ambulance", arrival_method)) %>%
  group_by(arrival_method) %>%
  summarise(n = sum(n), .groups = "drop") %>%   # SUM real counts, don't re-count rows
  # Lump anything still under threshold (after the ambulance collapse) into "Others"
  mutate(arrival_method = if_else(n < N_SUPPRESS_THRESHOLD, "Others", arrival_method)) %>%
  group_by(arrival_method) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n)) %>%
  mutate(pct = percent(n / sum(n)))

arrival_method_linked_export <- arrival_method_linked %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(arrival_method_linked_export, "arrival_method_falls50plus_afterLinkage.csv")

arrival_method_linked_records <- linked %>%
  select(hcn, arr_dt, arrival_method, hospital_name, trust_name, acuity, dispo) %>%
  arrange(arrival_method, arr_dt)
write_csv(arrival_method_linked_records, "arrival_method_falls50plus_afterLinkage_records.csv")

p_arrival_method_linked <- arrival_method_linked %>%
  ggplot(aes(x = fct_reorder(arrival_method, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#00A896") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Linked cohort — Arrival Method (n = ", comma(sum(arrival_method_linked$n)), ")"), x = NULL, y = "Attendances") +
  theme_minimal(base_size = 12)
ggsave("arrival_method_falls50plus_afterLinkage.png", p_arrival_method_linked, width = 9.5, height = 5, dpi = 150)

# ---- Acuity (linked) — previously had NO suppression, pct, or CSV at all ----
acuity_linked <- linked %>% mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(acuity_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))

acuity_linked_export <- acuity_linked %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n))
write_csv(acuity_linked_export, "acuity_distribution_falls50plus_afterLinkage.csv")

acuity_linked_records <- linked %>%
  mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  select(hcn, arr_dt, acuity_cat, hospital_name, trust_name, arrival_method, dispo) %>%
  arrange(acuity_cat, arr_dt)
write_csv(acuity_linked_records, "acuity_distribution_falls50plus_afterLinkage_records.csv")

p_acuity_linked <- acuity_linked %>%
  ggplot(aes(x = fct_reorder(acuity_cat, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#02C39A") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Linked cohort — Acuity (n = ", comma(sum(acuity_linked$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("acuity_distribution_falls50plus_afterLinkage.png", p_acuity_linked, width = 7, height = 5, dpi = 150)

## ---- Temporal pattern (linked) — direct before/after comparison ----------
## Mirrors the before-linkage heatmap in Part 4 (p_heatmap), computed on the
## LINKED cohort instead of falls_50plus, so the "does the temporal pattern
## hold after linkage" question can be answered directly rather than
## assumed. Placed here (Section 15) rather than immediately after the
## before-linkage version in Part 4, because `linked` does not exist yet at
## that point in the script — linkage (Part 6) happens after Part 4.
## Suppression deliberately NOT applied to this chart (by explicit review
## decision) — see the note on the before-linkage version in Part 4.
linked <- linked %>%
  mutate(arr_month_linked = floor_date(arr_dt, "month"),
         arr_wday_linked  = wday(arr_dt, label = TRUE, week_start = 1),
         arr_hour_linked  = hour(arr_dt))

p_heatmap_linked <- linked %>% count(arr_wday_linked, arr_hour_linked) %>%
  ggplot(aes(x = arr_hour_linked, y = arr_wday_linked, fill = n)) + geom_tile() +
  scale_fill_gradient(low = "#F4F9F9", high = "#01474F") +
  labs(title = paste0("Linked cohort (50+) — hour x day of week (n = ", comma(nrow(linked)), ")"), x = "Hour", y = NULL, fill = "Attendances") +
  theme_minimal(base_size = 12)
ggsave("heatmap_hour_dow_falls50plus_afterLinkage.png", p_heatmap_linked, width = 9, height = 5.5, dpi = 150)


## =============================================================================
## Additional EDA on linked cohort (v2 — low-number suppression applied)
## =============================================================================
## Consolidated addendum covering everything requested in the last several
## turns: disposition/acuity distribution CSVs across all three cohorts,
## a before/after-linkage care-setting bias check, and a new ED 50+ (all
## reasons) cohort export.
##
## v2 change: every aggregate CSV/chart below now suppresses BOTH n and pct
## for any category cell under N_SUPPRESS_THRESHOLD (suppress_n()/
## suppress_pct()), matching the pattern used throughout the main pipeline —
## previously only n was suppressed, leaving pct as a possible back-
## calculation route to the true count. Record-level exports are untouched
## (suppression doesn't apply/protect at record level the way it does for
## aggregates) and remain clearly labelled INTERNAL ONLY.
##
## =============================================================================

cat("\n##***## ADDENDUM: DISPOSITION / ACUITY / CARE SETTING / ED 50+ EXPORTS ##***##\n")

## =============================================================================
## 1. ED DISPOSITION distribution — all ages, falls50plus, linked
## =============================================================================

# All ages, all attendances (n = 72,499) — CONTEXT ONLY, not the study cohort
dispo_all_ed <- raw_clean %>%
  mutate(dispo_cat = coalesce(dispo, "Missing")) %>%
  count(dispo_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
write_csv(dispo_all_ed %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n)),
          "dispo_distribution_ALL_AGES_context_only.csv")

# Falls cohort, Direct-tier keyword, 50+ (n = 10,435) — "before linkage".
# NOTE: dispo_falls50plus and its CSV were already computed and written in
# Part 4 (feeding p_dispo) — not recomputed here to avoid writing the same
# file twice from two different code locations.

# Linked cohort, NIAS-confirmed, 50+ (n = 12,212) — "after linkage"
dispo_linked <- linked %>%
  mutate(dispo_cat = coalesce(dispo, "Missing")) %>%
  count(dispo_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
write_csv(dispo_linked %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n)),
          "dispo_distribution_falls50plus_afterLinkage.csv")

# Chart — previously missing for the linked cohort (CSV-only before this pass)
p_dispo_linked <- dispo_linked %>%
  ggplot(aes(x = fct_reorder(dispo_cat, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#01474F") +
  geom_text(aes(label = paste0(suppress_n(n), " (", suppress_pct(n, pct), ")")), hjust = -0.05, size = 3) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Linked cohort — ED disposition (n = ", comma(sum(dispo_linked$n)), ")"), x = NULL, y = "Attendances") + theme_minimal(base_size = 12)
ggsave("dispo_distribution_falls50plus_afterLinkage.png", p_dispo_linked, width = 8, height = 5, dpi = 150)

cat("Disposition distribution CSVs + chart written: ALL_AGES, falls50plus (before), linked (after)\n")

## =============================================================================
## 2. ACUITY distribution — all ages, falls50plus, linked
## =============================================================================
## falls50plus/linked charts already exist (p_acuity_falls, p_acuity_linked);
## this adds the missing CSV exports plus an all-ages version for completeness.

acuity_all_ed <- raw_clean %>%
  mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(acuity_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
write_csv(acuity_all_ed %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n)),
          "acuity_distribution_ALL_AGES_context_only.csv")

acuity_falls50plus <- falls_50plus %>%
  mutate(acuity_cat = coalesce(acuity, "Not yet assigned")) %>%
  count(acuity_cat, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
# NOTE: the CSV for this table was already written in Part 4 (feeding
# p_acuity_falls) — not re-written here to avoid two code locations writing
# the same file.

## NOTE: acuity_linked (data, CSV, and chart) was already fully computed
## above — not re-written here to avoid two code locations writing the
## same file.

cat("Acuity distribution CSVs written: ALL_AGES, falls50plus (before), linked (after)\n")

## =============================================================================
## 3. CARE SETTING — where falls happen, full NIAS extract vs linked cohort
## =============================================================================
## "Before linkage" here means the FULL NIAS extract (all fall-related
## incidents NIAS attended, n = 19,575) — care_setting is NIAS-only and does
## not exist on the ED side, so falls_50plus has no equivalent to compare.
## "After linkage" is the linked cohort only (n = 12,212). Comparing the two
## checks whether linkage introduces bias by care setting (e.g. if care-home
## falls are systematically harder to link than falls in other settings).

care_setting_full_nias <- nias_clean %>%
  count(care_setting, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
write_csv(care_setting_full_nias %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n)),
          "care_setting_full_nias_beforeLinkage.csv")

care_setting_linked <- linked %>%
  count(care_setting, sort = TRUE) %>%
  mutate(pct = percent(n / sum(n)))
write_csv(care_setting_linked %>% mutate(pct = suppress_pct(n, pct), n = suppress_n(n)),
          "care_setting_linked_afterLinkage.csv")

# Direct side-by-side comparison — the actual bias check. A large swing in
# share for any one setting between the two columns is the signal to write
# up, not the raw counts on their own. n_* columns are kept (suppressed) so
# a reader can see which side of the comparison a given pct rests on.
care_setting_comparison <- care_setting_full_nias %>%
  rename(pct_full_nias = pct, n_full_nias = n) %>% select(care_setting, n_full_nias, pct_full_nias) %>%
  full_join(care_setting_linked %>% rename(pct_linked = pct, n_linked = n) %>%
              select(care_setting, n_linked, pct_linked),
            by = "care_setting")

care_setting_comparison_export <- care_setting_comparison %>%
  mutate(pct_full_nias = suppress_pct(n_full_nias, pct_full_nias),
         pct_linked     = suppress_pct(n_linked, pct_linked),
         n_full_nias    = suppress_n(n_full_nias),
         n_linked       = suppress_n(n_linked))
cat("\n==== CARE SETTING — full NIAS extract vs linked cohort (share comparison) ====\n")
print(care_setting_comparison_export)
write_csv(care_setting_comparison_export, "care_setting_linkage_bias_check.csv")

p_care_setting_linked <- care_setting_linked %>%
  ggplot(aes(x = fct_reorder(care_setting, n), y = suppress_n_for_plot(n))) + geom_col(fill = "#00A896") +
  geom_text(aes(label = suppress_n(n)), hjust = -0.1) + coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = comma) +
  labs(title = paste0("Care Setting — Linked cohort (NIAS-confirmed, 50+) (n = ", comma(sum(care_setting_linked$n)), ")"), x = NULL, y = "Incidents") +
  theme_minimal(base_size = 12)
ggsave("care_setting_falls50plus_afterLinkage.png", p_care_setting_linked, width = 8, height = 5, dpi = 150)

cat("Care setting CSVs + chart written: full NIAS (before), linked (after), bias-check comparison\n")

## =============================================================================
## 4. ED 50+ cohort, ALL REASONS for attendance (not fall-restricted)
## =============================================================================
## NOTE ON INTERPRETATION: this is distinct from every other 50+ cohort built
## so far — falls_50plus is age 50+ AND fall-keyword-flagged; encompass_for_
## linkage is age 50+ AND has a valid Ambulance Incident Number. This is the
## simplest possible cohort: every ED attendance aged 50+, for ANY reason,
## with no other restriction. It exists to answer "what fraction of all 50+
## ED activity is fall-related?" — a denominator the other cohorts can't
## provide on their own. If this isn't what was meant by "ED data for 50 &
## 50+", flag it and this section can be redefined.
##
## n_total (the whole 50+ ED population) is a headline total, not a subgroup
## breakdown, so — consistent with the rest of the pipeline — it is NOT
## suppressed. n_fall_related IS a subgroup count and is suppressed, along
## with its paired pct_fall_related.

ed_50plus_all_reasons <- raw_clean %>%
  filter(age_years_exact_at_arrival >= 50, !age_flag_implausible)

cat("\n==== ED 50+ cohort, ALL REASONS for attendance (not fall-restricted) ====\n")
cat("n =", nrow(ed_50plus_all_reasons), "(headline total — not suppressed)\n")
cat("Of which fall-related (Direct-tier keyword):", suppress_n(sum(ed_50plus_all_reasons$is_fall_flag)),
    "(", suppress_pct(sum(ed_50plus_all_reasons$is_fall_flag), percent(mean(ed_50plus_all_reasons$is_fall_flag))),
    "of all 50+ ED activity)\n")

ed_50plus_summary <- ed_50plus_all_reasons %>%
  summarise(
    n_total          = n(),
    n_fall_related   = sum(is_fall_flag),
    pct_fall_related = percent(mean(is_fall_flag))
  )
write_csv(ed_50plus_summary %>%
            mutate(pct_fall_related = suppress_pct(n_fall_related, pct_fall_related),
                   n_fall_related   = suppress_n(n_fall_related)),
          # n_total intentionally left as the real value — headline total, see note above
          "ed_50plus_all_reasons_summary.csv")

# Full record-level export — HIGH RISK, record-level, for internal use /
# further analysis only. Do not treat as a Tier 1 aggregate output.
# Suppression does not apply/protect at record level, so this is unchanged.
write_csv(ed_50plus_all_reasons, "ed_50plus_all_reasons_RECORD_LEVEL_INTERNAL_ONLY.csv")

cat("ED 50+ (all reasons) summary + record-level export written.\n")
cat("\n##***## ADDENDUM COMPLETE ##***##\n")

## Same fix as the hospital-episode-duration histogram. Widened from
## 10-minute to 20-minute bins for readability — the 10-minute version had
## ~115 bins, making the x-axis too dense to read comfortably.
HANDOVER_BINWIDTH_MINS <- 20
handover_raw_binned <- linked %>% filter(!handover_flag_implausible, !is.na(handover_delay_mins)) %>%
  mutate(bin_start = floor(handover_delay_mins / HANDOVER_BINWIDTH_MINS) * HANDOVER_BINWIDTH_MINS,
         bin_label = paste0(bin_start, "-", bin_start + HANDOVER_BINWIDTH_MINS)) %>%
  count(bin_start, bin_label, name = "n") %>%
  arrange(bin_start) %>%
  mutate(pct = percent(n / sum(n)))

handover_raw_binned_export <- handover_raw_binned %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n)) %>%
  select(bin_label, n, pct)
write_csv(handover_raw_binned_export, "handover_delay_falls50plus_afterLinkage_raw_binned.csv")

p_handover <- handover_raw_binned %>%
  mutate(n_for_plot = suppress_n_for_plot(n),
         bin_label = fct_reorder(bin_label, bin_start)) %>%
  ggplot(aes(x = bin_label, y = n_for_plot)) +
  geom_col(fill = "#028090") +
  ## FIX: with 20-minute bins, bin_start values are 0, 20, 40... so 30
  ## never appears as an exact bin_start (it did by coincidence with the
  ## old 10-minute bins). findInterval() finds which bin's range actually
  ## CONTAINS the 30-minute reference mark, and its return value is
  ## already the correct 1-indexed discrete-axis position for geom_vline.
  geom_vline(xintercept = findInterval(30, sort(unique(handover_raw_binned$bin_start))),
             linetype = "dashed", colour = "#E8A33D") +
  labs(title = paste0("Ambulance handover delay — linked falls cohort (50+) (n = ", comma(sum(!linked$handover_flag_implausible & !is.na(linked$handover_delay_mins))), ")"),
       subtitle = "Bars <10 incidents capped",
       x = "Minutes", y = "Incidents") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
ggsave("handover_delay_falls50plus_afterLinkage.png", p_handover, width = 9, height = 5, dpi = 150)

## ==== HOSPITAL EPISODE DURATION — a DISTINCT metric, Admit patients only ==
## hospital_episode_hours (= difftime(disch_dt, arr_dt)) is NOT ED length of
## stay for admitted patients — disch_date_time records eventual hospital
## discharge, not ED departure (see the fix above). Once correctly labelled,
## it's a genuinely useful secondary metric in its own right: total hospital
## episode duration following a fall, relevant to bed/rehab capacity
## planning. Scoped to Admit dispositions only, where the distinction from
## ED-only time actually applies (see the by-disposition validation:
## non-admitted categories showed ~0h discrepancy between the two methods).
hospital_episode_admit_only <- falls_50plus %>%
  filter(str_detect(tolower(coalesce(dispo, "")), "^admit$"),
         !is.na(hospital_episode_hours), hospital_episode_hours >= 0,
         hospital_episode_hours <= 24 * 90)  # 90-day sanity cap, generous for a hospital stay
cat("\n==== HOSPITAL EPISODE DURATION — Admitted falls patients (50+) ====\n")
cat("DISTINCT from ED length of stay above — this is total time from ED\n",
    "arrival to eventual hospital discharge, for patients who were admitted.\n")
hospital_episode_summary <- hospital_episode_admit_only %>%
  summarise(n = n(),
            median_days = round(median(hospital_episode_hours) / 24, 1),
            p90_days    = round(quantile(hospital_episode_hours, 0.9) / 24, 1),
            max_days    = round(max(hospital_episode_hours) / 24, 1))
print(hospital_episode_summary %>% mutate(n = suppress_n(n)))
write_csv(hospital_episode_summary %>% mutate(n = suppress_n(n)), "hospital_episode_duration_admit_only.csv")

## FIX: geom_histogram() auto-bins with no hook to suppress individual thin
## bins, and had no CSV backing it at all — meaning nobody could check
## whether any bar represented fewer than the suppression threshold. Bins
## are now computed manually so the same suppress_n_for_plot() treatment
## used everywhere else applies here too, and there's a real CSV to check
## exact bin counts against. Widened from 2-day to 5-day bins: with the
## long right tail this metric has, 2-day bins left many thin bars needing
## visual capping even after suppression; 5-day bins substantially reduce
## how often that happens while still showing the distribution's shape.
EPISODE_BINWIDTH_DAYS <- 5
hospital_episode_binned <- hospital_episode_admit_only %>%
  mutate(episode_days = hospital_episode_hours / 24,
         bin_start = floor(episode_days / EPISODE_BINWIDTH_DAYS) * EPISODE_BINWIDTH_DAYS,
         bin_label = paste0(bin_start, "-", bin_start + EPISODE_BINWIDTH_DAYS)) %>%
  count(bin_start, bin_label, name = "n") %>%
  arrange(bin_start) %>%
  mutate(pct = percent(n / sum(n)))

hospital_episode_binned_export <- hospital_episode_binned %>%
  mutate(pct = suppress_pct(n, pct), n = suppress_n(n)) %>%
  select(bin_label, n, pct)
write_csv(hospital_episode_binned_export, "hospital_episode_duration_admit_only_binned.csv")

p_hospital_episode <- hospital_episode_binned %>%
  mutate(n_for_plot = suppress_n_for_plot(n),
         bin_label = fct_reorder(bin_label, bin_start)) %>%
  ggplot(aes(x = bin_label, y = n_for_plot)) +
  geom_col(fill = "#5B7A82") +
  labs(title = paste0("Total hospital episode duration — Admitted falls patients (50+) (n = ", comma(nrow(hospital_episode_admit_only)), ")"),
       subtitle = "NOT ED length of stay — arrival to eventual hospital discharge, admitted patients only. Bars <10 patients capped.",
       x = "Days", y = "Patients") + theme_minimal(base_size = 12) +
  theme(plot.subtitle = element_text(size = 8, colour = "grey40"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8))
ggsave("hospital_episode_duration_admit_only.png", p_hospital_episode, width = 9, height = 5, dpi = 150)

## Individual-record breakdown — INTERNAL QA/REVIEW ONLY. Contains hcn (a
## direct patient identifier) and per-patient episode duration; do not
## include in the report or anything shared with governance — record-level
## detail isn't protected by count suppression the way the summary above is.
hospital_episode_admit_only_records <- hospital_episode_admit_only %>%
  mutate(hospital_episode_days = round(hospital_episode_hours / 24, 1)) %>%
  select(hcn, arr_dt, disch_dt, hospital_episode_days, hospital_name, trust_name, dispo, acuity) %>%
  arrange(desc(hospital_episode_days))
cat("\nAdmitted falls patients (50+) — individual episode-duration records (n =",
    nrow(hospital_episode_admit_only_records), "):\n")
write_csv(hospital_episode_admit_only_records, "hospital_episode_duration_admit_only_records.csv")






cat("\n##***## END OF ANALYSIS PIPELINE ##***##\n")

