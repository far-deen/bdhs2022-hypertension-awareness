# ====================================================================
# CLEAN TABLES: nice labels, unweighted n, export to Word
# Run this AFTER your main script (needs: dat, fml, covs already created)
# ====================================================================

# install.packages("flextable")     # run once, needed for Word export
library(gtsummary)
library(flextable)
library(survey)
library(dplyr)

# --------------------------------------------------------------------
# 1. CAPITALISE DIVISION NAMES (barishal -> Barishal)
# --------------------------------------------------------------------
levels(dat$division) <- tools::toTitleCase(levels(dat$division))
table(dat$division)

# --------------------------------------------------------------------
# 2. REBUILD DESIGN OBJECTS AND MODELS (factor labels changed)
# --------------------------------------------------------------------
adults <- dat %>% filter(measured)
des    <- svydesign(ids = ~hv021, strata = ~hv022, weights = ~wt,
                    data = adults, nest = TRUE)
hdes   <- subset(des, htn == 1)
hdes2  <- subset(des, htn_nomed == 1)

m1 <- svyglm(fml, design = hdes, family = quasibinomial())
m2 <- svyglm(update(fml, undx_nm ~ .), design = hdes2, family = quasibinomial())
nobs(m1)   # main model n
nobs(m2)   # sensitivity model n

# --------------------------------------------------------------------
# 3. LABELS USED IN ALL TABLES
# --------------------------------------------------------------------
labs <- list(age_grp   = "Age group (years)",
             sex       = "Sex",
             residence = "Place of residence",
             division  = "Division",
             wealth    = "Wealth quintile",
             education = "Education",
             marital   = "Marital status",
             bmi_cat   = "BMI category")

# --------------------------------------------------------------------
# 4. TABLE 1: unweighted n with weighted row %
# --------------------------------------------------------------------
tbl1 <- hdes %>%
  tbl_svysummary(by = undx_lab,
                 include = all_of(covs),
                 percent = "row",
                 missing = "no",
                 statistic = all_categorical() ~ "{n_unweighted} ({p}%)",
                 label = labs) %>%
  add_overall() %>%
  add_p() %>%
  modify_header(label ~ "**Characteristic**",
                all_stat_cols() ~ "**{level}**  \nn = {n_unweighted}") %>%
  bold_labels()
tbl1

# --------------------------------------------------------------------
# 5. TABLE 2: crude (COR) and adjusted (AOR) odds ratios
# --------------------------------------------------------------------
crude <- lapply(covs, function(v) {
  m <- svyglm(as.formula(paste("undx ~", v)), design = hdes,
              family = quasibinomial())
  tbl_regression(m, exponentiate = TRUE, label = labs[v])
}) %>% tbl_stack() %>%
  modify_header(estimate ~ "**COR**")

adj <- tbl_regression(m1, exponentiate = TRUE, label = labs) %>%
  modify_header(estimate ~ "**AOR**")

tbl2 <- tbl_merge(list(crude, adj),
                  tab_spanner = c("**Crude**", "**Adjusted**")) %>%
  modify_header(label ~ "**Characteristic**") %>%
  bold_labels()
tbl2

# --------------------------------------------------------------------
# 6. SUPPLEMENTARY TABLE: sensitivity analysis (BP-only definition)
# --------------------------------------------------------------------
tbl_s1 <- tbl_regression(m2, exponentiate = TRUE, label = labs) %>%
  modify_header(label ~ "**Characteristic**", estimate ~ "**AOR**") %>%
  bold_labels()
tbl_s1

# --------------------------------------------------------------------
# 7. EXPORT TO WORD (files appear in your USCLAP folder)
# --------------------------------------------------------------------
as_flex_table(tbl1)   %>% save_as_docx(path = "Table1.docx")
as_flex_table(tbl2)   %>% save_as_docx(path = "Table2.docx")
as_flex_table(tbl_s1) %>% save_as_docx(path = "Supplementary_Table_S1.docx")

# Footnotes to type under the tables in Word:
# Table 1: n = unweighted count; percentages are weighted row percentages;
#          p-values from Rao-Scott chi-square test.
# Table 2: COR = crude odds ratio; AOR = adjusted odds ratio; CI = confidence
#          interval; adjusted for all variables listed; survey-weighted
#          logistic regression; final model n = (nobs(m1)).
