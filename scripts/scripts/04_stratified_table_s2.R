
# SUPPLEMENTARY TABLE S2: sex-stratified regression (education x sex)
# Run AFTER clean_tables.R and extra_checks.R
# Needs: hdes, fml, labs, covs


library(survey)
library(gtsummary)
library(flextable)
library(dplyr)




# 1. SEPARATE MODELS FOR MEN AND WOMEN (sex removed from the formula)

fml_ns  <- update(fml, . ~ . - sex)
labs_ns <- labs[names(labs) != "sex"]

des_m <- subset(hdes, sex == "Male")
des_f <- subset(hdes, sex == "Female")

m_male <- svyglm(fml_ns, design = des_m, family = quasibinomial())
m_fem  <- svyglm(fml_ns, design = des_f, family = quasibinomial())

n_male <- nobs(m_male)
n_fem  <- nobs(m_fem)
c(men = n_male, women = n_fem)





# 2. P-VALUE FOR THE INTERACTION (from the model with sex x education)

m_int3 <- svyglm(update(fml, . ~ . + sex:education), design = hdes,
                 family = quasibinomial())
p_int <- regTermTest(m_int3, ~sex:education)$p
p_int   # about 0.014






# 3. TABLE S2: adjusted odds ratios, men and women side by side

t_m <- tbl_regression(m_male, exponentiate = TRUE, label = labs_ns) %>%
  modify_header(estimate ~ "**AOR**")
t_f <- tbl_regression(m_fem, exponentiate = TRUE, label = labs_ns) %>%
  modify_header(estimate ~ "**AOR**")

tbl_s2 <- tbl_merge(
  list(t_m, t_f),
  tab_spanner = c(paste0("**Men (n = ", n_male, ")**"),
                  paste0("**Women (n = ", n_fem, ")**"))
) %>%
  modify_header(label ~ "**Characteristic**") %>%
  bold_labels()
tbl_s2






# 4. WEIGHTED % UNDIAGNOSED BY SEX AND EDUCATION (with 95% CI)

pct <- svyby(~I(undx == 1), ~sex + education, hdes, svyciprop,
             vartype = "ci", method = "logit")
names(pct) <- c("Sex", "Education", "Percent", "CI_lower", "CI_upper")
pct[, c("Percent", "CI_lower", "CI_upper")] <-
  round(pct[, c("Percent", "CI_lower", "CI_upper")] * 100, 1)
pct$Estimate <- paste0(pct$Percent, " (", pct$CI_lower, "-", pct$CI_upper, ")")
pct_tab <- pct[, c("Sex", "Education", "Estimate")]
pct_tab








# 5. EXPORT TO WORD

as_flex_table(tbl_s2) %>% save_as_docx(path = "Supplementary_Table_S2.docx")
flextable(pct_tab) %>% autofit() %>%
  save_as_docx(path = "Supplementary_Table_S3_percent_by_sex_education.docx")





# "AOR = adjusted odds ratio; CI = confidence interval. Survey-weighted logistic
#  regression models were fitted separately for men and women and adjusted for all
#  variables shown. p for interaction between sex and education = [p_int]."
