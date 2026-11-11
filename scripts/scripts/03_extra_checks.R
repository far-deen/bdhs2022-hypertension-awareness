# ====================================================================
# EXTRA CHECKS: overall Wald tests, interaction tests, AIC/BIC,
# events per variable. Run AFTER clean_tables.R (needs dat, hdes, m1, fml, covs)
# ====================================================================
library(survey)
library(dplyr)

# --------------------------------------------------------------------
# 1. OVERALL (JOINT) WALD TEST FOR EACH VARIABLE
#    Useful for multi-category variables (age, division, wealth, education)
#    Report as "overall p = ..." in the Results text
# --------------------------------------------------------------------
overall <- sapply(covs, function(v) {
  regTermTest(m1, as.formula(paste("~", v)))$p
})
data.frame(variable = covs, overall_p = signif(overall, 3))

# --------------------------------------------------------------------
# 2. INTERACTION TESTS (survey-weighted)
#    Pre-specify only a few that make sense; each is a separate test
# --------------------------------------------------------------------
m_int1 <- svyglm(update(fml, . ~ . + age_grp:sex), design = hdes,
                 family = quasibinomial())
regTermTest(m_int1, ~age_grp:sex)            # does the age effect differ by sex?

m_int2 <- svyglm(update(fml, . ~ . + residence:wealth), design = hdes,
                 family = quasibinomial())
regTermTest(m_int2, ~residence:wealth)       # does wealth effect differ by residence?

m_int3 <- svyglm(update(fml, . ~ . + sex:education), design = hdes,
                 family = quasibinomial())
regTermTest(m_int3, ~sex:education)          # does education effect differ by sex?

# p > 0.05 for an interaction = no evidence it is needed; keep the main model.

# --------------------------------------------------------------------
# 3. AIC AND BIC (unweighted logistic models on the same complete cases)
#    Only meaningful for COMPARING models. Lower = better.
# --------------------------------------------------------------------
hyp <- dat %>% filter(htn == 1)
hyp_cc <- hyp[complete.cases(hyp[c("undx", covs)]), ]
nrow(hyp_cc)                                  # should be 2,903

g_full  <- glm(fml, data = hyp_cc, family = binomial())
g_nodiv <- glm(update(fml, . ~ . - division), data = hyp_cc, family = binomial())
g_small <- glm(undx ~ age_grp + sex + education + marital + bmi_cat,
               data = hyp_cc, family = binomial())
g_int   <- glm(update(fml, . ~ . + age_grp:sex), data = hyp_cc, family = binomial())

models <- list("Full model" = g_full,
               "Without division" = g_nodiv,
               "Reduced (age, sex, education, marital, BMI)" = g_small,
               "Full + age x sex" = g_int)

data.frame(model = names(models),
           parameters = sapply(models, function(m) length(coef(m))),
           AIC = round(sapply(models, AIC), 1),
           BIC = round(sapply(models, BIC), 1),
           row.names = NULL)

# --------------------------------------------------------------------
# 4. EVENTS PER VARIABLE (rule of thumb: at least 10)
# --------------------------------------------------------------------
events <- sum(hyp_cc$undx == 1)
params <- length(coef(g_full)) - 1
c(events = events, parameters = params, events_per_parameter = round(events / params, 1))
