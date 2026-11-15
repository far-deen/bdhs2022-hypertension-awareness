

library(haven)
library(dplyr)
library(survey)
library(gtsummary)
library(car)
library(pROC)
library(xfun)
library(flextable)


setwd("C:/Users/Fardin/Desktop/USCLAP/R files")


df = data.frame(read_dta("BDPR81FL.dta"))        # Household Member Records


options(survey.lonely.psu = "adjust")          # avoids single-PSU errors






# 1

vars = c("hv005", "hv021", "hv022",           # weight, PSU, strata
          "hv024", "hv025", "hv104", "hv105",  # division, residence, sex, age
          "hv106", "hv115", "hv270",           # education, marital, wealth
          "wbp16", "wbp19", "wbp24", "wbp25",  # women: told HTN, medication, SBP, DBP
          "mbp16", "mbp19", "mbp24", "mbp25",  # men: same
          "ha40", "hb40")                      # BMI women / men (x100)


raw = read_dta("BDPR81FL.DTA", col_select = all_of(vars))

dim(raw)   # expect 132463 rows








# 2 


dat = raw %>%
  mutate(
    wt  = hv005 / 1000000,                                   # sample weight
    age = as.numeric(hv105),
    
    # combine women + men columns into one
    sbp = coalesce(as.numeric(wbp24), as.numeric(mbp24)),
    dbp = coalesce(as.numeric(wbp25), as.numeric(mbp25)),
    dx  = coalesce(as.numeric(wbp16), as.numeric(mbp16)),    # 1 = told by doctor
    med = coalesce(as.numeric(wbp19), as.numeric(mbp19)),    # 1 = on BP medication
    bmi = coalesce(as.numeric(ha40), as.numeric(hb40)) / 100,
    bmi = ifelse(bmi >= 99, NA, bmi),                        # 9998 = not measured
    
    # adults 18+ with a BP measurement
    measured = !is.na(sbp) & !is.na(dbp) & age >= 18,
    
    # hypertension (measured high BP OR on medication)
    htn = ifelse(measured,
                 as.integer(sbp >= 140 | dbp >= 90 | med %in% 1), NA),
    
    # sensitivity definition: measured high BP only (no medication criterion)
    htn_nomed = ifelse(measured, as.integer(sbp >= 140 | dbp >= 90), NA),
    
    # OUTCOME: UNAWARE  = hypertensive but never told by doctor/health worker
    undx     = ifelse(htn == 1, as.integer(dx == 0), NA),
    undx_nm  = ifelse(htn_nomed == 1, as.integer(dx == 0), NA),
    undx_lab = factor(undx, levels = c(0, 1),
                      labels = c("Diagnosed", "UNAWARE ")),
    
    # PREDICTORS
    age_grp   = cut(age, c(17, 29, 39, 49, 59, Inf),
                    labels = c("18-29", "30-39", "40-49", "50-59", "60+")),
    sex       = factor(as.numeric(hv104), levels = 1:2,
                       labels = c("Male", "Female")),
    residence = factor(as.numeric(hv025), levels = 1:2,
                       labels = c("Urban", "Rural")),
    division  = droplevels(as_factor(hv024)),
    wealth    = factor(as.numeric(hv270), levels = 1:5,
                       labels = c("Poorest", "Poorer", "Middle",
                                  "Richer", "Richest")),
    education = factor(case_when(as.numeric(hv106) == 0      ~ "No education",
                                 as.numeric(hv106) == 1      ~ "Primary",
                                 as.numeric(hv106) %in% 2:3  ~ "Secondary or higher"),
                       levels = c("No education", "Primary", "Secondary or higher")),
    marital   = factor(case_when(as.numeric(hv115) == 0      ~ "Never married",
                                 as.numeric(hv115) %in% 1:2  ~ "Married/living together",
                                 as.numeric(hv115) %in% 3:5  ~ "Widowed/divorced/separated"),
                       levels = c("Married/living together", "Never married",
                                  "Widowed/divorced/separated")),
    bmi_cat   = factor(case_when(bmi < 18.5 ~ "Underweight",
                                 bmi < 25   ~ "Normal",
                                 bmi >= 25  ~ "Overweight/obese"),
                       levels = c("Normal", "Underweight", "Overweight/obese"))
  )

dat


summary(dat)




# NAs in data

hyp = dat %>% filter(htn == 1)        # your analysis sample, 2,970 people

colSums(is.na(hyp[, c("undx","age_grp","sex","residence","division",
                      "wealth","education","marital","bmi_cat")]))



round(colMeans(is.na(hyp[, c("education","marital","bmi_cat")])) * 100, 1)   # % missing









# 3

flow = data.frame(
  step = c("All household members", "Adults 18+", "Adults with measured BP",
           "Hypertensive adults", "UNAWARE  hypertension"),
  n    = c(nrow(dat), sum(dat$age >= 18), sum(dat$measured),
           sum(dat$htn == 1, na.rm = TRUE), sum(dat$undx == 1, na.rm = TRUE))
)

print(flow)

# Check: should be about 14,296 measured adults, 2,970 hypertensive,
# 1,298 UNAWARE  (unweighted)





# 4


# Build on all measured adults, then subset to hypertensive
# (sub setting the design keeps standard errors correct)


adults = dat %>% filter(measured)

des    = svydesign(ids = ~hv021, strata = ~hv022, weights = ~wt,
                    data = adults, nest = TRUE)
hdes   = subset(des, htn == 1)

# Weighted prevalence of hypertension among all measured adults

svyciprop(~I(htn == 1), des, method = "logit")

# Weighted prevalence of UNAWARE  hypertension among hypertensive adults

svyciprop(~I(undx == 1), hdes, method = "logit")





# 5

# TABLE 1: DESCRIPTIVE + BIVARIATE (Rao-Scott test)
# percent = "row" -> % UNAWARE  within each category

covs = c("age_grp", "sex", "residence", "division", "wealth",
          "education", "marital", "bmi_cat")

tbl1 = hdes %>%
  tbl_svysummary(by = undx_lab,
                 include = all_of(covs),
                 percent = "row",
                 missing = "no",
                 label = list(age_grp ~ "Age group", sex ~ "Sex",
                              residence ~ "Residence", division ~ "Division",
                              wealth ~ "Wealth quintile",
                              education ~ "Education",
                              marital ~ "Marital status",
                              bmi_cat ~ "BMI category")) %>%
  add_overall() %>%
  add_p() %>%
  bold_labels()

tbl1



tbl1 %>% as_gt() %>% gt::gtsave("table1.html")






# 6

fml = undx ~ age_grp + sex + residence + division + wealth +
  education + marital + bmi_cat

m1 = svyglm(fml, design = hdes, family = quasibinomial())

summary(m1)

nobs(m1)    # observations used (complete cases)


# Adjusted odds ratios with 95% CI

aor <- exp(cbind(AOR = coef(m1), confint(m1)))
round(aor, 2)


# Crude + adjusted side by side

crude <- lapply(covs, function(v) {
  m <- svyglm(as.formula(paste("undx ~", v)), design = hdes,
              family = quasibinomial())
  tbl_regression(m, exponentiate = TRUE)
}) %>% tbl_stack()

adj <- tbl_regression(m1, exponentiate = TRUE)

tbl2 <- tbl_merge(list(crude, adj),
                  tab_spanner = c("**Crude**", "**Adjusted**")) %>%
  bold_labels()


tbl2

tbl2 %>% as_gt() %>% gt::gtsave("table2.html")






# 7 

# (a) Multicollinearity: GVIF^(1/(2*Df)) should be < 2 (VIF < 5 rule)

hyp <- adults %>% filter(htn == 1)

vif(glm(fml, data = hyp, family = binomial()))


# (b) Discrimination: AUC

roc_obj <- roc(m1$y, as.numeric(fitted(m1)))

auc(roc_obj)






# 8


hdes2 <- subset(des, htn_nomed == 1)

m2 <- svyglm(update(fml, undx_nm ~ .), design = hdes2,
             family = quasibinomial())

round(exp(cbind(AOR = coef(m2), confint(m2))), 2)

svyciprop(~I(undx_nm == 1), hdes2, method = "logit")





# 9 Saving the results


write.csv(flow, "flow.csv", row.names = FALSE)
write.csv(round(aor, 3), "AOR_results.csv")
write.csv(as_tibble(tbl1), "table1.csv", row.names = FALSE)
write.csv(as_tibble(tbl2), "table2.csv", row.names = FALSE)





