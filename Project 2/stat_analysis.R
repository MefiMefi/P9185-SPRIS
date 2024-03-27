## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
source(knitr::purl("./data_manipulation.Rmd", quiet=TRUE))
source(knitr::purl("./EDA.Rmd", quiet=TRUE))
library(knitr)
library(kableExtra)


## ----table1-------------------------------------------------------------------
data.baseline <- data.comp %>% filter(day == 0)

library(table1)
data.baseline$gender = ifelse(data.baseline$gender == "M", "Male", "Female")
label(data.baseline$age) = "Age"
label(data.baseline$gender) = "Gender"
label(data.baseline$treatment_group) = "Treatment"
label(data.baseline$mem_comp) = "Baseline Composite Memory Score"
my.render.cont <- function(x) {
    with(stats.default(x), 
        sprintf("%0.2f (%0.1f)", MEAN, SD))
}

tbl1 <- table1( ~ gender + age + mem_comp | treatment_group, data=data.baseline, render.continuous=my.render.cont) %>% t1kable(booktabs = TRUE, format = "latex") %>%  kable_styling(latex_options = c("hold_position", "scale_down"))

writeLines(as.character(tbl1), "tbl1.tex")


## -----------------------------------------------------------------------------
# centering age for better interpretation
data.full$age = data.full$age - mean(data.baseline$age)
data.comp$age = data.comp$age - mean(data.baseline$age)
data.surv$age = data.surv$age - mean(data.baseline$age)


## ----model-complete-case-gee--------------------------------------------------
library(gee)
library(geepack)
library(glmtoolbox)
library(gtsummary)
library(tidyverse)
data.comp.lag <-
  data.comp %>% 
  group_by(subject_id) %>% 
  mutate(mem_comp_lag = lag(mem_comp), mem_comp_delta_lag = lag(mem_comp_delta),observed_lag = lag(observed)) %>% 
  ungroup() %>% 
  filter(observed_lag != 0) %>% 
  mutate(drop = ifelse(observed == 0 & observed_lag == 1, 1, 0)) %>% 
  dplyr::select(subject_id, day, day_fct, age, gender, treatment_group, mem_comp_lag, mem_comp_delta_lag, drop) %>% 
  mutate(day_fct = factor(day, levels = c(5,19,90)))

data.comp <- 
  data.comp %>% 
  group_by(subject_id) %>% 
  mutate(total_obs = sum(as.numeric(observed))) %>% 
  ungroup() %>% 
  mutate(type = as.factor(ifelse(total_obs == 4, "Completer", "Drop-out")))


model.complete.1 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp %>% filter(type=="Completer"), family = gaussian, id = subject_id, corstr = "independence")
model.complete.2 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp %>% filter(type=="Completer"), family = gaussian, id = subject_id,  corstr = "ar1")
model.complete.3 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp %>% filter(type=="Completer"), family = gaussian, id = subject_id, corstr = "exchangeable")

model.complete.1.tbl <- tbl_regression(model.complete.1,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.complete.2.tbl <- tbl_regression(model.complete.2,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.complete.3.tbl <- tbl_regression(model.complete.3,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 

model.complete.tbl <-
  tbl_merge(list(model.complete.1.tbl, model.complete.2.tbl, model.complete.3.tbl), tab_spanner = c("Independent","AR-1","Exchangable")) %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position")) %>% add_header_above(c(" ", "Independent" = 3, "AR-1" = 3, "Exchangable" = 3))
model.complete.tbl
writeLines(as.character(model.complete.tbl), "complete_case_gee.tex")


## ----model-complete-case-gee-delta--------------------------------------------
model.complete.delta.1 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender , data = data.comp %>% filter(type=="Completer", day > 0) %>% mutate(day_fct = factor(day_fct, levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence")
model.complete.delta.2 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender , data = data.comp %>% filter(type=="Completer", day > 0) %>% mutate(day_fct = factor(day_fct, levels = c(5,19,90))), family = gaussian, id = subject_id,  corstr = "ar1")
model.complete.delta.3 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender, data = data.comp %>% filter(type=="Completer", day > 0) %>% mutate(day_fct = factor(day_fct, levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "exchangeable")


model.complete.delta.1.tbl <- tbl_regression(model.complete.delta.1,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% modify_header(list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")) 
model.complete.delta.2.tbl <- tbl_regression(model.complete.delta.2,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.complete.delta.3.tbl <- tbl_regression(model.complete.delta.3,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 

model.complete.delta.tbl <-
  tbl_merge(list(model.complete.delta.1.tbl, model.complete.delta.2.tbl, model.complete.delta.3.tbl), tab_spanner = c("Independent","AR-1","Exchangable")) %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position")) %>% add_header_above(c(" ", "Independent" = 3, "AR-1" = 3, "Exchangable" = 3))
model.complete.delta.tbl
writeLines(as.character(model.complete.delta.tbl), "complete_case_gee_delta.tex")


## ----trt-test-complete-case-gee-delta-----------------------------------------
# full model
model.complete.delta.trt.1 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender, data = data.comp %>% filter(type=="Completer", day > 0) %>% mutate(day_fct = factor(day_fct, levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence")
# null model
model.complete.null.1 <- geeglm(mem_comp_delta ~ day_fct + age + gender, data = data.comp %>% filter(type=="Completer", day > 0) %>% mutate(day_fct = factor(day_fct, levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence")

anova(model.complete.delta.trt.1,model.complete.null.1)


## ----intr-test-complete-case-gee-delta----------------------------------------
anova(model.complete.delta.trt.1,model.complete.delta.1)


## ----model-test-mcar----------------------------------------------------------
data.comp.lag <-
  data.comp.lag %>% 
  group_by(subject_id) %>% 
  mutate(just_dropped = ifelse((drop == 1 & lag(drop) == 0) | (drop == 1 & is.na(lag(drop))) , 1, 0)) %>% 
  ungroup()
model.drop <-
  geeglm(just_dropped ~ day_fct + treatment_group + age + gender, data = data.comp.lag, family = binomial, id = subject_id, corstr = "independence")

tbl_regression(model.drop, exponentiate = F)

model.drop <-
  geeglm(just_dropped ~ day_fct + treatment_group + age + gender, data = data.comp.lag, family = binomial, id = subject_id, corstr = "independence")

tbl_regression(model.drop, exponentiate = F)


## ----model-lmer---------------------------------------------------------------
library(lme4)
library(nlme)
library(lattice)
library(knitr)
library(kableExtra)

model.lmer = lmer(
  mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct + (1 |subject_id), data = data.comp
)

tbl_regression(model.lmer)

model.lmer.delta = lmer(
  mem_comp_delta ~ day_fct + treatment_group + age + gender + treatment_group * day_fct + (1 |subject_id), data = data.comp %>% filter(day > 0)
)

tbl_regression(model.lmer.delta)


## ----model-ps-----------------------------------------------------------------
# propensity score models, using logistic regression and coxph model 
model.prob.drop <-
  geeglm(dropped ~ day_fct + treatment_group + age + gender, data = data.surv %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = binomial, id = subject_id, corstr = "independence")

model.prob.drop.coxph <- 
  coxph(Surv(start, stop, dropped) ~ treatment_group + age + gender, data = data.surv)

model.prob.drop.tbl <- tbl_regression(model.prob.drop, exponentiate = T,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "95% CI", label = "Characteristics", estimate = "OR")
  )  %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position"))

writeLines(as.character(model.prob.drop.tbl), "ps_model.tex")

model.prob.drop.coxph.tbl <- tbl_regression(model.prob.drop.coxph, exponentiate = T,label = list(
      #day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "HR")
  )  %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position"))

writeLines(as.character(model.prob.drop.coxph.tbl), "ps_cox_model.tex")

tbl_merge(
  list(tbl_regression(model.prob.drop, exponentiate = F),tbl_regression(model.prob.drop.coxph, exponentiate = F))
)

predict.prob.drop <- predict(model.prob.drop, newdata = data.surv, type = "response")
predict.prob.drop.coxph <- predict(model.prob.drop.coxph, newdata = data.surv, type = "survival")

data.surv$prob.stay <- 1 - predict.prob.drop
data.surv$prob.stay.coxph <- predict.prob.drop.coxph

data.comp <- left_join(data.comp, data.surv %>% dplyr::select(subject_id,day_fct,treatment_group,prob.stay, prob.stay.coxph))
data.comp <-
  data.comp %>% 
  mutate(
    prob.stay = ifelse(day_fct==0, 1, prob.stay),
    prob.stay.coxph = ifelse(day_fct==0, 1, prob.stay.coxph)
  )


## ----model-ipw-gee------------------------------------------------------------
model.ipw.1 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "independence", weights = 1/prob.stay)
model.ipw.2 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id,  corstr = "ar1", weights = 1/prob.stay)
model.ipw.3 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "exchangeable", weights = 1/prob.stay)


model.ipw.1.tbl <- tbl_regression(model.ipw.1,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.ipw.2.tbl <- tbl_regression(model.ipw.2,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.ipw.3.tbl <- tbl_regression(model.ipw.3,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender",
      `day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 

model.ipw.tbl <-
  tbl_merge(list(model.ipw.1.tbl, model.ipw.2.tbl, model.ipw.3.tbl), tab_spanner = c("Independent","AR-1","Exchangable")) %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position")) %>% add_header_above(c(" ", "Independent" = 3, "AR-1" = 3, "Exchangable" = 3))
model.ipw.tbl
writeLines(as.character(model.ipw.tbl), "ipw_gee.tex")


## ----model-ipw-gee-coxph------------------------------------------------------
model.ipw.coxph.1 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender , data = data.comp %>% filter(day > 0) %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence", weights = 1/prob.stay.coxph)
model.ipw.coxph.2 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender , data = data.comp %>% filter(day > 0) %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = gaussian, id = subject_id,  corstr = "ar1", weights = 1/prob.stay.coxph)
model.ipw.coxph.3 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender , data = data.comp %>% filter(day > 0) %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "exchangeable", weights = 1/prob.stay.coxph)

model.ipw.coxph.1.tbl <- tbl_regression(model.ipw.coxph.1,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.ipw.coxph.2.tbl <- tbl_regression(model.ipw.coxph.2,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 
model.ipw.coxph.3.tbl <- tbl_regression(model.ipw.coxph.3,label = list(
      day_fct ~ "Days",
      treatment_group ~ "Treatment",
      age ~ "Age",
      gender ~ "Gender"
      #`day_fct:treatment_group` ~ "Days*Treatment"
    )) %>% 
  modify_header(
  update = list(p.value = "p-value", ci = "$95\\%$ CI", label = "Characteristics", estimate = "$\\widehat\\beta$")
  ) 

model.ipw.coxph.tbl <-
  tbl_merge(list(model.ipw.coxph.1.tbl, model.ipw.coxph.2.tbl, model.ipw.coxph.3.tbl), tab_spanner = c("Independent","AR-1","Exchangable")) %>% as_kable(booktabs = T, format = "latex",escape = FALSE) %>%  kable_styling(latex_options = c("hold_position")) %>% add_header_above(c(" ", "Independent" = 3, "AR-1" = 3, "Exchangable" = 3))
# tbl_merge(
#   list(tbl_regression(model.ipw.coxph.1), tbl_regression(model.ipw.coxph.2), tbl_regression(model.ipw.coxph.3))
# )


## -----------------------------------------------------------------------------
model.ipw.coxph.trt.1 <- geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender, data = data.comp %>% filter(day > 0) %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence", weights = 1/prob.stay.coxph)

model.ipw.coxph.null.1 <- geeglm(mem_comp_delta ~ day_fct + age + gender, data = data.comp %>% filter(day > 0) %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = gaussian, id = subject_id, corstr = "independence", weights = 1/prob.stay.coxph)

# testing treatment effects
anova(model.ipw.coxph.trt.1, model.ipw.coxph.null.1) # 0.0346 
# testing interactions
anova(model.ipw.coxph.trt.1, model.ipw.coxph.1) # 0.2212


## -----------------------------------------------------------------------------
data.comp %>% 
    group_by(day_fct) %>% 
    summarise(observed = round(sum(!is.na(mem_comp))/158,digits=2), missing = round(sum(is.na(mem_comp))/30, digits = 2))
temp <- matrix(c(47, 45, 35, 31, 0, 2, 12, 16),nrow=4,ncol=2)
chisq.test(temp,correct=FALSE)



data.comp %>% 
    group_by(gender) %>% 
    summarise(observed = round(sum(!is.na(mem_comp))/158,digits=2), missing = round(sum(is.na(mem_comp))/30, digits = 2))
temp <- matrix(c(87, 71, 13, 17),nrow=2,ncol=2)
chisq.test(temp,correct=FALSE)
 


data.comp %>% 
    group_by(treatment_group) %>% 
    summarise(observed = round(sum(!is.na(mem_comp))/158,digits=2), missing = round(sum(is.na(mem_comp))/30, digits = 2))
temp <- matrix(c(58, 46, 54, 10, 10, 10),nrow=2,ncol=2)
chisq.test(temp,correct=FALSE)

