## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## -----------------------------------------------------------------------------
source(knitr::purl("./data_manipulation.Rmd", quiet=TRUE))
source(knitr::purl("./EDA.Rmd", quiet=TRUE))


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

tbl1 <- table1( ~ gender + age + mem_comp | treatment_group, data=data.baseline, render.continuous=my.render.cont)

tbl1


## -----------------------------------------------------------------------------
# centering age for better interpretation
data.full$age = data.full$age - mean(data.full$age)
data.comp$age = data.comp$age - mean(data.comp$age)
data.surv$age = data.surv$age - mean(data.surv$age)


## ----model-complete-case-gee--------------------------------------------------
library(gee)
library(geepack)
library(glmtoolbox)
library(gtsummary)
library(tidyverse)
data.comp.lag <-
  data.comp %>% 
  group_by(subject_id) %>% 
  mutate(mem_comp_lag = lag(mem_comp), observed_lag = lag(observed)) %>% 
  ungroup() %>% 
  filter(observed_lag != 0) %>% 
  mutate(drop = ifelse(observed == 0 & observed_lag == 1, 1, 0)) %>% 
  dplyr::select(subject_id, day, day_fct, age, gender, treatment_group, mem_comp_lag, drop) %>% 
  mutate(day_fct = factor(day, levels = c(5,19,90)))


model.complete.1 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "independence")
model.complete.2 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id,  corstr = "ar1")
model.complete.3 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "exchangeable")

#tbl_regression(model.complete.1)
#tbl_regression(model.complete.2)
#tbl_regression(model.complete.3)

tbl_merge(
  list(tbl_regression(model.complete.1), tbl_regression(model.complete.2), tbl_regression(model.complete.3))
)


## ----model-test-mcar----------------------------------------------------------
model.drop <-
  geeglm(drop ~ day_fct + treatment_group + age + gender + mem_comp_lag, data = data.comp.lag, family = binomial, id = subject_id, corstr = "independence")

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


## ----model-ps-----------------------------------------------------------------
# propensity score models, using logistic regression and coxph model 
model.prob.drop <-
  geeglm(dropped ~ day_fct + treatment_group + age + gender, data = data.surv %>% mutate(day_fct = factor(day_fct,levels = c(5,19,90))), family = binomial, id = subject_id, corstr = "independence")

model.prob.drop.coxph <- coxph(Surv(start, stop, dropped) ~ treatment_group + age + gender, data = data.surv)

tbl_regression(model.prob.drop, exponentiate = F)
tbl_regression(model.prob.drop.coxph, exponentiate = F)

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


tbl_merge(
  list(tbl_regression(model.ipw.1), tbl_regression(model.ipw.2), tbl_regression(model.ipw.3))
)

## ----model-ipw-gee-coxph------------------------------------------------------
model.ipw.coxph.1 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "independence", weights = 1/prob.stay.coxph)
model.ipw.coxph.2 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id,  corstr = "ar1", weights = 1/prob.stay.coxph)
model.ipw.coxph.3 <- geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, data = data.comp, family = gaussian, id = subject_id, corstr = "exchangeable", weights = 1/prob.stay.coxph)


tbl_merge(
  list(tbl_regression(model.ipw.coxph.1), tbl_regression(model.ipw.coxph.2), tbl_regression(model.ipw.coxph.3))
)


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

