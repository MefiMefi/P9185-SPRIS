
library(tidyverse)
library(sampleSelection)


data("RandHIE", package = "sampleSelection")

# insurance plan categories
# catastrophic = 2,3,4,14,15,16
# free = 11

rand <- RandHIE %>% 
  mutate(plantype = case_when(plan %in% c(2,3,4,14,15,16) ~ "Catastrophic",
                              plan == 11 ~ "Free") %>% as_factor) %>%
  # select only first year observation for each person
  group_by(zper) %>% slice(1) %>%
  select(xage,      # age in years
         female,    # 1 if person is female
         black,     # 1 if race of household head is black
         educdec,   # education of household head in years
         disea,     # number of chronic diseases
         #ghindx,    # general health index
         meddol,    # all covered medical expenses
         plantype) %>%
  # remove observations with missing values
  drop_na()

# calculate the propensity score
ps <- predict(glm(plantype ~ xage + female + black + educdec + disea, family = "binomial", data = rand), type = "response") %>% as.numeric

# calc Ey
m <- glm(meddol ~ plantype + xage + female + black + educdec + disea, family = "gaussian", data = rand)
# calc ey1
ey1 <- predict(m, newdata = rand %>% mutate(plantype="Free"), type = "response") %>% as.numeric
# calc ey0
ey0 <- predict(m, newdata = rand %>% mutate(plantype="Catastrophic"), type = "response") %>% as.numeric


# aipw estimator
aipw <- function(a, y, ps, ey1, ey0) {
  mean( ( (a*y)/ps - (1-a)*y/(1-ps) ) - (a-ps)/(ps*(1-ps)) * ( (1-ps)*ey1 + ps*ey0 ) )
}

aipw(a = as.numeric(rand$plantype)-1, ps = ps, y = rand$meddol, ey1 = ey1, ey0 = ey0)


# ipw estimator
ipw <- function(a, y, ps) {
  mean(a*y/ps - (1-a)*y/(1-ps))
}

ipw(a = as.numeric(rand$plantype)-1, y = rand$meddol, ps = ps)


# regression estimate
reg <- mean(ey1 - ey0)
reg


#### 2. example  NHIS

library(tidyverse)
data("NHIS2009", package = "masteringmetrics")
# install with
# devtools::install_github("jrnold/masteringmetrics", subdir = "masteringmetrics")


nhis <- NHIS2009 %>%
  # only include married adults between 26 and 59 in age and
  # remove single households
  filter(between(age, 26, 59),
         marradult, adltempl >= 1) %>%
  select(age,        # age
         fml,        # female yes/no
         yedu,       # years of education
         famsize,    # family size
         empl,       # employment status
         inc,        # houshold income
         health,     # health status
         uninsured)  # insurance status


# calculate the propensity score
ps <- predict(glm(uninsured-1 ~ age + fml + yedu + famsize + empl + inc, family = "binomial", data = nhis), type = "response") %>% as.numeric

# calc Ey
m <- glm(health ~ uninsured + age + fml + yedu + famsize + empl + inc, family = "gaussian", data = nhis)
# calc ey1
ey1 <- predict(m, newdata = nhis %>% mutate(uninsured=1), type = "response") %>% as.numeric
# calc ey0
ey0 <- predict(m, newdata = nhis %>% mutate(uninsured=0), type = "response") %>% as.numeric


# aipw estimator
aipw <- function(a, y, ps, ey1, ey0) {
  mean( ( (a*y)/ps - (1-a)*y/(1-ps) ) - (a-ps)/(ps*(1-ps)) * ( (1-ps)*ey1 + ps*ey0 ) )
}

aipw(a = nhis$uninsured-1, ps = ps, y = nhis$health, ey1 = ey1, ey0 = ey0)


# regression estimate
reg <- mean(ey1 - ey0)
reg


# ipw estimator
ipw <- function(a, y, ps) {
  mean(a*y/ps - (1-a)*y/(1-ps))
}

ipw(a = nhis$uninsured-1, y = nhis$health, ps = ps)


################################
# calculate bootstrap confidence intervals

library(rsample)

set.seed(1)
nboot <- 100  # number of bootstrap iterations

# creat bootstrapped data sets
nhis_boot <- bootstraps(nhis, times = nboot)

# empty data frame for the results
res_boot <- data.frame(matrix(ncol=3,nrow=0))

# calculate estimators for all bootstrapped data sets
for (i in 1:nboot) {
  nhis <- analysis(nhis_boot$splits[[i]])
  head(nhis)
  ps <- predict(glm(uninsured-1 ~ age + fml + yedu + famsize + empl + inc, family = "binomial", data = nhis), type = "response") %>% as.numeric
  m <- glm(health ~ uninsured + age + fml + yedu + famsize + empl + inc, family = "gaussian", data = nhis)
  ey1 <- predict(m, newdata = nhis %>% mutate(uninsured=1), type = "response") %>% as.numeric
  ey0 <- predict(m, newdata = nhis %>% mutate(uninsured=0), type = "response") %>% as.numeric
  res <- c(aipw(a = nhis$uninsured-1, ps = ps, y = nhis$health, ey1 = ey1, ey0 = ey0),
           ipw(a = nhis$uninsured-1, y = nhis$health, ps = ps),
           mean(ey1 - ey0))
  res_boot <- rbind(res_boot, res)
}

colnames(res_boot) <- c("aipw", "ipw", "reg")

# calculate the percentiles for each estimator
quantile(res_boot$aipw, probs = c(0.025, 0.975))
quantile(res_boot$ipw, probs = c(0.025, 0.975))
quantile(res_boot$reg, probs = c(0.025, 0.975))






