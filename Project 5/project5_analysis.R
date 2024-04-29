library(readxl)
library(mice)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(lme4)
library(GLMMadaptive)
library(glmmTMB)


Proj_4_data <- read_excel("./Proj 4 data.xlsx")

Proj_4_data$school <- as.factor(Proj_4_data$school)
Proj_4_data$group <- as.factor(Proj_4_data$group)
Proj_4_data$ID <- as.factor(Proj_4_data$ID)

## missing assume MAR
# use mice to impute and compare 3 different methods
# decide to use CART 

md.pattern(Proj_4_data)

mice_imputed <- data.frame(
  original = Proj_4_data$SFD,
  imputed_pmm = complete(mice(Proj_4_data, method = "pmm"))$SFD,
  imputed_cart = complete(mice(Proj_4_data, method = "cart"))$SFD,
  imputed_lasso = complete(mice(Proj_4_data, method = "lasso.norm"))$SFD
)
mice_imputed

h1 <- ggplot(mice_imputed, aes(x = original)) +
  geom_histogram(fill = "#ad1538", color = "#000000", position = "identity") +
  ggtitle("Original distribution") +
  theme_classic()
h2 <- ggplot(mice_imputed, aes(x = imputed_pmm)) +
  geom_histogram(fill = "#15ad4f", color = "#000000", position = "identity") +
  ggtitle("PMM-imputed distribution") +
  theme_classic()
h3 <- ggplot(mice_imputed, aes(x = imputed_cart)) +
  geom_histogram(fill = "#1543ad", color = "#000000", position = "identity") +
  ggtitle("CART-imputed distribution") +
  theme_classic()
h4 <- ggplot(mice_imputed, aes(x = imputed_lasso)) +
  geom_histogram(fill = "#ad8415", color = "#000000", position = "identity") +
  ggtitle("Lasso-imputed distribution") +
  theme_classic()

plot_grid(h1, h2, h3, h4, nrow = 2, ncol = 2)

Proj_4_data$SFD_imputed <- mice_imputed$imputed_cart

## Zero-Inflated Over-dispersion

# check zero-inflation
hist(Proj_4_data$SFD_imputed)
# check over-dispersion
mean(Proj_4_data$SFD_imputed)
var(Proj_4_data$SFD_imputed)


# standard negative binomial
mnb1 <- glmer.nb(SFD_imputed ~ group  + time +
                   (1 | school) + (1 | ID), data = Proj_4_data, control = glmerControl(optimizer = "bobyqa"))
overdisp_fun(mnb1)
summary(mnb1)

# zero inflated negative binomial with only fixed effects in the zero part
gm1 <- mixed_model(SFD_imputed ~ group + time, random = ~ 1 | ID:school, data = Proj_4_data, 
                   family = zi.negative.binomial(),zi_fixed = ~ group, control = list(iter_EM = 50))

gm1.zinb.1 <- glmmTMB(SFD_imputed ~ group + time + (1|ID:school) , 
                    zi = ~ group , 
                    data = Proj_4_data,
                    family = nbinom2, REML = T)

gm1.zinb.2 <- glmmTMB(SFD_imputed ~ group + time + (1|ID) + (1|school) , 
                    zi = ~ group , 
                    data = Proj_4_data,
                    family = nbinom2, REML = T)

summary(gm1)
summary(gm1.zinb.1)
summary(gm1.zinb.2)

hist(predict(gm1, newdata = Proj_4_data))
hist(predict(gm1.zinb.1))

Proj_4_data$SFD_imputed

# zero inflated negative binomial with both fixed and random effects in the zero part
gm2 <- mixed_model(SFD_imputed ~ group + time, random = ~ 1 | ID, data = Proj_4_data, 
                   family = zi.poisson(),zi_fixed = ~  group + time, zi_random = ~ 1 | ID, control = list(iter_EM = 50))

gm2.zinb.1 <- glmmTMB(SFD_imputed ~ group + time + (1|ID:school) , 
                      zi = ~ group + (1|ID:school), 
                      data = Proj_4_data,
                      family = nbinom2, REML = T, control=glmmTMBControl(optimizer=optim,
                                                                         optArgs=list(method="BFGS")))
summary(gm2)
summary(gm2.zinb.1)
hist(predict(gm2, newdata = Proj_4_data),freq =T)

# model selection
anova(gm1, gm2)
anova(gm2.zinb.1, gm1.zinb.1)
