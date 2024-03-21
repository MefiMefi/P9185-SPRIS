## ----setup, include=FALSE--------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------
source(knitr::purl("./data_manipulation.Rmd", quiet=TRUE))
theme_set(theme_bw())
library(tidyverse)
library(patchwork)


## --------------------------------------------------------------------------------------------------------
table(data.full$treatment_group, data.full$day)


## ----eda-traj--------------------------------------------------------------------------------------------
traj.plot<-
data.full %>% 
  ggplot(aes(x = day_fct, y = mem_comp)) +
  geom_line(alpha = 0.3, linewidth = 0.5, aes(group = subject_id, color = treatment_group)) +
  facet_grid(.~treatment_group) + 
  geom_smooth(color="blue", method="lm", aes(group = 1), se = FALSE) +
  ylab("Composite memory score")+
  xlab("Days of observation")+
  theme(legend.position = "none")
traj.plot

traj.boxplot<-
data.full %>%
  group_by(day_fct, treatment_group) %>%
  mutate(mean_mem_comp = mean(mem_comp, na.rm = TRUE)) %>%
  ungroup() %>% 
  ggplot(aes(x = day_fct, y = mem_comp, color = treatment_group)) +
  geom_boxplot(alpha = 0.3, linewidth = 0.5) +
  #geom_line(aes(x = day_fct, y = mean_mem_comp, group = treatment_group), size = 0.5, alpha = 0.8) +
  labs(color = "Treatment Group") +
  ylab("Composite memory score")+
  xlab("Days of observation")
traj.boxplot

ggsave("./plots/trajectory_plot.jpg", traj.plot+traj.boxplot, width = 24, height = 12, dpi = 300)


## ----eda-traj-comp-drop----------------------------------------------------------------------------------
mean.score.plot <-
data.comp %>% 
  group_by(subject_id) %>% 
  mutate(total_obs = sum(as.numeric(observed))) %>% 
  ungroup() %>% 
  mutate(type = as.factor(ifelse(total_obs == 4, "Completer", "Drop-out"))) %>% 
  group_by(type, day_fct, treatment_group) %>% 
  mutate(mean_mem_comp = mean(mem_comp,na.rm = T)) %>% 
  dplyr::select(day_fct, treatment_group, type, mean_mem_comp) %>% 
  distinct() %>% 
  ungroup() %>% 
  ggplot(aes(x = day_fct, y = mean_mem_comp, group = interaction(treatment_group, type), color = treatment_group, linetype = type)) +
  geom_line(alpha = 0.8, size = 0.5) +
  scale_linetype_manual(values = c("Completer" = "solid", "Drop-out" = "dashed")) +
  labs(color = "Treatment Group", linetype = "Type") +
  ylab("Composite memory score")+
  xlab("Days of observation")+
  theme_bw()

mean.score.plot
ggsave("./plots/mean_score_plot.jpg", mean.score.plot, width = 12, height = 8, dpi = 300)


## --------------------------------------------------------------------------------------------------------
mean.score.boxplot <-
data.comp %>% 
  group_by(subject_id) %>% 
  mutate(total_obs = sum(as.numeric(observed))) %>% 
  ungroup() %>% 
  mutate(type = as.factor(ifelse(total_obs == 4, "Completer", "Drop-out"))) %>% 
  ggplot(aes(x = day_fct, y = mem_comp, color = treatment_group, linetype = type)) +
  geom_boxplot() +
  labs(color = "Treatment Group", linetype = "Type") +
  ylab("Composite memory score")+
  xlab("Days of observation")+
  theme_bw()

mean.score.boxplot
ggsave("./plots/mean_score_boxplot.jpg", mean.score.boxplot, width = 12, height = 8, dpi = 300)


## --------------------------------------------------------------------------------------------------------
library(survival)
library(ggsurvfit)
library(survminer)

data.surv <-
  data.comp %>% 
  dplyr::select(subject_id, day_fct, observed, treatment_group, age, gender) %>% 
  mutate(dropped = 1-observed) %>% 
  distinct() %>% 
  dplyr::select(-observed) %>% 
  mutate(stop = as.numeric(day_fct)-1) %>% 
  group_by(subject_id) %>% 
  mutate(start = lag(stop)) %>% 
  drop_na(start)

surv_object <- Surv(data.surv$start, data.surv$stop, data.surv$dropped)

surv_fit <- survfit(surv_object ~ treatment_group, data = data.surv)

stay.plot <-
ggsurvplot(
  surv_fit,
  data = data.surv,
  xlab = "Days",
  ylab = "Proportion remaining in the study",
  ggtheme = theme_bw(),
  #risk.table = "nrisk_cumcensor",
  #tables.height = 0.3,
  xlim = c(0,3.5),
  ylim = c(0.30,1),
  break.x.by = 1,
  #axes.offset = T,
  #risk.table.title = "",
  ncensor.plot = T,
  ncensor.plot.height = 0.5,
  ncensor.plot.title = "",
  legend.title = "Treatment Group", # legend title
  legend.labs = c("A", "B", "C")
)
#stay.plot <- stay.plot$plot + scale_x_continuous(labels = c("0", "5", "19", "90"))

stay.plot
ggsave("./plots/stay_plot.jpg", (stay.plot$plot + scale_x_continuous(breaks = c(0,1,2,3),labels = c(0,5,19,90)) )/(stay.plot$ncensor.plot + theme(legend.position = "none")+ scale_x_continuous(breaks = c(0,1,2,3), labels = c(0,5,19,90))), width = 12, height = 8, dpi = 300)


table(data.full$treatment_group, data.full$day) %>% as.data.frame() %>% 
  group_by(Var1) %>% 
  mutate(Prop = Freq / first(Freq)) %>% 
  ggplot(aes(x = Var2, y = Prop, group = Var1, color = Var1)) +
  geom_step(direction = "vh") + 
  geom_point()

## --------------------------------------------------------------------------------------------------------
data.comp %>%
  ggplot(aes(x = day_fct, y = mem_comp)) +
  naniar::geom_miss_point()+
  facet_wrap(~treatment_group)
naniar::gg_miss_var(data.comp, show_pct = TRUE)

