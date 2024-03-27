library(visdat)
library(mice)
library(VIM)
library(lattice)
library(ggplot2)
library(miceadds)
library(formattable)
library(graphics)


## Setup
data.full <- read.csv("./data.csv") %>% janitor::clean_names() %>% 
  mutate(
    day_fct = factor(day, levels = c(0,5,19,90)),
    gender = factor(gender, levels = c("M", "F")),
    treatment_group = factor(treatment_group, levels = c("A","B","C"))
  )
# Create a complete grid of all possible combinations of subject_id and day
data.all <- expand.grid(subject_id = unique(data.full$subject_id), 
                        day = c(0, 5, 19, 90))

# Merge the complete data with existing data
data.comp <- 
  merge(data.all , data.full , by = c("subject_id", "day"), all.x = TRUE) %>% 
  group_by(subject_id) %>% 
  fill(age:treatment_group, .direction = "downup") %>% 
  ungroup() %>% 
  mutate(
    observed = as.numeric(!is.na(mem_comp)),
    day_fct = factor(day, levels = c(0,5,19,90))
  )

## Tests
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
temp <- matrix(c(58, 46, 54, 10, 10, 10),nrow=3,ncol=2)
chisq.test(temp,correct=FALSE)

## Graph
imp <- mice(data.comp, m = 5)
data.imp <- complete(imp, "long", inc = TRUE)
cdplot(factor(observed, levels = 0:1, labels = c("Missing", "Observed")) ~ mem_comp, data = data.imp, ylab = "")

## Delta-adjustment
delta_MNAR_f = function(delta){
  
  MAR_step = mice(data.comp, m=5, method = "pmm", quickpred(data.comp, exc = "observed"), seed = 20)
  
  MNAR_conversion_delta = 
    complete(MAR_step, action='long', include=TRUE) %>%
    mutate(mem_comp = ifelse(observed == 0, mem_comp + delta, mem_comp))
  
  MNAR_conversion_delta <- MNAR_conversion_delta %>% 
    arrange(.imp, subject_id, day) %>% 
    group_by(.imp, subject_id) %>% 
    mutate(baseline_mem_comp = first(mem_comp)) %>% 
    mutate(mem_comp_delta = mem_comp - baseline_mem_comp, na.rm = T)
  
  MNAR_conversion_delta <- MNAR_conversion_delta %>% filter(day > 0)%>% mutate(day_fct = factor(day_fct, levels = c(5,19,90)))
  
  MNAR_data_delta = as.mids(MNAR_conversion_delta)
  
  # output_delta =
  #   with(MNAR_data_delta, geeglm(mem_comp ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, family = gaussian, id = subject_id, corstr = "independence"))
  output_delta =
    with(MNAR_data_delta, geeglm(mem_comp_delta ~ day_fct + treatment_group + age + gender + treatment_group * day_fct, family = gaussian, id = subject_id, corstr = "independence"))
  
  params_delta = 
    summary(pool(output_delta)) %>%
    filter(term == "treatment_groupC") 
  
  desc_delta = with(MNAR_data_delta, expr=c("Y_mean"=mean(mem_comp), "Y_sd"=stats::sd(mem_comp)))
  desc_delta = withPool_MI(desc_delta) %>% as.data.frame() %>% t()
  
  cbind(params_delta, desc_delta)
  
}

  # Set range of plausible delta values
  delta_inputs = c( -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1.2, 2,5)
  
  # Map output
  output_delta = 
    map_df(delta_inputs, delta_MNAR_f)
  
  rownames(output_delta) <- c()
  
  #Output
  output_formatted_delta = 
    cbind(delta_inputs, output_delta) %>%
    select(-term) %>%
    mutate(across(c("estimate":"std.error"), round, 3),
           #across(c("statistic":"df", "Y_mean":"Y_sd"), round, 2),
           p.value = scientific(p.value, digits = 2))
  
  output_formatted_delta %>%
    knitr::kable(bootstrap_options = "condensed")
  