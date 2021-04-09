library(dplyr)
library(lfe)
library(stargazer)


### Normal Conjoint Function ####
make_table_conjoint <- function(RHS,
                                outcome_vars,
                                round,
                                outcome_labels,
                                treatment_labels,
                                table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(hesitancy_long, round == 1),
           weights = subset(hesitancy_long, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(hesitancy_long, round == 2),
           weights = subset(hesitancy_long, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(hesitancy_long[,y], na.rm = TRUE),3), "-", round(max(hesitancy_long[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- hesitancy_long %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models,
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}

### Conjoint function - Confidence Intervals ####  
make_table_conjoint_ci <- function(RHS,
                                outcome_vars,
                                round,
                                outcome_labels,
                                treatment_labels,
                                table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(hesitancy_long, round == 1),
           weights = subset(hesitancy_long, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(hesitancy_long, round == 2),
           weights = subset(hesitancy_long, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(hesitancy_long[,y], na.rm = TRUE),3), "-", round(max(hesitancy_long[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- hesitancy_long %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models,
            ci = T, ci.level = 0.95,
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}




##### Conjoint Function - Catholic #### 
make_table_conjoint_cath <- function(RHS,
                                             outcome_vars,
                                             round,
                                             outcome_labels,
                                             treatment_labels,
                                             table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(catholic, round == 1),
           weights = subset(catholic, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(catholic, round == 2),
           weights = subset(catholic, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = catholic,
           weights = catholic$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = catholic,
           weights = catholic$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(catholic[,y], na.rm = TRUE),3), "-", round(max(catholic[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- catholic %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models, 
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}

##### Conjoint Function - Evangelical ####
make_table_conjoint_evangelical <- function(RHS,
                                             outcome_vars,
                                             round,
                                             outcome_labels,
                                             treatment_labels,
                                             table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(evangelical, round == 1),
           weights = subset(evangelical, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(evangelical, round == 2),
           weights = subset(evangelical, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = evangelical,
           weights = evangelical$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = evangelical,
           weights = evangelical$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(evangelical[,y], na.rm = TRUE),3), "-", round(max(evangelical[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- evangelical %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models, 
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}

##### Conjoint Function - Most Hesitant #### 
make_table_conjoint_most_hes <- function(RHS,
                                             outcome_vars,
                                             round,
                                             outcome_labels,
                                             treatment_labels,
                                             table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(most_hes, round == 1),
           weights = subset(most_hes, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(most_hes, round == 2),
           weights = subset(most_hes, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = most_hes,
           weights = most_hes$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = most_hes,
           weights = most_hes$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(most_hes[,y], na.rm = TRUE),3), "-", round(max(most_hes[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- most_hes %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models, 
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}
##### Conjoint Function - Less Hesitant #### 
make_table_conjoint_not_most_hes <- function(RHS,
                                   outcome_vars,
                                   round,
                                   outcome_labels,
                                   treatment_labels,
                                   table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(not_most_hes, round == 1),
           weights = subset(not_most_hes, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             "+ std_months_pre | as.factor(fixed_effects) | 0 | responseid")),
           data = subset(not_most_hes, round == 2),
           weights = subset(not_most_hes, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | factor(responseid) + as.factor(round) | 0 | responseid")),
           data = not_most_hes,
           weights = not_most_hes$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | std_months_pre + as.factor(fixed_effects) + as.factor(round) | 0 | responseid")),
           data = not_most_hes,
           weights = not_most_hes$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(not_most_hes[,y], na.rm = TRUE),3), "-", round(max(not_most_hes[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- not_most_hes %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models, 
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range),
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}


##### Conjoint for Balance Test ##### 
make_table_conjoint_balance <- function(RHS,
                                outcome_vars,
                                round,
                                outcome_labels,
                                treatment_labels,
                                table_name) {
  
  if (round == "1") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | as.factor(country_num) | 0 | 0")),
           data = subset(hesitancy_long, round == 1),
           weights = subset(hesitancy_long, round == 1)$w_conjoint,
           cmethod = 'reghdfe')
    )
    
  } else if (round == "2") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | as.factor(country_num) | 0 | 0")),
           data = subset(hesitancy_long, round == 2),
           weights = subset(hesitancy_long, round == 2)$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | as.factor(country_num) | 0 | 0")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  } else if (round == "full_het") {
    models <- lapply(outcome_vars, function(y)
      felm(as.formula(paste0(y,
                             " ~ ",
                             RHS,
                             " | as.factor(country_num) | 0 | 0")),
           data = hesitancy_long,
           weights = hesitancy_long$w_conjoint,
           cmethod = 'reghdfe')
    )
  }
  
  outcome_range <- as.character(sapply(outcome_vars, function(y)
    paste0(round(min(hesitancy_long[,y], na.rm = TRUE),3), "-", round(max(hesitancy_long[,y], na.rm = TRUE),3))
  ))
  
  baseline_expression <- paste0(gsub(" \\+ ", " == 0 \\& ", RHS), " == 0")
  
  baseline_expression <- rlang::parse_expr(baseline_expression)
  
  outcome_stats <- hesitancy_long %>%
    filter(!!baseline_expression) %>%
    summarize(across(
      all_of(outcome_vars),
      .fns = list(Mean = mean, SD = sd), na.rm = TRUE))
  
  outcome_stats <- do.call(cbind, outcome_stats)
  
  stargazer(models,
            model.names = FALSE,
            dep.var.labels.include = FALSE,
            column.labels = outcome_labels,
            covariate.labels = treatment_labels,
            type = "latex",
            omit = c('Constant', 'fixed_effects', 'responseid', 'round', 'std_months_pre'),
            omit.stat = c('f', 'adj.rsq', 'ser'),
            add.lines = list(c("Fixed Effects", rep("Yes",length(outcome_labels))),
                             c("Outcome Range", outcome_range), 
                             c("Control Mean", round(as.numeric(outcome_stats[seq(from = 1, to = length(outcome_stats), by = 2)]),3)),
                             c("Control SD", round(as.numeric(outcome_stats[seq(from = 2, to = length(outcome_stats), by = 2)]),3))),
            out = paste0(table_name, ".tex"))
  
}






