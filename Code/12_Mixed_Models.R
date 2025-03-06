# *******************
# Linear Mixed Models 
# *****************************************************************************

# Setup data w/ trust as common factor
df_data_common <- df_data %>% 
  # Response rates in rows
  pivot_longer(cols      = NSDUH:ANES,
               names_to  = "Survey",
               values_to = "RR")


# ::::::::::::::::::::::::::::::
# Common Factor Regressions ----

# Variable names
trust_measures <- names(df_data_common)[2:15]

# Empty list
regs_list <- list()

# GLMM for each variable
for(var in trust_measures) {
  # Regression formula
  fml <- as.formula(
    glue::glue("scale(RR) ~ 1 + {var} + year + ({var} || Survey)")
  )
  # Collect results
  regs_list[[var]] <- lmer(formula = fml,
                           data = df_data_common, 
                           REML = TRUE,
                           na.action = "na.omit")
  
  
}

# Print results
stargazer::stargazer(regs_list, type = "text")


# :::::::::::::::::::::::::::::
# Pooled Domain Regression ----

# Treating survey domain as a category
mdl <- lmer(
  ResponseRate ~ 1 + Domain:Trust + (Domain:Trust || Survey),
  data = df_panel, 
  na.action = "na.omit") 


# Print results
stargazer::stargazer(mdl, type = "text")

### EOF ###