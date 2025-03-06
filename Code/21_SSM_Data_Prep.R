# *****************
# Prep Data for SSM
# ******************************************************************************

# ****************************************
# Assemble Response Rates Time Series ----
df_RRs <- df_resp_rates %>% 
  # Subset rows
  filter(DataYear >= 2000 & DataYear < 2023) %>% 
  # Subset columns
  select(DataYear, Survey, ResponseRate)%>% 
  # Place surveys in columns
  pivot_wider(
    names_from = Survey,
    values_from = ResponseRate
  ) 


# *******************************
# Assemble Trust Time Series ----
df_Trust_Series <- df_GSS %>% 
  # Subset rows
  filter(year %in% 2000:2022) %>% 
  # Rename year
  rename(DataYear = year) %>% 
  # Complete the data frame
  complete(DataYear = min(DataYear):max(DataYear))
  

### EOF ###