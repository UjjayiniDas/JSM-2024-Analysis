# ************
# Prepare Data
# *****************************************************************************

weighted_mean_na <- function(x, w, na.rm = TRUE) {
  # Check if all values are NA
  if (all(is.na(x))) {
    return(NA)
  }
  else {
    # Calculate weighted mean, removing NAs if na.rm = TRUE
    return(weighted.mean(x, w, na.rm = na.rm)) 
  }
}


# :::::::::::::::::::::::
# GSS Trust Measures ----

df_GSS <- readxl::read_excel(
  path = "Data/GSS.xlsx",
  sheet = 1) %>% 
  
  # Recode variables
  mutate(
    
    # Recode confidence in ... with NA's
    across(.cols = conpersonal:conarmy,
           ~{case_when(
             grepl("^\\.", .x) ~ NA,
             grepl("Can trust|A GREAT DEAL", .x) ~ 1,
             TRUE ~ 0)})
    ) %>% 
  
  # Tabulate the shares
  reframe(.by = year, 
          across(conpersonal:conarmy,
                 ~weighted_mean_na(.x, w = wtssall, na.rm = TRUE)))
  

# :::::::::::::::::::
# Response Rates ----

df_resp_rates <- readxl::read_excel(
  path = "Data/Response_Rates_Data.xlsx",
  sheet = 1) %>% 
  
  # Convert to numeric
  mutate(ResponseRate = as.numeric(ResponseRate)) %>% 
  
  #
  arrange(Survey, DataYear)




# ::::::::::::::::::::::::::::::
# Prepare the Panel Dataset ----


df_panel <- left_join(
  # Response rates
  df_resp_rates,
  
  # GSS variables
  df_GSS %>%
    # Subset variables + rename
    select(DataYear           = year, 
           `Socioeconomic`    = confed, 
           `Health`           = conmedic, 
           `Criminal Justice` = conjudge,
           `Politics`         = conlegis) %>% 
    # Place in rows
    pivot_longer(
      cols      = Socioeconomic:Politics,
      names_to  = "Domain",
      values_to = "Trust") %>% 
    # Re-scale trust
    mutate(Trust = 100*Trust),
  
  # Joining variables
  by = join_by(DataYear, Domain)
  
  ) %>% 
  
  # Sort data
  arrange(Survey, DataYear)


# **********************
# Prep Data for SSM ----

df_data <- df_resp_rates %>% 
  # Subset data
  select(year = DataYear,
         Survey,
         RR = ResponseRate) %>% 
  # Surveys in columns (for SSM)
  pivot_wider(names_from = Survey,
              values_from = RR) %>% 
  # Keep 1990+
  filter(year >= 1990) %>% 
  # Add in GSS vars
  left_join(df_GSS, by = join_by(year))%>% 
  # Sort by year
  arrange(year)


### EOF ###