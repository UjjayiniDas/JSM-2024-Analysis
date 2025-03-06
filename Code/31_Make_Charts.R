# *******************
# Plot RR Time Series
# *****************************************************************************



# :::::::::::::::::::::::::::::::::::::::::::::
# Print a Plot of Response Rates by Survey ----



df_resp_rates %>% 
  
  # Keep 1990+ and drop SIPP (for plotting)
  filter(Survey != "SIPP", DataYear >= 1990) %>% 
  
  # Make a ggplot object
  ggplot(data = ., aes(x = DataYear, y = ResponseRate, color = Survey)) +
  
  # Line plot
  geom_line(linewidth = 1) +
  
  # Add dots
  geom_point(size = 1.5) +
  
  # Minimalist theme
  theme_classic( ) + 
  
  # Axis scales
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10),
                     limits = c(20, 100)) +
  scale_x_continuous(breaks = seq(from = 1990, to = 2022, by = 2)) +
  
  # Apply colourblind palette
  scale_color_manual(values = generate_palette(13)) +
  
  # Theming
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  
  # Place legend items in 2 rows
  guides(colour = guide_legend(nrow = 2)) + 
  
  # Labels
  labs(y = "Response Rate (%)",
       x = "Data Year")


# ::::::::::::::::::::::::::::::::::
# Print a Plot of Trust by Type ----

df_GSS %>% 
  
  # Place trust in rows
  pivot_longer(
    col = 2:ncol(.),
    names_to = "TrustType",
    values_to = "Trust"
  ) %>% 
  
  # Remove the NA's 
  na.omit(.) %>% 
  
  # Categorize the trust types
  mutate(Category = case_when(
    grepl("conpersonal|conclerg", TrustType) ~ "Social Institutions",
    grepl("confinan|conbus|conlabor", TrustType) ~ "Economic Institutions",
    grepl("confed|conlegis|conjudge|conarmy", TrustType) ~ "Government Institutions",
    grepl("conpress|contv", TrustType) ~ "Media and Press",
    grepl("coneduc|consci|conmedic", TrustType) ~ "Education, Science, and Medicine"),
    TrustLabel = str_replace_all(TrustType,  Trust_Names)
  ) %>% 
  
  # Start ggplot object
  ggplot(data = ., aes(x = year, y = 100*Trust, color = TrustLabel)) +
  
  # Time series plot
  geom_line(linewidth = 1) +
  
  # Add dots
  geom_point(size = 2) +
  
  # Apply colourblind palette
  scale_color_manual(values = generate_palette(14)) +
  
  # Minimalist theme
  theme_classic( ) + 
  
  # Theming
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  
  # Place legend items in 2 rows
  guides(colour = guide_legend(nrow = 2)) + 
  
  # Subplots by category
  facet_wrap(~Category, scales = "free_y") +
  
  # Plot labels  
  labs(y = "Trust (%)",
       x = "Year")

### EOF ###