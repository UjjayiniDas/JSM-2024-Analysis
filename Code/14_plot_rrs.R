

cb_palette_extended <- c(
  "#D55E00", "#E69F00", "#009E73", "#56B4E9", "#0072B2", 
  "#F0E442", "#CC79A7", "#999999", "#000000", "#0099FF", 
  "#66CC99", "#6600CC"
)



df_resp_rates %>% 
  
  #
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
  scale_color_manual(values = cb_palette_extended) +
  
  # Theming
  theme(legend.position = "top",
        legend.title = element_blank()) + 
  
  # Place legend items in 2 rows
  guides(colour = guide_legend(nrow = 2)) + 
  
  # Labels
  labs(y = "Response Rate (%)",
       x = "Data Year")
  
