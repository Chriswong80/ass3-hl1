# R Code for ANOVA and Boxplot of Part Resistance by Pressure*Temperature (Machine 1)
library(tidyverse)
library(plotly)

# Filter data for Machine 1
machine1_data_pt <- X031.assignment.2 %>% filter(Machine == 1)

# Convert Pressure and Temperature to factors
machine1_data_pt$Pressure_Factor <- as.factor(machine1_data_pt$Pressure)
machine1_data_pt$Temperature_Factor <- as.factor(machine1_data_pt$Temperature)

# Perform ANOVA for interaction effect
anova_result_pt <- aov(PartResistance ~ Pressure_Factor * Temperature_Factor, data = machine1_data_pt)
cat("ANOVA Summary for Part Resistance by Pressure*Temperature (Machine 1):\n")
print(summary(anova_result_pt))

# Generate boxplot
p_pt <- machine1_data_pt %>% 
  ggplot(aes(x = interaction(Pressure_Factor, Temperature_Factor, sep = ":"), y = PartResistance, fill = interaction(Pressure_Factor, Temperature_Factor, sep = ":"))) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 2) +
  geom_hline(yintercept = 10, linetype = "dashed", color = "#D55E00", linewidth = 1) + # USL
  labs(
    title = "Boxplot of Part Resistance by Pressure and Temperature (Machine 1)",
    x = "Pressure:Temperature Combination",
    y = "Part Resistance"
  ) +
  scale_fill_manual(values = c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#0072B2")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

plotly_plot_pt <- ggplotly(p_pt)
saveWidget(plotly_plot_pt, file = "media/plots/anova_part_resistance_by_pressure_temperature_machine1.html", selfcontained = TRUE)
