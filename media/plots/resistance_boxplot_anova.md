
# Filter data for Machine == 1
df_machine1 <- subset(X031.assignment.2, Machine == 1)

# Convert Pressure to a factor
df_machine1$Pressure <- as.factor(df_machine1$Pressure)

# Perform ANOVA
anova_model <- aov(PartResistance ~ Pressure, data = df_machine1)
# summary(anova_model) # Output is shown in the R cell above

# Create boxplot
library(ggplot2)
library(plotly)
library(htmlwidgets)

# Define USL
USL <- 10

p <- ggplot(df_machine1, aes(x = Pressure, y = PartResistance, fill = Pressure)) +
  geom_boxplot() +
  geom_hline(yintercept = USL, linetype = 'dashed', color = 'red', size = 1) +
  annotate('text', x = Inf, y = USL, label = 'USL = 10', vjust = -0.5, hjust = 1, color = 'red', size = 5, fontface = 'bold') +
  labs(
    title = 'Part Resistance by Pressure (Machine 1)',
    x = 'Pressure',
    y = 'Part Resistance'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = 'bold'),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = 'none'
  ) +
  scale_fill_manual(values = c('#0072B2', '#D55E00', '#009E73', '#CC79A7'))

plotly_plot <- ggplotly(p)
htmlwidgets::saveWidget(plotly_plot, file = 'media/plots/resistance_boxplot_anova.html', selfcontained = TRUE)

