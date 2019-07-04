## college revenue (% of GDP)
## data_clean is from Github/Ming folder

college_revenue <- data_clean %>%
  ggplot(aes(x = Academic_Year, y = Income_per_GDP, color = Party)) +
  geom_line(aes(color=Party, group=1), size = 1.5) +
  labs(title = "College Revenue (1987 - 2013)",
       x = "Academic Year",
       y = "College Revenue (% of GDP)") + 
  scale_color_manual(values=c(Democrat="#3366FF", Republican="#FF6633")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.5, color = "Grey"),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 15, face = "italic")) +
  annotate("rect", xmin=1990, xmax=1991, ymin=-Inf, ymax=Inf, alpha=0.3) +
  annotate("rect", xmin=2000.5, xmax=2001.5, ymin=-Inf, ymax=Inf, alpha=0.3) +
  annotate("rect", xmin=2008, xmax=2010, ymin=-Inf, ymax=Inf, alpha=0.3)