
setwd("C:/Users/danie/Documents/Online for git/Fish-biomass-catches/Outputs-by-region/")

library(tidyverse)

# plot biomass tonnes_km2

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

df_long <- map_dfr(file_list, ~ {
  read_csv(.x) %>%
    pivot_longer(
      cols = c("Stckbio_SUR_MTkm2", "Stckbio_STO_MTkm2", "Tbio_SUR_MTkm2"),
      names_to = "measure",
      values_to = "value"
    )
})

df_summary <- df_long %>%
  group_by(Region, measure) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

EwE <- data.frame(
  Region = c("North Sea", "Gulf of Alaska", "Celtic-Biscay Shelf", "East Bering Sea"),
  measure = "EwE",
  median = c(18.08, 43.0, 13.50, 53.5),
  lower = NA, upper = NA
)

df_summary <- rbind(df_summary, EwE)

df_summary <- df_summary %>%
  group_by(Region) %>%
  mutate(mean_median = mean(median, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mean_median) %>%
  mutate(Region = factor(Region, levels = unique(Region)))

df_summary <- df_summary %>%
  mutate(measure = recode(measure,
                          "Stckbio_SUR_MTkm2" = "Stock biomass survey",
                          "Stckbio_STO_MTkm2" = "Stock biomass assessment",
                          "Tbio_SUR_MTkm2" = "Total survey biomass",
                          "EwE" = "Total EwE biomass"
  ))

# --- Split into two plots ---

# Plot 1: Total biomass and EwE
df_plot1 <- df_summary %>%
  filter(measure %in% c("Total survey biomass", "Total EwE biomass"))

p1 <- ggplot(df_plot1, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "Total and EwE Biomass by Region",
       x = "Region (sorted)",
       y = "Tonnes/km²",
       color = "Metric") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 150)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Stock biomass survey and assessment
df_plot2 <- df_summary %>%
  filter(measure %in% c("Stock biomass survey", "Stock biomass assessment"))

p2 <- ggplot(df_plot2, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "Stock Biomass (Survey vs Assessment)",
       x = "Region (sorted)",
       y = "Tonnes/km²",
       color = "Metric") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 50)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display plots
combined_plot <- cowplot::plot_grid(p2, p1, labels = c("A", "B"), ncol = 2, align = "v")
combined_plot
ggsave("../Outputs/combined_biomass_plots.pdf", combined_plot,
       width = 12, height = 5, units = "in")

# -----------------------------------------------------------------------------
# now get ER

# Step 1: Load all region CSV files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Step 2: Combine and reshape to long format
df_long <- map_dfr(file_list, ~ {
  read_csv(.x) %>%
    pivot_longer(
      cols = c("ER_SAU_SUR", "ER_WAT_SUR", "ER_STO"),
      names_to = "measure",
      values_to = "value"
    )
})

df_filtered <- df_long %>%
  #filter(Year < 2000)
  filter(Year >= 2005, Year <= 2010)

# Step 3: Summarise for each region × measure
df_summary <- df_filtered %>%
  group_by(Region, measure) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Sort regions by average of the 3 medians
df_summary <- df_summary %>%
  group_by(Region) %>%
  mutate(mean_median = mean(median,na.rm=T)) %>%
  ungroup() %>%
  arrange(mean_median) %>%
  mutate(Region = factor(Region, levels = unique(Region)))

# rename
df_summary <- df_summary %>%
  mutate(measure = recode(measure,
                          "ER_SAU_SUR" = "SAU catch/total suvey biomass",
                          "ER_WAT_SUR" = "Watson catch/total suvey biomass",
                          "ER_STO"    = "Stock data catch/biomass"
  ))

# Step 5: Plot
ER <- ggplot(df_summary, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "Exploitation rate by Region",
       x = "Region (sorted from low to high)",
       y = "Exploitation rate",
       color = "Metric") +
  theme_minimal() + ylim(0,1)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ER
ggsave("../Outputs/Exploitation_rate.pdf", ER,
       width = 7, height = 5, units = "in")

# ---------------------------------------------------------------------------
# get perc demersal

# Step 1: Load all region CSV files
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Step 2: Combine and reshape to long format
df_long <- map_dfr(file_list, ~ {
  read_csv(.x) %>%
    pivot_longer(
      cols = c("pcdem_SAU", "pcdem_WAT", "pcdem_SUR"),
      names_to = "measure",
      values_to = "value"
    )
})

df_filtered <- df_long %>%
  #filter(Year < 2000)
  filter(Year >= 2005, Year <= 2010)

# Step 3: Summarise for each region × measure
df_summary <- df_filtered %>%
  group_by(Region, measure) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Sort regions by average of the 3 medians
df_summary <- df_summary %>%
  group_by(Region) %>%
  mutate(mean_median = mean(median,na.rm=T)) %>%
  ungroup() %>%
  arrange(mean_median) %>%
  mutate(Region = factor(Region, levels = unique(Region)))

# rename
df_summary <- df_summary %>%
  mutate(measure = recode(measure,
                          "pcdem_SAU" = "SAU data",
                          "pcdem_WAT" = "Watson data",
                          "pcdem_SUR" = "Survey data"
  ))

# Step 5: Plot
ggplot(df_summary, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "Percentage demersal by Region",
       x = "Region (sorted from low to high)",
       y = "Perc. demersal fish versus total fish",
       color = "Metric") +
  theme_minimal() + ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


# now some final checks
df_long <- map_dfr(file_list, ~ {
  read_csv(.x)})

tr <- aggregate(df_long$Land_SAU_tonnes,by=list(df_long$Region),FUN=mean)
sum(tr[,2])/10^6/(80*0.85)*100 # % of finfish landings

ggplot()+geom_point(data=df_long,aes(x=Stckbio_STO_MTkm2,y=Stckbio_SUR_MTkm2,col=Region))+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + theme_classic()

df_long$diffc <- df_long$Catch_STO_tonnes/df_long$Catch_WAT_tonnes
df_long$diffs <- df_long$Stckbio_SUR_MTkm2/df_long$Tbio_SUR_MTkm2

test <- subset(df_long,df_long$Year > 2000)
test <- aggregate(cbind(diffc,diffs)~Region,data=test,FUN=mean)
plot(test$diffc,test$diffs,ylim=c(0,2),xlim=c(0,2));abline(0,1)

ggplot()+geom_point(data=df_long,aes(x=diffc,y=diffs,col=Region))+
  xlim(0,1.5)+ ylim(0,1.5)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + theme_classic()
