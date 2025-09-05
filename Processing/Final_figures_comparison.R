
setwd("C:/Users/danie/Documents/Online for git/Fish-biomass-catches/Outputs-by-region/")

year_start <- 1995
year_end <- 2010

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

df_filtered <- df_long %>%
  filter(Year >= year_start, Year <= year_end)

df_summary <- df_filtered %>%
  group_by(Region, measure) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    lower = quantile(value, 0.025, na.rm = TRUE),
    upper = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

df_summary <- rbind(df_summary)

# arrange by total biomass
region_order <- df_summary %>%
  filter(measure == "Tbio_SUR_MTkm2") %>%
  group_by(Region) %>%
  summarise(total_biomass = sum(median, na.rm = TRUE)) %>%
  arrange(total_biomass) %>%
  pull(Region)

df_summary <- df_summary %>%
  mutate(Region = factor(Region, levels = region_order)) %>%
  mutate(measure = recode(measure,
                          "Stckbio_SUR_MTkm2" = "Stock biomass survey",
                          "Stckbio_STO_MTkm2" = "Stock biomass assessment",
                          "Tbio_SUR_MTkm2" = "Total survey biomass"))

# use this if you like to order by median instead of total
# df_summary <- df_summary %>%
#   group_by(Region) %>%
#   mutate(mean_median = mean(median, na.rm = TRUE)) %>%
#   ungroup() %>%
#   arrange(mean_median) %>%
#   mutate(Region = factor(Region, levels = unique(Region)))
# 
# df_summary <- df_summary %>%
#   mutate(measure = recode(measure,
#                           "Stckbio_SUR_MTkm2" = "Stock biomass survey",
#                           "Stckbio_STO_MTkm2" = "Stock biomass assessment",
#                           "Tbio_SUR_MTkm2" = "Total survey biomass"
#   ))

# --- Split into two plots ---

# Plot 1: Total biomass and EwE
df_plot1 <- df_summary %>%
  filter(measure %in% c("Total survey biomass"))

p1 <- ggplot(df_plot1, aes(x = Region, y = median, color = "measure")) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "Total Biomass by Region",
       x = "Region (sorted)",
       y = "Tonnes/km²",
       color = "Metric") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 125)) +
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

p1 <- ggplot(df_plot1, aes(x = Region, y = median)) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2,
                color="blue") +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  labs(title = "",
       x = "Regions (sorted)",
       y = "Total biomass (Tonnes/km²)",
       color = "Metric") +
  theme_minimal() +
  coord_cartesian(ylim = c(1, 160)) +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../Outputs/Total_biomass.pdf", p1,
       width = 5.5, height = 5, units = "in")


# get the proportional difference
df_wide <- df_summary %>%
  filter(measure %in% c("Stock biomass survey", "Stock biomass assessment")) %>%
  select(Region, measure, median, lower, upper) %>%
  pivot_wider(
    names_from = measure,
    values_from = c(median, lower, upper),
    names_glue = "{measure}_{.value}"
  )

# Scatterplot with error bars for both axes
p2_scatter_ci <- ggplot(df_wide, 
                        aes(x = `Stock biomass assessment_median`, 
                            y = `Stock biomass survey_median`)) +
  geom_errorbar(aes(ymin = `Stock biomass survey_lower`, ymax = `Stock biomass survey_upper`), 
                width = 0.02,color="blue") +
  geom_errorbarh(aes(xmin = `Stock biomass assessment_lower`, xmax = `Stock biomass assessment_upper`), 
                 height = 0.02,color="blue") +
  geom_point(size = 3, color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "",
       x = "Stock Biomass (Assessment, tonnes/km²)",
       y = "Stock Biomass (Survey, tonnes/km²)") +
    theme_minimal() + coord_cartesian(xlim = c(1.5, 50), ylim = c(1.5, 50), clip = "off")

ggsave("../Outputs/Stock_biomass.pdf", p2_scatter_ci,
       width = 5.5, height = 5, units = "in")
p2_scatter_ci


rmse <- df_wide %>%
  summarise(RMSE = sqrt(mean((`Stock biomass survey_median` - 
                                `Stock biomass assessment_median`)^2,
                             na.rm = TRUE))) %>%
  pull(RMSE)

rmse

r <- cor(df_wide$`Stock biomass survey_median`,
         df_wide$`Stock biomass assessment_median`,
         use = "complete.obs")
r

r <- cor(log10(df_wide$`Stock biomass survey_median`),
         log10(df_wide$`Stock biomass assessment_median`),
         use = "complete.obs")
r


# Calculate RMSE on log10 scale, ignoring NAs
epsilon <- 0 #1e-6
rmse_log <- df_wide %>%
  filter(!is.na(`Stock biomass survey_median`) & 
           !is.na(`Stock biomass assessment_median`)) %>%
  summarise(RMSE_log = sqrt(mean((log10(`Stock biomass survey_median` + epsilon) - 
                                    log10(`Stock biomass assessment_median` + epsilon))^2))) %>%
  pull(RMSE_log)

rmse_log
10^rmse_log


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
  filter(Year >= year_start, Year <= year_end)

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
                          "ER_SAU_SUR" = "SAU catch/survey biomass",
                          "ER_WAT_SUR" = "Watson catch/survey biomass",
                          "ER_STO"    = "Stock ass. catch/biomass"
  ))

# Step 5: Plot
ER <- ggplot(df_summary, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  scale_color_manual(values = c("steelblue",
                                "forestgreen",
                                 "firebrick"))+
  labs(title = "",
       x = "Region (sorted)",
       y = "Exploitation rate (year-1)",
       color = "Metric") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10()

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
  filter(Year >= year_start, Year <= year_end)

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
PD <- ggplot(df_summary, aes(x = Region, y = median, color = measure)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper),
                position = position_dodge(width = 0.6),
                width = 0.2) +
  labs(title = "",
       x = "Region (sorted)",
       y = "Perc. demersal fish versus total fish") +
  scale_color_manual(values = c("steelblue",
                                "forestgreen",
                                "firebrick")) +
  theme_minimal() + ylim(0,100)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

PD
ggsave("../Outputs/percentage_demersal.pdf", PD,
       width = 7, height = 5, units = "in")

# now some final checks
df_long <- map_dfr(file_list, ~ {
  read_csv(.x)})

tr <- aggregate(df_long$Land_SAU_tonnes,by=list(df_long$Region),FUN=mean)
sum(tr[,2])/10^6/(80*0.85)*100 # % of finfish landings

ggplot()+geom_point(data=df_long,aes(x=Stckbio_STO_MTkm2,y=Stckbio_SUR_MTkm2,col=Region))+
  xlim(0,50)+ ylim(0,50)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") + theme_classic()


