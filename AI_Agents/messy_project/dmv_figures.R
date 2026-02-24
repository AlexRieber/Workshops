# DMV event study plots — Figure 7
# Quarterly organ donor registration rates around policy changes

library(haven)
library(ggplot2)
library(dplyr)
library(fixest)

dmv <- read_dta("3_dmv_quarterly_clean.dta")
dmv <- dmv[!is.na(dmv$PercentDonor), ]
dmv$yq <- (dmv$Year - 2010) * 4 + dmv$Quarter
dmv$state_num <- as.integer(factor(dmv$State))

# ---- California ----
ca <- dmv %>%
  filter(Year >= 2010 & Year <= 2012) %>%
  mutate(treat = as.integer(State == "California"),
         event_time = yq - 6)  # reform at Q2 2011

# normalize to baseline
baseline_ca <- ca %>%
  filter(event_time == 0) %>%
  group_by(treat) %>%
  summarise(base = mean(PercentDonor), .groups = "drop")

ca <- left_join(ca, baseline_ca, by = "treat")
ca$normalized <- ca$PercentDonor / ca$base * 100

ca_plot <- ca %>%
  group_by(event_time, treat) %>%
  summarise(mean_norm = mean(normalized, na.rm=TRUE), .groups = "drop") %>%
  mutate(group = ifelse(treat == 1, "California", "Other States"))

p_ca <- ggplot(ca_plot, aes(x = event_time, y = mean_norm, color = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Figure 7a: California — Donor Registration Rates",
       x = "Quarters Relative to Reform", y = "Registration Rate (Baseline = 100)") +
  theme_minimal()
print(p_ca)
ggsave("fig7a_california.png", p_ca, width = 8, height = 5)

# ---- New York ----
ny <- dmv %>%
  filter(yq >= 10 & yq <= 22) %>%
  mutate(treat = as.integer(State == "New York"),
         event_time = yq - 15)

baseline_ny <- ny %>%
  filter(event_time == 0) %>%
  group_by(treat) %>%
  summarise(base = mean(PercentDonor), .groups = "drop")

ny <- left_join(ny, baseline_ny, by = "treat")
ny$normalized <- ny$PercentDonor / ny$base * 100

ny_plot <- ny %>%
  group_by(event_time, treat) %>%
  summarise(mean_norm = mean(normalized, na.rm=TRUE), .groups = "drop") %>%
  mutate(group = ifelse(treat == 1, "New York", "Other States"))

p_ny <- ggplot(ny_plot, aes(x = event_time, y = mean_norm, color = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Figure 7b: New York — Donor Registration Rates",
       x = "Quarters Relative to Reform", y = "Registration Rate (Baseline = 100)") +
  theme_minimal()
print(p_ny)
ggsave("fig7b_newyork.png", p_ny, width = 8, height = 5)

# ---- Hawaii ----
hi <- dmv %>%
  filter(yq >= 13 & yq <= 23) %>%
  mutate(treat = as.integer(State == "Hawaii"),
         event_time = yq - 18)

baseline_hi <- hi %>%
  filter(event_time == 0) %>%
  group_by(treat) %>%
  summarise(base = mean(PercentDonor), .groups = "drop")

hi <- left_join(hi, baseline_hi, by = "treat")
hi$normalized <- hi$PercentDonor / hi$base * 100

hi_plot <- hi %>%
  group_by(event_time, treat) %>%
  summarise(mean_norm = mean(normalized, na.rm=TRUE), .groups = "drop") %>%
  mutate(group = ifelse(treat == 1, "Hawaii", "Other States"))

p_hi <- ggplot(hi_plot, aes(x = event_time, y = mean_norm, color = group)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Figure 7c: Hawaii — Donor Registration Rates",
       x = "Quarters Relative to Reform", y = "Registration Rate (Baseline = 100)") +
  theme_minimal()
print(p_hi)
ggsave("fig7c_hawaii.png", p_hi, width = 8, height = 5)
