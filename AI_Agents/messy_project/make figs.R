# figures for the paper
# Figure 2: comparison with johnson & goldstein
# Figure 3: changes in registration by wave

library(haven)
library(ggplot2)
library(dplyr)

d <- read_dta("1_experiment_clean.dta")
d$in_person <- d$wave %in% c(1, 2)

#============================
# FIGURE 2 - Registration rates by frame
#============================

# our experiment
fig2_data <- d %>%
  filter(before == 0) %>%
  group_by(yesno) %>%
  summarise(
    prop = mean(after, na.rm = TRUE),
    n = n(),
    se = sqrt(prop * (1 - prop) / n),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lo = prop - 1.96 * se,
    ci_hi = prop + 1.96 * se,
    study = "Kessler & Roth",
    frame = ifelse(yesno == 1, "Yes/No", "Opt-in")
  )

# add Johnson & Goldstein comparison
jg <- data.frame(
  yesno = c(0, 1),
  prop = c(0.42, 0.79),
  n = c(NA, NA),
  se = c(NA, NA),
  ci_lo = c(NA, NA),
  ci_hi = c(NA, NA),
  study = "Johnson & Goldstein (hypothetical)",
  frame = c("Opt-in", "Yes/No")
)
plot_data <- bind_rows(fig2_data, jg)

p2 <- ggplot(plot_data, aes(x = frame, y = prop, fill = study)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "Figure 2: Registration Rates by Question Frame",
       subtitle = "Unregistered subjects only",
       y = "Proportion Registered",
       x = "Question Frame",
       fill = "Study") +
  theme_minimal() +
  scale_fill_manual(values = c("gray40", "steelblue"))

print(p2)
ggsave("fig2_draft.png", p2, width = 8, height = 5)

#============================
# FIGURE 3 - Changes by wave
#============================

fig3_data <- d %>%
  mutate(wave_label = ifelse(wave %in% c(1,2), "In-person (Waves 1-2)", "Online (Wave 3)")) %>%
  group_by(wave_label, before) %>%
  summarise(
    changed = ifelse(first(before) == 0,
                     mean(after == 1, na.rm = TRUE),
                     mean(after == 0, na.rm = TRUE)),
    n = n(),
    se = sqrt(changed * (1 - changed) / n),
    .groups = "drop"
  ) %>%
  mutate(
    direction = ifelse(before == 0, "Registered (was unregistered)",
                       "Unregistered (was registered)")
  )

p3 <- ggplot(fig3_data, aes(x = wave_label, y = changed, fill = direction)) +
  geom_col(position = position_dodge(0.8), width = 0.65) +
  geom_errorbar(aes(ymin = changed - 1.96*se, ymax = changed + 1.96*se),
                position = position_dodge(0.8), width = 0.2) +
  labs(title = "Figure 3: Changes in Registration Status",
       y = "Proportion Changing Status",
       x = "", fill = "") +
  theme_minimal() +
  scale_fill_manual(values = c("firebrick3", "steelblue"))

print(p3)
ggsave("fig3_draft.png", p3, width = 8, height = 5)

# TODO: figure 7 (event study) and figure 8 (NOK) still need to be done
