# ============================================
# Difference-in-Differences mini-lab (tidyverse)
# ============================================

# Packages
library(tidyverse)
library(broom)

rm(list=ls())

# ----------------------------
# 0) Load data
# ----------------------------

# パスをそのまま指定（または相対パスでもOK）
df <- read_csv("did_training_1.csv", show_col_types = FALSE) %>%
  mutate(
    treated = as.integer(treated_group),
    post    = as.integer(post),
    year    = as.integer(year)
  )

glimpse(df)
# Expected columns: unit, treated_group, year, post, outcome

# Choose the focal pre/post for the simple 2x2 (use 2018 vs 2019)
pre_year  <- 2018
post_year <- 2019

# ----------------------------
# A) By-hand DiD (2x2 means)
# ----------------------------
means_2x2 <- df |>
  filter(year %in% c(pre_year, post_year)) |>
  mutate(period = if_else(year == post_year, "Post", "Pre"),
         group  = if_else(treated == 1, "Treated", "Control")) |>
  group_by(group, period) |>
  summarise(mean_outcome = mean(outcome, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = period, values_from = mean_outcome)

# Show table
means_2x2

# Compute DiD = (Treated Post - Treated Pre) - (Control Post - Control Pre)
did_by_hand <- with(
  list(m = means_2x2),
  (m$Post[m$group == "Treated"] - m$Pre[m$group == "Treated"]) -
    (m$Post[m$group == "Control"] - m$Pre[m$group == "Control"]))

did_by_hand
# ----------------------------
# B) Regression DiD
#   Y = a + b*Treated + c*Post + d*(Treated*Post) + e
#   d is the DiD estimate
# ----------------------------
m_did <- lm(outcome ~ treated * post, data = df)
summary(m_did)

cat("\n--- DiD regression (treated × post) ---\n")

# Extract the DiD coefficient (treated:post)
did_coef <- tidy(m_did, conf.int = TRUE) |>
  filter(term == "treated:post")
did_coef

# ----------------------------
# C) Quick pre-trend check (2017–2018)
# ----------------------------
pre_trend_df <- df |>
  filter(year < post_year) |>
  group_by(treated, year) |>
  summarise(mean_outcome = mean(outcome, na.rm = TRUE), .groups = "drop") |>
  mutate(group = if_else(treated == 1, "Treated", "Control"))

pre_trend_df

ggplot(pre_trend_df, aes(x = year, y = mean_outcome, linetype = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Pre-trend check (means by year and group)",
       x = "Year", y = "Mean outcome", linetype = "Group")+
  coord_cartesian(ylim = c(48, 58))

# Optional: full 2017–2020 trend plot
trend_df <- df |>
  group_by(treated, year) |>
  summarise(mean_outcome = mean(outcome, na.rm = TRUE), .groups = "drop") |>
  mutate(group = if_else(treated == 1, "Treated", "Control"))

ggplot(trend_df, aes(x = year, y = mean_outcome, linetype = group)) +
  geom_line() +
  geom_point() +
  labs(title = "Trends 2017–2020 (means by year and group)",
       x = "Year", y = "Mean outcome", linetype = "Group")+
  coord_cartesian(ylim = c(48, 58))


# ----------------------------
# D) Optional: Event-study (leads/lags)
#   Interact Treated with each year (omit base year)
# ----------------------------
base_year <- 2018  # omit this as the reference pre-period
df_es <- df |>
  mutate(
    year = factor(year),
    # Interactions will be treated:yearYYYY, with year==base_year omitted
    across(year, forcats::fct_relevel, as.character(base_year))
  )

m_event <- lm(outcome ~ treated * year, data = df_es)
event_coef <- tidy(m_event, conf.int = TRUE) |>  filter(term == "treated:year") 

event_coef
cat("\n--- Event study (treated × year, base =", base_year, ") ---\n")

# Tidy ES coefficients for plotting (only the interactions)
es_tidy <- tidy(m_event, conf.int = TRUE) |>
  filter(str_detect(term, "^treated:year")) |>
  mutate(year = readr::parse_number(term))

ggplot(es_tidy, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Event-study estimates (relative to base year)",
       x = "Year", y = "Estimate (with 95% CI)")

# ----------------------------