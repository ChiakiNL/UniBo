
# Assigned Group 1
# Use did_training 1.csv

# Import dataset

did_training_1 <- read.csv("C:/Users/chiak/OneDrive - BI Norwegian Business School (BIEDU)/GitHub/UniBo/96342BigData/Lecture06/Activity_week3/activity_week3/did_training_1.csv")

View(did_training_1)
  
library(tidyverse)
library(gapminder)

# Filter 
did_small <- did_training_1 %>%
  filter(year %in% c(2018, 2019)) %>%
  mutate(
    post = if_else(year == 2019, 1, 0),          # 2019→1, 2018→0
    treated_group = as.integer(treated_group)    # 念のため整数化（0/1）
  )

# --- Task A-1: Make a 2x2 table ---

means_2x2 <- did_small %>%
  group_by(treated_group, post) %>%
  summarise(mean_outcome = mean(outcome, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    treated_label = if_else(treated_group == 1, "Treated", "Control"),
    period_label  = if_else(post == 1, "Post(2019)", "Pre(2018)")
  ) %>%
  select(treated_label, period_label, mean_outcome) %>%
  pivot_wider(names_from = period_label, values_from = mean_outcome) %>%
  arrange(desc(treated_label))

means_2x2


# --- Task A-2: Compute DiD from means_2x2 ---

# 2x2の平均を取り出し
treated_pre  <- means_2x2 %>% filter(treated_label == "Treated") %>% pull(`Pre(2018)`)
treated_post <- means_2x2 %>% filter(treated_label == "Treated") %>% pull(`Post(2019)`)
control_pre  <- means_2x2 %>% filter(treated_label == "Control") %>% pull(`Pre(2018)`)
control_post <- means_2x2 %>% filter(treated_label == "Control") %>% pull(`Post(2019)`)

# DiD = (Treated_Post - Treated_Pre) - (Control_Post - Control_Pre)
DiD <- (treated_post - treated_pre) - (control_post - control_pre)

# Show DiD result
DiD

# --- Task B: Regression DiD ---

# OLS回帰モデルを推定
did_reg <- lm(outcome ~ treated_group + post + treated_group:post, data = did_small)

# 結果を表示
summary(did_reg)

# 交互作用項（treated_group:post）の係数を抽出
coef_interaction <- coef(did_reg)["treated_group:post"]
cat("Coefficient on interaction (DiD estimate) =", round(coef_interaction, 3), "\n")

# 確認用コメント
cat("This value should be approximately equal to your manual DiD (up to rounding).\n")
cat("Manual DiD =", round(DiD, 3), "\n")
