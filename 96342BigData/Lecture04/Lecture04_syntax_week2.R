# R Script for Data Analysis
# Author: Nicola Barban
# Date: 2023-09-28
# Description: This script contains various R code snippets for the analysis of population dynamics



# -------------------------------------------------------------------------------------------------------
# Load necessary libraries and data sets
library("tidyverse")

# Load data sets from URLs and save them as CSV files
Sweden <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/Sweden.csv")
Kenya <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/Kenya.csv")
World <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/INTRO/World.csv")

# Save the downloaded data as CSV files
write_csv(Sweden, "data/Sweden.csv")
write_csv(Kenya, "data/Kenya.csv")
write_csv(World, "data/World.csv")

# Display the 'Sweden' data frame
Sweden

# -------------------------------------------------------------------------------------------------------
# Calculate total person-years for each data set
Sweden$py.total <- Sweden$py.men + Sweden$py.women
Kenya$py.total <- Kenya$py.men + Kenya$py.women
World$py.total <- World$py.men + World$py.women

# -------------------------------------------------------------------------------------------------------
# Calculate crude birth rates (CBR) for specific time periods
# R の c() は concatenate（連結） の略で、複数の値をベクトルとしてまとめる関数

Sweden.CBR <- c(sum(Sweden$births[1:15]) / sum(Sweden$py.total[1:15]),
                sum(Sweden$births[16:30]) / sum(Sweden$py.total[16:30]))
Kenya.CBR <- c(sum(Kenya$births[1:15]) / sum(Kenya$py.total[1:15]),
               sum(Kenya$births[16:30]) / sum(Kenya$py.total[16:30]))
World.CBR <- c(sum(World$births[1:15]) / sum(World$py.total[1:15]),
               sum(World$births[16:30]) / sum(World$py.total[16:30]))

# -------------------------------------------------------------------------------------------------------
# Assign meaningful names to the CBR vectors
names(Sweden.CBR) <- c("1950-1955", "2005-2010")
names(Kenya.CBR) <- c("1950-1955", "2005-2010")
names(World.CBR) <- c("1950-1955", "2005-2010")

# -------------------------------------------------------------------------------------------------------
# Use Tidyverse commands
Sweden.period.CBR <-
  Sweden %>%
  group_by(period) %>%
  summarize(Sweden.CBR.total = sum(births)/sum(py.men + py.women))
Sweden.period.CBR

# -------------------------------------------------------------------------------------------------------

# Dont want to repeat the same code so we use function
# Create a function to calculate CBR for a given data frame
CBR_func <- function(data) {
  output <- data %>%
    group_by(period) %>%
    summarize(CBR = sum(births) / sum(py.men + py.women))
  return(output)
}

# Calculate CBR for Sweden, Kenya, and World using the CBR_func function
CBR_func(Sweden)
CBR_func(Kenya)
CBR_func(World)

# -------------------------------------------------------------------------------------------------------
# CBR - 分母に 男性も高齢女性も含まれてしまう ので、実際の「出産に関係する女性（15〜49歳）」に焦点を当てていません。
# そこで使うのが Age-Specific Fertility Rate (ASFR)。
# これは 特定の年齢区間ごとに、女性1人あたりの出生数を計算する指標です。

# Create age groups for filtering ages [15-50)
age_groups <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")

# Calculate Age-Specific Fertility Rates (ASFR) for Sweden
Sweden.period.ASFR <-
  Sweden %>%
  group_by(period) %>%
  mutate(Sweden.ASFR = births / py.women) %>%
  filter(age %in% age_groups)
  
Sweden.period.ASFR %>%
  select(period, age, Sweden.ASFR)

Sweden.period.ASFR %>% ggplot(aes(x=age, y=Sweden.ASFR, group=period)) + geom_line(size = 1) +
  geom_point(size = 2)

# -------------------------------------------------------------------------------------------------------
# Create a function to calculate ASFR for a given data frame
ASFR <- function(data) {
  data %>%
    group_by(period) %>%
    mutate(asfr = births / py.women) %>%
    filter(age %in% age_groups) %>%
    select(period, age, asfr)
}

# Calculate ASFR for Sweden, Kenya, and World using the ASFR function
ASFR(Sweden)
ASFR(Kenya)
ASFR(World)

# -------------------------------------------------------------------------------------------------------
# Calculate Total Fertility Rate (TFR) for Sweden, Kenya, and World
Sweden.TFR <- ASFR(Sweden) %>%
  summarise(sweden.TFR = sum(asfr * 5))

Kenya.TFR <- ASFR(Kenya) %>%
  summarise(kenya.TFR = sum(asfr * 5))

World.TFR <- ASFR(World) %>%
  summarise(world.TFR = sum(asfr * 5))

# Display TFR values
Sweden.TFR
Kenya.TFR
World.TFR

# -------------------------------------------------------------------------------------------------------
# Create a function to calculate Crude Death Rate (CDR) for a given data frame
CDR <- function(data) {
  data %>%
    group_by(period) %>%
    summarize(CDR.total = sum(deaths) / sum(py.men + py.women))
}

# Calculate CDR for Sweden, Kenya, and World
CDR(Sweden)
CDR(Kenya)
CDR(World)

# -------------------------------------------------------------------------------------------------------
# Create a function to calculate Age-Specific Death Rate (ASDR) for a given data frame
ASDR <- function(data) {
  data %>%
    group_by(period) %>%
    mutate(asdr = deaths / (py.men + py.women)) %>%
    filter(period == "2005-2010") %>%
    select(period, age, asdr)
}

# Calculate ASDR for Sweden and Kenya
ASDR(Sweden)
ASDR(Kenya)

ASDR(Kenya) %>% ggplot(aes(x=age, y=asdr)) + geom_point()

# -------------------------------------------------------------------------------------------------------
# Calculate ASDR for Sweden and Kenya using a different approach
ASDR <- function(data) {
  out <- data$deaths / data$py.total
  names(out) <- data$age
  return(out[16:30] * 1000)
}

ASDR(Sweden)
ASDR(Kenya)

# -------------------------------------------------------------------------------------------------------
# Calculate ASDR for Kenya and Sweden using the same method
ASDR <- function(data) {
  data %>%
    group_by(period) %>%
    mutate(asdr = deaths / (py.men + py.women)) %>%
    filter(period == "2005-2010") %>%
    select(period, age, asdr)
}

Kenya.ASDR <- ASDR(Kenya)
Sweden.ASDR <- ASDR(Sweden)

# Display ASDR values for Sweden and Kenya
ASDR(Sweden)
ASDR(Kenya)

# -------------------------------------------------------------------------------------------------------
# Filter data for the year 2005-2010 and calculate population proportions
Sweden.2005 <- Sweden %>%
  filter(period == "2005-2010") %>%
  mutate(total.py = py.men + py.women)

# Calculate Sweden's total population
Swe.tot.pop <- sum(Sweden.2005$total.py)

# Calculate age bracket population proportions
Sweden.2005 <- Sweden.2005 %>%
  mutate(pop.prop = total.py / Swe.tot.pop)

# Calculate Kenya's counterfactual CDR
Kenya.counter.CDR <- sum(Kenya.ASDR$asdr * Sweden.2005$pop.prop)

# Display Kenya's counterfactual CDR and CDR for the year 2005-2010
Kenya.counter.CDR
CDR(Kenya) %>%
  filter(period == "2005-2010")


