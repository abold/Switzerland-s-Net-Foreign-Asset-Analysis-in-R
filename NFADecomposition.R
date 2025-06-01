# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

# ---- Read Current Account CSV ----
current_file <- "C:/Users/anarb/Documents/snb_nfa_project/current_account.csv"
current_raw <- read.csv(current_file, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Read Valuation Effect CSV ----
valuation_file <- "C:/Users/anarb/Documents/snb_nfa_project/valuation_effect.csv"
valuation_raw <- read.csv(valuation_file, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Clean 'Value' Columns ----
current_raw$Value <- as.numeric(gsub(",", "", current_raw$Value))
valuation_raw$Value <- as.numeric(gsub(",", "", valuation_raw$Value))

# ---- Convert 'Date' Columns to Year ----
current_raw$Year <- as.numeric(current_raw$Date)
valuation_raw$Year <- as.numeric(valuation_raw$Date)

# ---- Filter for Net Current Account: D1 == "S" ----
current_net <- current_raw %>%
  filter(D1 == "S") %>%
  select(Year, Current_Account = Value)

# ---- Filter for Valuation Effect: D0 == "SD" ----
valuation_net <- valuation_raw %>%
  filter(D0 == "SD") %>%
  select(Year, Valuation_Effect = Value)

# ---- Join and Calculate Total ----
combined_data <- inner_join(current_net, valuation_net, by = "Year") %>%
  mutate(Total = Current_Account + Valuation_Effect)

# ---- View Result ----
print(head(combined_data))

# ---- Load Required Libraries ----
library(ggplot2)
library(patchwork)  # For combining plots
library(scales)
# Compute a shared y-axis range
# Determine limits
# Shared y-axis limits
y_min <- floor(min(combined_data$Current_Account,
                   combined_data$Valuation_Effect,
                   combined_data$Total, na.rm = TRUE) / 50000) * 50000
y_max <- ceiling(max(combined_data$Current_Account,
                     combined_data$Valuation_Effect,
                     combined_data$Total, na.rm = TRUE) / 50000) * 50000

y_breaks <- seq(y_min, y_max, by = 50000)


windows()
p1 <- ggplot(combined_data, aes(x = Year, y = Current_Account)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Current Account Over Years", y = "Million CHF", x = NULL) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()

p2 <- ggplot(combined_data, aes(x = Year, y = Valuation_Effect)) +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Statistical Difference Over Years", y = "Million CHF", x = NULL) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()

p3 <- ggplot(combined_data, aes(x = Year, y = Total)) +
  geom_line(color = "brown", linewidth = 1) +
  geom_point(color = "brown") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Current Account + Statistical Difference Over Years", 
       y = "Million CHF", x = "Year") +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()
print(p1 / p2 / p3)
