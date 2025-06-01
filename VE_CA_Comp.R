# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)
library(purrr)
library(tidyr) # to use function replace_na

# ---- Read Current Account CSV ----
NIIP_file <- "C:/Users/anarb/Documents/snb_nfa_project/NIIPQuarterly2000.csv"
NIIP_raw <- read.csv(NIIP_file, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Read Valuation Effect CSV ----
current_file <- "C:/Users/anarb/Documents/snb_nfa_project/FA_CA_RES.csv"
current_raw <- read.csv(current_file, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Clean 'Value' Columns ----
NIIP_raw$Value <- as.numeric(gsub(",", "", NIIP_raw$Value))
current_raw$Value <- as.numeric(gsub(",", "", current_raw$Value))

# ---- Convert 'Date' Columns to Year ----
current_raw$Year <- as.numeric(current_raw$Date)

# ---- Filter for Net Current Account: D0 == "S0" ----
current_net <- current_raw %>%
  filter(D0 == "S0") %>%
  select(Year, Current_Account = Value)

# ---- Filter for Net Capital Account: D0 == "S8" ----
capital_net <- current_raw %>%
  filter(D0 == "S8") %>%
  select(Year, Capital_Account = Value)

# ---- Filter for Net Financial Account: D0 == "S9" ----
financial_net <- current_raw %>%
  filter(D0 == "S9") %>%
  select(Year, Financial_Account = Value)
# ---- Filter for Net Derivatives: D0 == "S9" ----
derivatives <- current_raw %>%
  filter(D0 == "S14") %>%
  select(Year, Derivatives = Value)
  # ---- Filter for Net Derivatives: D0 == "S9" ----
residuals <- current_raw %>%
  filter(D0 == "SD") %>%
  select(Year, Residuals = Value)
# ---- Filter for Valuation Effect: D0 == "SD" ----
NIIP_net <- NIIP_raw %>%
  filter(str_detect(Date, "Q4"),D0 == "N", D1 == "T0") %>%
  select(Date, nfa = Value)

NIIP_net$Year <- as.numeric(str_sub(NIIP_net$Date,1,4))
#print(head(current_net))
#print(head(capital_net))
#print(head(financial_net))
#print(head(NIIP_net))
# Calculate year-over-year change (ΔNFA)
NIIP_net <- NIIP_net %>%
  mutate(Delta_NFA = nfa - lag(nfa),
           Delta_NFA = replace_na(Delta_NFA, 0)
            )
# ---- View Result ----
print(head(NIIP_net))
# ---- Join and Calculate Total ----
df_list <- list(current_net, capital_net, financial_net, derivatives, residuals, NIIP_net)
final_data <- reduce(df_list, inner_join, by = "Year")
final_data <- final_data %>%
  mutate(
    # Flip signs for FA and Derivatives to follow BoP accounting convention
    FA_macro = -Financial_Account - Derivatives,
    FA_macro = replace_na(FA_macro, 0),
    # Calculate Valuation Effect
    Valuation_Effect = Delta_NFA - Current_Account - Capital_Account + FA_macro - Residuals,
    Valuation_Effect = replace_na(Valuation_Effect, 0),
    Total = Current_Account + Valuation_Effect
     )
print(head(final_data,20))
# ---- Load Required Libraries ----
library(ggplot2)
library(patchwork)  # For combining plots
library(scales)
# Compute a shared y-axis range
# Determine limits
# Shared y-axis limits
y_min <- floor(min(final_data$Current_Account,
                   final_data$Valuation_Effect,
                   final_data$Total, na.rm = TRUE) / 50000) * 50000
y_max <- ceiling(max(final_data$Current_Account,
                     final_data$Valuation_Effect,
                     final_data$Total, na.rm = TRUE) / 50000) * 50000

y_breaks <- seq(y_min, y_max, by = 50000)


windows()
p1 <- ggplot(final_data, aes(x = Year, y = Current_Account)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Current Account", y = "Million CHF", x = NULL) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()

p2 <- ggplot(final_data, aes(x = Year, y = Valuation_Effect)) +
  geom_line(color = "orange", linewidth = 1) +
  geom_point(color = "orange") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Valuation Effect", y = "Million CHF", x = NULL) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()

p3 <- ggplot(final_data, aes(x = Year, y = Total)) +
  geom_line(color = "brown", linewidth = 1) +
  geom_point(color = "brown") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(title = "Change in Current Account + Valuation Effect", 
       y = "Million CHF", x = "Year") +
  scale_y_continuous(limits = c(y_min, y_max), breaks = y_breaks, labels = scales::comma) +
  theme_minimal()
print(p1 / p2 / p3)

library(ggplot2)
library(tidyr)
library(scales)
library(dplyr)

final_data <- final_data %>%
  arrange(Year) %>%
  mutate(
    Cumulative_CA = cumsum(Current_Account),
    Cumulative_KA = cumsum(Capital_Account),
    Cumulative_FA = cumsum(FA_macro),
    Cumulative_VA = cumsum(Valuation_Effect),
    Cumulative_SD = cumsum(Residuals)
  )
print(final_data %>% select(Year, Cumulative_VA))
write.csv(
  final_data %>% select(Year, Cumulative_VA),
  "C:/Users/anarb/Documents/snb_nfa_project/CumulativeVA.csv",
  row.names = FALSE
)
final_data$Date <- as.Date(paste0(final_data$Year, "-01-01"))
# Prepare data in long format for all cumulative components
plot_data <- final_data %>%
  select(Date, Cumulative_CA, Cumulative_KA, Cumulative_FA, Cumulative_VA, Cumulative_SD) %>%
  pivot_longer(cols = -Date, names_to = "Component", values_to = "Value") %>%
  mutate(Component = recode(Component,
    "Cumulative_CA" = "Cumulative Current Account",
    "Cumulative_KA" = "Cumulative Capital Account",
    "Cumulative_FA" = "Cumulative Financial Account",
    "Cumulative_VA" = "Cumulative Valuation Effects",
    "Cumulative_SD" = "Cumulative Residuals"
  ))

# Color map (fresh, readable colors)
color_map <- c(
  "Cumulative Current Account" = "#1f77b4",      # Blue
  "Cumulative Capital Account" = "#9467bd",      # Purple
  "Cumulative Financial Account" = "#ff7f0e",    # Orange
  "Cumulative Valuation Effects" = "#d62728",    # Red
  "Cumulative Residuals" = "#2ca02c", # Green
  "NFA (Actual)" = "black"                       # Dashed black line
)
#print(
    # Plot with Matplotlib-like style
    ggplot() +
    # Solid lines for components
    geom_line(data = plot_data, aes(x = Date, y = Value, color = Component), linewidth = 1.2) +

    # Dashed line for actual NFA
    geom_line(data = final_data, aes(x = Date, y = nfa, color = "NFA (Actual)"),
          linewidth = 1.4, linetype = "dashed") +

    # Labels and scales
    labs(
        title = "Decomposition of Switzerland’s Net Foreign Assets (NFA)",
        x = "Year", y = "Million CHF", color = NULL, linetype = NULL
    ) +
    scale_color_manual(values = color_map) +
    scale_linetype_manual(values = c("NFA (Actual)" = "dashed")) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    scale_y_continuous(
        labels = comma_format(),
        minor_breaks = seq(-1e6, 2e6, by = 50000)  # adjust range and step as needed
        )+

    # Theme matching the reference style
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.05, 0.35),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
        panel.grid.major.x = element_line(color = "gray80"),  # vertical gridlines
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.minor.y = element_line(color = "gray90", linetype = "dotted")  # finer y grid
        )      # Save the plot
#)
