# ---- Load Required Libraries ----
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)
# ---- Step 1: Load CSV Files ----

weights_file <- "C:/Users/anarb/Documents/snb_nfa_project/Unified_Currency_Composition__2000_2024_.csv"  # Columns: Year, USD, EUR, JPY, GBP, CAD
weights_raw <- read.csv(weights_file, stringsAsFactors = FALSE)

fx_file <- "C:/Users/anarb/Documents/snb_nfa_project/ExchangeRateMain5Currency.csv"   # Columns: Year, USD, EUR, JPY, GBP, CAD
fx_raw <- read.csv(fx_file, sep = ";", stringsAsFactors = FALSE, skip = 2)

asset_file <- "C:/Users/anarb/Documents/snb_nfa_project/ForeignAssetsince2000.csv"     # Columns: Year, Foreign_Assets (in million CHF)
asset_raw <- read.csv(asset_file, sep = ";", stringsAsFactors = FALSE, skip = 2)
print(head(weights_raw))
# Rename columns
names(fx_raw) <- c("Year", "Code", "Value")

# Convert types
fx_raw$Year <- as.integer(fx_raw$Year)
fx_raw$Value <- as.numeric(fx_raw$Value)

# Clean currency codes (e.g., EUR1 -> EUR)
fx_raw <- fx_raw %>%
  mutate(Currency = str_replace(Code, "1|100", "")) %>%
  mutate(Currency = if_else(Code == "JPY100", "JPY", Currency)) %>%
  mutate(Value = if_else(Currency == "JPY", Value / 100, Value))  # Convert JPY100 to JPY1

# Pivot to wide format
fx_wide <- fx_raw %>%
  select(Year, Currency, Value) %>%
  pivot_wider(names_from = Currency, values_from = Value)
fx_rates <- fx_wide %>%
  select(Year, USD, EUR, JPY, GBP, CAD)
# View result
print(head(fx_wide))
print(head(fx_rates))

# FX exchange rates over years
asset_raw$Value <- as.numeric(gsub(",", "", asset_raw$Value))
# ---- Convert 'Date' Columns to Year ----
asset_raw$Year <- as.numeric(asset_raw$Date)
# ---- Filter for total Foreign Assets: D1 == "T0" ----
asset_net <- asset_raw %>%
  filter(D1 == "T0") %>%
  select(Year, Foreign_Assets = Value)

print(asset_net)


# FX returns = percentage change from year to year
fx_returns <- fx_rates %>%
  arrange(Year) %>%
  mutate(
    USD_return = (USD - lag(USD)) / lag(USD),
    EUR_return = (EUR - lag(EUR)) / lag(EUR),
    JPY_return = (JPY - lag(JPY)) / lag(JPY),
    GBP_return = (GBP - lag(GBP)) / lag(GBP),
    CAD_return = (CAD - lag(CAD)) / lag(CAD)
  ) %>%
  select(Year, USD_return, EUR_return, JPY_return, GBP_return, CAD_return) %>%
  filter(!is.na(USD_return))  # remove first row (no prior year)

# View result
print(fx_returns)
fx_returns$Year <- as.integer(fx_returns$Year)
weights_raw$Year <- as.integer(weights_raw$Year)
asset_net$Year <- as.integer(asset_net$Year)

# Step 1: Join FX returns, weights, and foreign assets
df <- weights_raw %>%
  select(-Others) %>%  # Drop 'Others' since it's not part of FX effect
  rename_with(~ paste0(., "_weight"), -Year) %>%
  inner_join(
    fx_returns %>% rename(
      USD_fx = USD_return,
      EUR_fx = EUR_return,
      JPY_fx = JPY_return,
      GBP_fx = GBP_return,
      CAD_fx = CAD_return
    ),
    by = "Year"
  ) %>%
  inner_join(asset_net, by = "Year")

# Step 2: Calculate valuation effect for each currency
df <- df %>%
  mutate(
    USD_val = USD_weight * USD_fx * Foreign_Assets,
    EUR_val = EUR_weight * EUR_fx * Foreign_Assets,
    JPY_val = JPY_weight * JPY_fx * Foreign_Assets,
    GBP_val = GBP_weight * GBP_fx * Foreign_Assets,
    CAD_val = CAD_weight * CAD_fx * Foreign_Assets,
    Total_Valuation_Effect = USD_val + EUR_val + JPY_val + GBP_val + CAD_val
  )

# Step 3: Prepare data for plotting
plot_data <- df %>%
  select(Year, USD_val, EUR_val, JPY_val, GBP_val, CAD_val) %>%
  pivot_longer(cols = -Year, names_to = "Currency", values_to = "Valuation_Effect")
windows()
print(head(plot_data))
# Step 4: Plot stacked contribution and total line

print(
    ggplot(plot_data, aes(x = Year, y = Valuation_Effect, fill = Currency)) +
        geom_col(position = "stack") +
        geom_line(
            data = df,
            aes(x = Year, y = Total_Valuation_Effect),
            inherit.aes = FALSE,
            color = "black",
            linewidth = 1.1
        ) +
        labs(
            title = "FX Valuation Effect Contribution on Swiss Foreign Assets",
            x = "Year", y = "Valuation Effect (Million CHF)",
            fill = "Currency"
        ) +
        scale_y_continuous(labels = comma_format()) +
        theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right"
    )
  )


plot_data_cumulative <- plot_data %>%
  group_by(Currency) %>%
  arrange(Year) %>%
  mutate(Cumulative_Valuation = cumsum(Valuation_Effect)) %>%
  ungroup()
windows()
print(
    ggplot(plot_data_cumulative, aes(x = Year, y = Cumulative_Valuation, fill = Currency)) +
    geom_area(position = "stack") +
    labs(
        title = "Cumulative FX Valuation Effect Contribution on Swiss Foreign Assets",
        x = "Year", y = "Cumulative Valuation Effect (Million CHF)",
        fill = "Currency"
    ) +
    scale_y_continuous(labels = comma_format()) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.05, 0.35),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 12),
    )
)
df_cumulative <- df %>%
  arrange(Year) %>%
  mutate(Cumulative_Total_Valuation = cumsum(Total_Valuation_Effect)) %>%
  select(Year, Cumulative_Total_Valuation)

# Print result
print(df_cumulative)

# Load SNB-reported cumulative valuation effect
snb_va <- read_csv("C:/Users/anarb/Documents/snb_nfa_project/CumulativeVA.csv") %>%
  rename(SNB_Valuation = Cumulative_VA)

plot_data_cumulative_total <- df %>%
  select(Year) %>%
  distinct() %>%
  left_join(snb_va, by = "Year") %>%
  left_join(
    plot_data_cumulative %>%
      group_by(Year) %>%
      summarise(FX_Valuation = sum(Cumulative_Valuation), .groups = "drop"),
    by = "Year"
  )
# Prepare line data in long format for clean legend mapping
line_data <- plot_data_cumulative_total %>%
  select(Year, `FX-Only Valuation` = FX_Valuation, `Total Valuation (SNB)` = SNB_Valuation) %>%
  pivot_longer(-Year, names_to = "LineType", values_to = "Value")
print(line_data)
windows()
print(
    ggplot(plot_data_cumulative, aes(x = Year, y = Cumulative_Valuation, fill = Currency)) +
    geom_area(position = "stack") +

    geom_line(
        data = line_data,
        aes(x = Year, y = Value, group = LineType, color = LineType),
        inherit.aes = FALSE,  # ← this prevents ggplot from using fill = Currency
        linewidth = 1.2
    ) +

    labs(
        title = "Cumulative FX Valuation Effect vs. SNB-Reported Valuation Effect Total",
        x = "Year", y = "Cumulative Valuation Effect (Million CHF)",
        fill = "Currency",
        color = "Line"
    ) +
    scale_fill_brewer(palette = "Set3") +
    scale_color_manual(
        values = c("FX-Only Valuation" = "black", "Total Valuation (SNB)" = "magenta")
    ) +
    scale_linetype_manual(values = c(
        "FX-Only Valuation" = "solid",
        "Total Valuation (SNB)" = "dashed"
    )) +
    #scale_y_continuous(labels = comma_format()) +
    theme_minimal(base_size = 13) +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.05, 0.45),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 12)
    )
)