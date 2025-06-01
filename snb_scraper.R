# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

# ---- Read CSV Data (Skip First Two Lines) ----
file_path <- "C:/Users/anarb/Documents/snb_nfa_project/snb_data1985.csv"
rawData <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Clean Value Column ----
rawData$Value <- as.numeric(gsub(",", "", rawData$Value))  # If commas exist; keep for safety

# ---- Convert Date (Assuming Year Only) ----
rawData$Date <- ymd(paste0(rawData$Date, "-01-01"))  # Convert 1985 → "1985-01-01" → Date

# ---- Filter and Process Data ----

# Net Foreign Assets
nfa_data <- rawData %>%
  filter(D0 == "N", D1 == "T0") %>%
  select(Date, Value) %>%
  rename(NFA = Value)

# Foreign Assets
foreign_assets_data <- rawData %>%
  filter(D0 == "A", D1 == "T0") %>%
  select(Date, Value) %>%
  rename(Foreign_Assets = Value)

# Foreign Liabilities
foreign_liabilities_data <- rawData %>%
  filter(D0 == "P", D1 == "T0") %>%
  select(Date, Value) %>%
  rename(Foreign_Liabilities = Value)

# ---- Merge Data ----
combined_data <- nfa_data %>%
  left_join(foreign_assets_data, by = "Date") %>%
  left_join(foreign_liabilities_data, by = "Date")
windows()
print(
  # ---- Plot with custom legend inside the graph ----
  if (nrow(combined_data) > 0) {
    ggplot(combined_data, aes(x = Date)) +
      geom_line(aes(y = NFA / 1e6, color = "Net Foreign Assets (NFA)"), linewidth = 1.2) +
      geom_line(aes(y = Foreign_Assets / 1e6, color = "Foreign Assets"), linewidth = 1.2) +
      geom_line(aes(y = Foreign_Liabilities / 1e6, color = "Foreign Liabilities"), linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = "Switzerland's Net Foreign Assets, Foreign Assets, and Foreign Liabilities Over Time",
        x = "Year",
        y = "Value (Million CHF)"  # Set y-axis label
      ) +
      scale_color_manual(values = c(
        "Net Foreign Assets (NFA)" = "steelblue",
        "Foreign Assets" = "darkgreen",
        "Foreign Liabilities" = "firebrick"
      )) +
      scale_x_date(
        date_breaks = "5 years",  # Show every 5 years
        date_labels = "%Y",       # Format labels as year
        limits = c(as.Date("1983-01-01"), NA)  # Start from 1985
      ) +
      scale_y_continuous(
        breaks = seq(0, 6, by = 1),  # y-axis from 0 to 6, with ticks every 1
        labels = function(x) paste0(x)  # Label ticks with numbers
      ) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.05, 0.95),  # Position the legend inside the graph
        legend.title = element_blank(),   # Remove legend title
        legend.justification = c("left", "top"),  # Align legend to the top-left corner
        legend.background = element_rect(fill = alpha("white", 0.5), size = 0.5),  # Slight transparency for readability
        legend.key = element_rect(fill = "transparent", color = NA)  # Transparent legend keys
      ) +
      guides(
        color = guide_legend(
          ncol = 1,  # One column for the legend
          byrow = TRUE  # Organize the legend in rows
        )
      )
  } else {
    print("No data available to plot.")
  }
)
