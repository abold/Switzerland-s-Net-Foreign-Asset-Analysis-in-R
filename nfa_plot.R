# ---- Load Required Libraries ----
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

# ---- Read CSV Data (Skip First Two Lines) ----
file_path <- "C:/Users/anarb/Documents/snb_nfa_project/snb_data1985.csv"
rawData <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, skip = 2)

# ---- Defensive Check: Print column names ----
print("Column names:")
print(names(rawData))

# ---- Clean Value Column ----
if ("Value" %in% names(rawData)) {
  rawData$Value <- as.numeric(gsub(",", "", rawData$Value))
} else {
  stop("Column 'Value' not found in raw data.")
}

# ---- Convert Date (Assuming Year Only) ----
if ("Date" %in% names(rawData)) {
  rawData$Date <- ymd(paste0(rawData$Date, "-01-01"))
} else {
  stop("Column 'Date' not found in raw data.")
}

# ---- Filter and Process Data ----
required_columns <- c("D0", "D1", "Date", "Value")
if (!all(required_columns %in% names(rawData))) {
  stop(paste("Missing columns:", paste(setdiff(required_columns, names(rawData)), collapse = ", ")))
}

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
  full_join(foreign_assets_data, by = "Date") %>%
  full_join(foreign_liabilities_data, by = "Date") %>%
  arrange(Date)

# ---- Handle NA and check ----
combined_data <- combined_data %>% filter(!is.na(Date))
print(paste("Number of rows in combined data:", nrow(combined_data)))
windows()
# ---- Plot ----
print(
  if (nrow(combined_data) > 0) {
    ggplot(combined_data, aes(x = Date)) +
      geom_line(aes(y = Foreign_Assets / 1e6, color = "Foreign Assets"), linewidth = 1.2, na.rm = TRUE) +
      geom_line(aes(y = Foreign_Liabilities / 1e6, color = "Foreign Liabilities"), linewidth = 1.2, na.rm = TRUE) +
      geom_line(aes(y = NFA / 1e6, color = "Net Foreign Assets (NFA)"), linewidth = 1.2, linetype = "dashed", na.rm = TRUE) +
      labs(
        title = "Switzerland's Foreign Assets, Liabilities, and NFA Over Time",
        x = "Year",
        y = "Value (Million CHF)",
        color = NULL
      ) +
      scale_color_manual(values = c(
        "Foreign Assets" = "darkgreen",
        "Foreign Liabilities" = "red",
        "Net Foreign Assets (NFA)" = "blue"
      )) +
      scale_x_date(
        date_breaks = "5 years",
        date_labels = "%Y",
        limits = c(as.Date("1983-01-01"), NA)
      ) +
      scale_y_continuous(
        limits = c(0, 6),
        breaks = seq(0, 6, by = 1),
        labels = scales::comma_format(scale = 1)
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = c(0.05, 0.95),
        legend.title = element_blank(),
        legend.justification = c("left", "top"),
        legend.box = "horizontal",
        legend.background = element_rect(fill = "white", color = "black"),
        legend.text = element_text(size = 14),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_blank()
      )
  } else {
    print("No data available to plot.")
  }
)