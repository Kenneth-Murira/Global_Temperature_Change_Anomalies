## Load dataset
climate <- read.csv("GLB.Ts+dSST.csv", skip = 1)

## Libraries
library(tidyverse)
library(RColorBrewer)
library(rmarkdown)
library(zoo)

## Clean and Reshape Data
df <- climate %>%
  +     rename(Annual = J.D) %>%
  +     select(Year, Annual, Jan:Dec) %>%
  +     mutate(across(Jan:Dec, ~ as.numeric(.))) %>%
  +     pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "Anomaly") %>%
  +     mutate(Month = factor(Month, levels = month.abb))

## Core Analysis
## 1.Long-term trend analysis
annual_df <- climate %>%
  +     select(Year, Annual = J.D) %>%
  +     mutate(Annual = as.numeric(Annual))
## Linear Regression of annual mean temperature against year
model <- lm(Annual ~ Year, data = annual_df)

## 2.Anomaly Distribution and Extremes
annual_stats <- climate %>%
  +     select(Year, Annual = J.D) %>%
  +     mutate(Annual = as.numeric(Annual)) %>%
  +     summarize(
    +         mean_anomaly = mean(Annual, na.rm = TRUE),
    +         median_anomaly = median(Annual, na.rm = TRUE),
    +         sd_anomaly = sd(Annual, na.rm = TRUE),
    +         min_anomaly = min(Annual, na.rm = TRUE),
    +         max_anomaly = max(Annual, na.rm = TRUE),
    +         coldest_year = Year[which.min(Annual)],
    +         hottest_year = Year[which.max(Annual)])
annual_stats

## 3.Decadal Warming Comparison

decade_stats <- annual_df %>%
  +     mutate(decade = case_when(
      +             Year >= 1950 & Year <= 1959 ~ "1950s",
      +             Year >= 1980 & Year <= 1989 ~ "1980s",
      +             Year >= 2010 & Year <= 2019 ~ "2010s",
      +             TRUE ~ NA_character_)) %>%
  +     filter(!is.na(decade)) %>%
  +     group_by(decade) %>%
  +     summarize(
    +         avg_anomaly = mean(Annual, na.rm = TRUE),
    +         .groups = "drop")

## Total Warming Difference from Start to Most Recent Values
warming_difference <- annual_df %>%
  drop_na(Annual) %>%
  summarize(
    first_year = min(Year),
    last_year = max(Year),
    anomaly_start = Annual[Year == min(Year)],
    anomaly_end = Annual[Year == max(Year)],
    total_warming = anomaly_end - anomaly_start)

warming_difference

## 4.Seasonal Variation
seasonal_df <- climate %>%
  + select(Year, DJF, MAM, JJA, SON) %>%
  + mutate(across(DJF:SON, as.numeric)) %>%
  + pivot_longer(cols = DJF:SON,
                 +     names_to = "Season",
                 +     values_to = "Anomaly") %>%
  + filter(!is.na(Anomaly))

seasonal_trends <- seasonal_df %>%
  + group_by(Season) %>%
  + summarize(
    +     slope = coef(lm(Anomaly ~ Year)) [2],
    +     .groups = "drop") %>%
  + arrange(desc(slope))


## Visualizations
## Time Series plot of annual anomalies
annual_anomaly_df <- climate %>%
  + select(Year, Annual = J.D) %>%
  + mutate(Annual = as.numeric(Annual)) %>%
  + filter(!is.na(Annual))


ggplot(annual_df, aes(x = Year, y = Annual)) +
  geom_line(color = "#1f77b4", linewidth = 1) +  
  geom_smooth(method = "lm", se = FALSE, color = "#d62728", linewidth = 1.2, linetype = "longdash") +  
  labs(
    title = "Global Surface Temperature Anomalies (1880–2024)",
    subtitle = "Annual mean anomalies relative to the long-term climate baseline",
    x = "Year",
    y = "Temperature Anomaly (°C)",
    caption = "Data Source: NASA GISS") +
  scale_x_continuous(limits = c(1880, 2024), breaks = seq(1880, 2024, by = 20)) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(
      color = "#1f77b4",
      face = "bold",
      size = 15,
      hjust = 0.5),
    plot.subtitle = element_text(
      color = "#d62728",
      size = 12,
      hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray85"),
    panel.grid.major.y = element_line(color = "gray90"),
    plot.caption = element_text(size = 10, face = "italic", hjust = 1))

## Climate Strips
ggplot(annual_df, aes(x = Year, y = 1, fill = Annual)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = c("darkblue", "blue", "white", "orange", "red")) +
  labs(
    title = "Climate Stripes: Rising Global Temperatures (1880–2024)",
    x = "Year",
    y = "",
    caption = "Data source: NASA GISS") +
  theme_void() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none")


## Smoothed Trend Plot- 10 Year Rolling Average
annual_anomaly_df <- annual_anomaly_df %>%
  +     mutate(RollingMean10 = rollmean(Annual, k = 10, fill = NA, align = "right"))


ggplot(annual_anomaly_df, aes(x = Year)) +
  +     geom_line(aes(y = Annual),
                  +               color = "gray40", alpha = 0.7, linewidth = 0.8) +
  +     geom_line(aes(y = RollingMean10),
                  +               color = "#d62728", linewidth = 1.4, lineend = "round") +
  +     scale_x_continuous(limits = c(1880, 2024),
                           +                        breaks = seq(1880, 2024, by = 20)) +
  +     labs(
    +         title = "Smoothed Global Temperature Trend (1880–2024)",
    +         subtitle = "10-Year Rolling Average of Annual Temperature Anomalies",
    +         x = "Year",
    +         y = "Temperature Anomaly (°C)",
    +         caption = "Data Source: NASA GISS") +
  +     theme_minimal(base_size = 13) +
  +     theme(
    +         plot.title = element_text(color = "#1f77b4", face = "bold", size = 15, hjust = 0.5),
    +         plot.subtitle = element_text(color = "#d62728", size = 12, hjust = 0.5),
    +         axis.title = element_text(face = "bold"),
    +         panel.grid.minor = element_blank(),
    +         panel.grid.major.x = element_line(color = "gray90"),
    +         panel.grid.major.y = element_line(color = "gray85"),
    +         plot.caption = element_text(size = 10, face = "italic", hjust = 1))

##Histogram + density overlay
annual_df1 <- climate %>%
  + select(Year, Annual = J.D) %>%
  + mutate(Annual = as.numeric(Annual)) %>%
  + filter(!is.na(Annual))
## Calculate mean anomaly (average mean for all years)
mean_anomaly <- mean(annual_df1$Annual, na.rm = TRUE)

ggplot(annual_df1, aes(x = Annual)) +
  +     geom_histogram(
    +         aes(y = ..density..),
    +         bins = 25,
    +         fill = "red",
    +         color = "black",
    +         alpha = 0.85) +
  +     geom_density(linewidth = 1.3, color = "black") +
  +     geom_vline(
    +         xintercept = mean_anomaly,
    +         color = "white",
    +         linetype = "dashed",
    +         linewidth = 1.2) +
  +     annotate(
    +         "text",
    +         x = mean_anomaly + 0.1,
    +         y = max(density(annual_df1$Annual)$y) * 0.85,
    +         label = paste0("Mean: ", round(mean_anomaly, 2), "°C"),
    +         color = "black",
    +         fontface = "bold") +
  +     labs(
    +         title = "Annual Temperature Anomalies",
    +         subtitle = "Histogram + density overlay (1880–2024)",
    +         x = "Temperature Anomaly (°C)",
    +         y = "Density") +
  +     theme_classic(base_size = 14) +
  +     theme(plot.title = element_text(face = "bold"))

## Heat map of Monthly Anomalies
ggplot(df, aes(x = Month, y = Year, fill = Anomaly)) +
  +     geom_tile() +
  +     scale_fill_gradientn(
    +         colors = c("darkblue", "blue", "white", "orange", "red"),
    +         name = "Anomaly (°C)") +
  +     labs(
    +         title = "Heatmap of Monthly Global Temperature Anomalies (1880–2024)",
    +         subtitle = "Deep red shading in recent decades highlights persistent warming",
    +         x = "Month",
    +         y = "Year",
    +         caption = "Source: NASA GISS") +
  +     theme_minimal(base_size = 12) +
  +     theme(
    +         panel.grid = element_blank(),
    +         plot.title = element_text(face = "bold"),
    +         axis.text.x = element_text(angle = 45, hjust = 1))













    
    





