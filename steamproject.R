library(rvest)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(scales)

#
#setwd("directory of saved html files")

#' Estimate Steam Player Count
#'
#' Estimates the total number of players based on Steam reviews using the
#' method described by The Multiplayer Group.
#'
#' @param total_reviews Integer. The total number of reviews on Steam.
#' @param release_year Integer. The year the game was released (e.g., 2018).
#' @param price Numeric. The release price of the game.
#' @param review_score Numeric. The review score percentage (0-100).
#' @param manual_adjustment Numeric. Optional manual adjustment to the multiplier (e.g., -10, +20).
#'                          If 0, the function attempts a basic heuristic.
#'
#' @return A list containing the estimated player count, the adjusted reviews, and the final multiplier used.
estimate_steam_players <- function(reviews, followers, peak_players, release_year, price) {
  
  # Safety Check: If data is missing, return NA
  if (is.na(reviews) || is.na(followers)) {
    return(NA_real_)
  }
  
  # --- ESTIMATE 1: The Review Method (Boxleiter) ---
  base_mult <- 55 # Default for big games
  
  if (reviews < 1000) {
    base_mult <- 25      # Small games (Fans are vocal)
  } else if (reviews < 10000) {
    base_mult <- 35      # Mid-size games
  }
  
  # Adjust for "Old Game" Inflation (Pre-2017 reviews were scarcer)
  if (!is.na(release_year) && release_year < 2017) {
    base_mult <- base_mult + 20
  }
  
  # Adjust for Price (Expensive games have lower multipliers)
  if (!is.na(price) && price > 40) {
    base_mult <- base_mult - 10
  }
  
  est_reviews <- reviews * base_mult
  
  # --- ESTIMATE 2: The Follower Method (Hype) ---
  follower_mult <- 12 # Default "Lifetime" ratio
  if (!is.na(release_year) && release_year >= 2024) {
    follower_mult <- 7 # Newer games haven't converted all followers yet
  }
  
  est_followers <- followers * follower_mult
  
  # --- ESTIMATE 3: The Peak Player Method (Sanity Check) ---
  est_peak <- if (!is.na(peak_players)) peak_players * 40 else NA_real_
  
  # --- FINAL TRIANGULATION ---
  # We weight Reviews highest (60%), Followers (30%), Peak (10%)
  
  final_estimate <- 0
  
  if (!is.na(est_peak) && est_peak > 0) {
    final_estimate <- (est_reviews * 0.6) + (est_followers * 0.3) + (est_peak * 0.1)
  } else {
    # If no peak data, split 70/30
    final_estimate <- (est_reviews * 0.7) + (est_followers * 0.3)
  }
  
  return(round(final_estimate))
}


# --- Read HTM files into dataframe ---
parse_steamdb_file <- function(file_path) {
  
  # 1. Read the HTML file
  if (!file.exists(file_path)) {
    warning(paste("File not found:", file_path))
    return(NULL)
  }
  
  page <- read_html(file_path)
  
  # 2. Select the rows (excluding headers)
  rows <- page %>% html_elements("tr.app")
  
  # 3. Parse data
  steam_data <- rows %>%
    map_dfr(function(row) {
      
      # Extract Name
      name <- row %>% html_element(".b") %>% html_text(trim = TRUE)
      
      # Extract all 'td' cells
      cells <- row %>% html_elements("td")
      
      # Helper to extract 'data-sort' safely
      get_sort <- function(index) {
        if(index > length(cells)) return(NA_character_)
        html_attr(cells[[index]], "data-sort")
      }
      
      # Extract raw attributes based on your file structure
      # Index 1: Rank
      # Index 4: Discount %
      # Index 5: Price (Current, in cents)
      # Index 6: Rating %
      # Index 7: Release Date (Unix Epoch)
      # Index 8: Followers
      # Index 9: Reviews
      # Index 10: Peak Players
      
      tibble(
        Name          = name,
        Rank          = as.numeric(get_sort(1)),
        Discount_Pct  = as.numeric(get_sort(4)),
        Price_Raw     = as.numeric(get_sort(5)),
        Rating        = as.numeric(get_sort(6)),
        Release_Epoch = as.numeric(get_sort(7)),
        Followers     = as.numeric(get_sort(8)),
        Reviews       = as.numeric(get_sort(9)),
        Peak_Players  = as.numeric(get_sort(10)),
        # Add a Source column so you know which file this row came from
        Source_File   = basename(file_path)
      )
    })
  
  # 4. Clean up and Calculate Columns
  final_df <- steam_data %>%
    mutate(
      # Convert Cents to Dollars
      Price_Current = Price_Raw / 100,
      
      # Calculate True (Original) Price
      # Formula: Current / (1 - Discount%)
      Price_True = case_when(
        Discount_Pct > 0 ~ Price_Current / (1 - (Discount_Pct / 100)),
        TRUE ~ Price_Current
      ),
      
      # Format Date
      Release_Date = as.POSIXct(Release_Epoch, origin = "1970-01-01", tz = "UTC"),
      
      # Round prices
      Price_True = round(Price_True, 2)
    ) %>%
    # Select final columns
    select(
      Rank, Name, Price_Current, Price_True, Discount_Pct,
      Followers, Rating, Reviews, Peak_Players, Release_Date, Source_File
    )
  
  return(final_df)
}
files <- list.files(path = "C:/Users/sineg/Desktop/steam", pattern = "\\.htm$", full.names = TRUE)

# 2. Run the function on all files and combine into one big dataframe
all_years_data <- map_dfr(files, parse_steamdb_file)

# 3. View the combined result
print(all_years_data)

all_years_data_final <- all_years_data %>%
  mutate(
    Release_Year = year(Release_Date),
    # Ensure numerical columns are actually numbers (handling NAs as 0 for safety if needed)
    Reviews = coalesce(Reviews, 0),
    Followers = coalesce(Followers, 0),
    Peak_Players = coalesce(Peak_Players, 0)
  ) %>%
  rowwise() %>% # Critical: The function uses 'if' statements, so we must process row-by-row
  mutate(
    # DIRECTLY assign the result to owner_estimates (no more 'list' unpacking needed)
    owner_estimates = estimate_steam_players(
      reviews = Reviews,
      followers = Followers,
      peak_players = Peak_Players,
      release_year = Release_Year,
      price = Price_True
    )
  ) %>%
  ungroup() %>%
  # Re-arrange for viewing
  select(Rank, Release_Date, Name, Price_True, owner_estimates, everything())

# 3. View the result
print(head(all_years_data_final))


library(jsonlite) # You will need to install.packages("jsonlite")

# --- 1. Clean and Map Data for the Dashboard ---
dashboard_data <- all_years_data_final %>%
  mutate(
    # ESTIMATE WISHLISTS (SteamDB gives Followers. Rule of thumb is 7x-12x. We use 10x)
    Estimated_Wishlists = Followers * 10,
    
    # MAP RATING TO SENTIMENT LABEL (Matches your D3 Logic)
    Sentiment_Label = case_when(
      Rating >= 95 ~ "Overwhelmingly Positive",
      Rating >= 85 ~ "Very Positive",
      Rating >= 80 ~ "Positive",
      Rating >= 70 ~ "Mostly Positive", # Steam calls 70-79 Mostly Positive, D3 can map this to Positive
      Rating >= 40 ~ "Mixed",
      Rating >= 20 ~ "Mostly Negative",
      TRUE ~ "Negative" # Covers Negative and Overwhelmingly Negative
    ),
    
    # Create a simple Genre placeholder if you don't have it (or Color by Price Bucket)
    Genre_Placeholder = case_when(
      Price_True > 50 ~ "AAA / Premium",
      Price_True > 30 ~ "AA / Mid-Tier",
      Price_True > 15 ~ "Indie Premium",
      TRUE ~ "Indie Budget"
    )
  ) %>%
  # Filter for reasonable data (remove free games or things with 0 reviews)
  filter(Price_True > 0, Reviews > 10) %>%
  # Select only what the D3 needs to keep file size small
  select(
    title = Name,
    reviews = Reviews,
    price = Price_True,
    wishlists = Estimated_Wishlists,
    rating = Rating,
    sentiment = Sentiment_Label,
    genre = Genre_Placeholder,
    release_year = Release_Year
  )

# --- 2. Preview the Data ---
print(head(dashboard_data))

# --- 3. Export to JSON ---
# This saves the file in your working directory. 
# You will upload this 'steam_data.json' to your portfolio folder.
write_json(dashboard_data, "steam_data.json", pretty = TRUE)

message("Successfully exported steam_data.json!")

# --- Example Usage (based on 'Hunt: Showdown' from the article) ---
# Hunt: Showdown | Reviews: ~133,756 | Year: 2018 | Price: High | Score: Decent (Positive)
# Mine, 6673404 (11.03M - 13.98M)
# estimate_steam_players(
#   total_reviews = 361829,
#   release_year = 2020,
#   price = 30,
#   review_score = 96.26,
#   manual_adjustment = 58
# )
#2.708


expensive_games_trend <- all_years_data_final %>%
  # Filter for games released at $50 or more
  filter(Price_True >= 50) %>%
  # Ensure we have a Year column (if you haven't created it yet)
  mutate(Release_Year = year(Release_Date)) %>%
  # Group by Year to get the stats
  group_by(Release_Year) %>%
  summarise(
    Total_Owners = sum(owner_estimates, na.rm = TRUE),
    Average_Owners = mean(owner_estimates, na.rm = TRUE),
    Game_Count = n() # Useful context: how many $50+ games were released?
  )

# 2. Plot A: Total Market Volume (Are users buying fewer copies in total?)
ggplot(expensive_games_trend, aes(x = Release_Year, y = Total_Owners)) +
  geom_col(fill = "#2a475e") + # Steam Blue color
  geom_text(aes(label = label_number(scale_cut = cut_short_scale())(Total_Owners)),
            vjust = -0.5, color = "black") +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = min(expensive_games_trend$Release_Year):max(expensive_games_trend$Release_Year)) +
  theme_minimal() +
  labs(
    title = "Total Estimated Owners of $50+ Games by Year",
    subtitle = "Sum of owner estimates for all releases priced ≥ $50",
    x = "Release Year",
    y = "Total Estimated Owners"
  )

# 3. Plot B: Average Sales per Game (Is the average expensive game selling worse?)
ggplot(expensive_games_trend, aes(x = Release_Year, y = Average_Owners)) +
  geom_line(color = "#66c0f4", size = 1.5) +
  geom_point(color = "#1b2838", size = 3) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = min(expensive_games_trend$Release_Year):max(expensive_games_trend$Release_Year)) +
  theme_minimal() +
  labs(
    title = "Average Popularity of $50+ Games",
    subtitle = "Mean owner estimates per title priced ≥ $50",
    x = "Release Year",
    y = "Average Owners per Game"
  )




# 1. Isolate the 2025 expensive games
games_2025 <- all_years_data_final %>%
  mutate(Release_Year = year(Release_Date)) %>%
  filter(Release_Year == 2025, Price_True >= 50)

# 2. Look at the top 5 best sellers of 2025 (Did we miss a hit?)
print("--- Top 5 Best Selling $50+ Games of 2025 ---")
games_2025 %>%
  arrange(desc(owner_estimates)) %>%
  select(Name, Price_True, owner_estimates, Rating, Reviews) %>%
  head(5) %>%
  print()

# 3. Look at the bottom 5 (Are there many expensive failures?)
print("--- Bottom 5 Selling $50+ Games of 2025 ---")
games_2025 %>%
  arrange(owner_estimates) %>%
  select(Name, Price_True, owner_estimates, Rating, Reviews) %>%
  head(5) %>%
  print()

# 4. Compare the counts vs previous years
print("--- Count of $50+ Games per Year ---")
all_years_data_final %>%
  mutate(Release_Year = year(Release_Date)) %>%
  filter(Price_True >= 50) %>%
  count(Release_Year, name = "Number_of_Games") %>%
  print()



# 1. Calculate both Mean and Median
market_health_trend <- all_years_data_final %>%
  filter(Price_True >= 50) %>%
  mutate(Release_Year = year(Release_Date)) %>%
  group_by(Release_Year) %>%
  summarise(
    Median_Owners = median(owner_estimates, na.rm = TRUE),
    Mean_Owners = mean(owner_estimates, na.rm = TRUE),
    Game_Count = n()
  )

print(market_health_trend)

# 2. Plot Median (The "Typical" Game Performance)
ggplot(market_health_trend, aes(x = Release_Year, y = Median_Owners)) +
  geom_line(color = "#ff7f0e", size = 1.5) +   # Orange line for Median
  geom_point(color = "#1b2838", size = 3) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = min(market_health_trend$Release_Year):max(market_health_trend$Release_Year)) +
  theme_minimal() +
  labs(
    title = "Median Popularity of $50+ Games",
    subtitle = "The sales of the 'typical' expensive game (removing the mega-hit skew)",
    x = "Release Year",
    y = "Median Owners per Game"
  )

# 3. (Optional) Combined Plot to see the 'Mega-Hit Gap'
# This helps you see if the average is high just because of 1 or 2 games.
ggplot(market_health_trend, aes(x = Release_Year)) +
  geom_line(aes(y = Mean_Owners, color = "Mean (Average)"), size = 1.2) +
  geom_line(aes(y = Median_Owners, color = "Median (Typical)"), size = 1.2) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_color_manual(values = c("Mean (Average)" = "#66c0f4", "Median (Typical)" = "#ff7f0e")) +
  theme_minimal() +
  labs(
    title = "Market Skew: Average vs. Median Owners ($50+ Games)",
    y = "Estimated Owners",
    color = "Metric"
  )



# --- Step 1: Prepare the Data for Modeling ---
model_data <- all_years_data_final %>%
  filter(Price_True >= 50) %>%
  mutate(
    # FIX: Re-calculate Release_Year from the date column
    Release_Year = year(Release_Date),
    
    # Now we can calculate Age
    Age_Years = 2025 - Release_Year
  ) %>%
  # Filter out rows with missing data
  filter(!is.na(owner_estimates), !is.na(Rating), !is.na(Price_True))

# --- Step 2: Train the Model ---
# We exclude the current 2025 games from training
training_set <- model_data %>% filter(Release_Year < 2025)

model <- lm(log(owner_estimates) ~ Rating + Price_True + Age_Years, data = training_set)

# Check the model summary (Look for significance *** on Age_Years)
print(summary(model))

# --- Step 3: Make the Prediction ---
future_2025_games <- model_data %>%
  filter(Release_Year == 2025) %>%
  mutate(
    # Artificially age them by 1 year to see where they 'should' be
    Age_Years = 1 
  )

# Predict and reverse the log calculation
future_2025_games$projected_owners <- exp(predict(model, newdata = future_2025_games))

# --- Step 4: Compare Totals ---
current_2025_total <- sum(model_data %>% filter(Release_Year == 2025) %>% pull(owner_estimates))
projected_2025_total <- sum(future_2025_games$projected_owners)

cat(sprintf("Current 2025 Total: %.2f M\n", current_2025_total / 1e6))
cat(sprintf("Projected 2025 Total (Year 2): %.2f M\n", projected_2025_total / 1e6))

# --- Step 5: Visualizing the Growth ---
projection_plot_data <- tibble(
  Status = c("Current (Year 1)", "Projected (Year 2)"),
  Total_Owners = c(current_2025_total, projected_2025_total)
)

ggplot(projection_plot_data, aes(x = Status, y = Total_Owners, fill = Status)) +
  geom_col() +
  scale_fill_manual(values = c("Current (Year 1)" = "#2a475e", "Projected (Year 2)" = "#66c0f4")) +
  geom_text(aes(label = label_number(scale_cut = cut_short_scale())(Total_Owners)), 
            vjust = -0.5, size = 5) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(
    title = "Forecast: The 'Patient Gamer' Effect",
    subtitle = "Predicted growth of the 2025 cohort after 1 year on the market",
    y = "Total Estimated Owners"
  )

# 1. Calculate the 'Growth Factor' using the Median
# We compare the typical 1-year-old game (2024) to the typical 0-year-old game (2025)
medians <- all_years_data_final %>%
  filter(Price_True >= 50) %>%
  mutate(Release_Year = year(Release_Date)) %>%
  group_by(Release_Year) %>%
  summarise(Median_Owners = median(owner_estimates, na.rm = TRUE))

median_2024 <- medians %>% filter(Release_Year == 2024) %>% pull(Median_Owners)
median_2025 <- medians %>% filter(Release_Year == 2025) %>% pull(Median_Owners)

growth_multiplier <- median_2024 / median_2025

# 2. Apply this multiplier to your specific 2025 Total
current_2025_total <- 47.89 # from your previous chart
projected_2025_total <- current_2025_total * growth_multiplier

# 3. Print the Result
cat(sprintf("Median 2024 (Year 2): %.0f\n", median_2024))
cat(sprintf("Median 2025 (Year 1): %.0f\n", median_2025))
cat(sprintf("Growth Multiplier: %.2fx\n", growth_multiplier))
cat("---------------------------------\n")
cat(sprintf("Current 2025 Total: %.2f M\n", current_2025_total))
cat(sprintf("Projected 2025 Total: %.2f M\n", projected_2025_total))



# --- Step 1: Create the Price Buckets ---
price_analysis_data <- all_years_data_final %>%
  # Filter to only include games within your requested range (<= $50)
  filter(Price_True <= 50) %>%
  mutate(
    Price_Tier = cut(
      Price_True,
      breaks = c(0, 10, 20, 25, 30, 40, 50),
      labels = c("<$10", "$11-$20", "$21-$25", "$26-$30", "$31-$40", "$41-$50"),
      include.lowest = TRUE,
      right = TRUE # (10, 20] includes 20, but not 10. Perfect for thresholds.
    ),
    Release_Year = year(Release_Date)
  ) %>%
  filter(!is.na(Price_Tier)) # Remove any weird edge cases

# --- Step 2: Summarize the Data ---
tier_trends <- price_analysis_data %>%
  group_by(Release_Year, Price_Tier) %>%
  summarise(
    Median_Owners = median(owner_estimates, na.rm = TRUE),
    Mean_Owners = mean(owner_estimates, na.rm = TRUE),
    Total_Volume = sum(owner_estimates, na.rm = TRUE),
    Game_Count = n(),
    .groups = "drop"
  )

# --- Step 3: Plot A (Median Owners - The "Sweet Spot" Check) ---
ggplot(tier_trends, aes(x = Release_Year, y = Median_Owners, color = Price_Tier)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  # scales = "fixed" ensures you can compare the HEIGHT directly
  facet_wrap(~Price_Tier, scales = "fixed", ncol = 3) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Median Game Popularity by Price Range",
    subtitle = "Comparing typical sales performance across price bands (Fixed Scale)",
    x = "Release Year",
    y = "Median Owners"
  )

# --- Step 4: Plot B (Total Volume - Where the customers are) ---
ggplot(tier_trends, aes(x = Release_Year, y = Total_Volume, fill = Price_Tier)) +
  geom_col() +
  facet_wrap(~Price_Tier, scales = "fixed", ncol = 3) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  scale_fill_viridis_d(option = "plasma") + 
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "Total Market Volume by Price Range",
    subtitle = "Total estimated owners in each price bucket (Fixed Scale)",
    x = "Release Year",
    y = "Total Estimated Owners"
  )

# --- Step 1: Create the Specific Price Buckets ---
revenue_analysis_data <- all_years_data_final %>%
  filter(!is.na(Price_True), !is.na(owner_estimates)) %>%
  mutate(
    Price_Tier = cut(
      Price_True,
      # Breaks correspond to your requested tiers:
      # 0-10, 10-20, 20-25, 25-30, 30-40, 40-50, and 50+
      breaks = c(0, 10, 20, 25, 30, 40, 50, Inf), 
      labels = c("<$10", "$11-$20", "$21-$25", "$26-$30", "$31-$40", "$41-$50", ">$50"),
      include.lowest = TRUE,
      right = TRUE
    ),
    Release_Year = year(Release_Date),
    # Calculate Estimated Revenue for this game
    Est_Revenue = owner_estimates * Price_True
  ) %>%
  filter(!is.na(Price_Tier))

# --- Step 2: Group by Year and Tier ---
revenue_trends <- revenue_analysis_data %>%
  group_by(Release_Year, Price_Tier) %>%
  summarise(
    Total_Revenue = sum(Est_Revenue, na.rm = TRUE),
    Median_Owners = median(owner_estimates, na.rm = TRUE),
    Game_Count = n(),
    .groups = "drop"
  ) %>%
  # Calculate Percentage Share per Year
  group_by(Release_Year) %>%
  mutate(
    Yearly_Total_Revenue = sum(Total_Revenue),
    Revenue_Share = Total_Revenue / Yearly_Total_Revenue
  ) %>%
  ungroup()

# --- Step 3: Plot A - Where is the Money? (Total Revenue) ---
ggplot(revenue_trends, aes(x = Release_Year, y = Total_Revenue, fill = Price_Tier)) +
  geom_area(alpha = 0.8, size = 0.5, color = "white") +
  scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  theme_minimal() +
  labs(
    title = "Total Estimated Revenue by Price Tier",
    subtitle = "Are lower price tiers growing in total value?",
    x = "Release Year",
    y = "Total Estimated Revenue ($)",
    fill = "Price Tier"
  )

# --- Step 4: Plot B - The Shift (Percentage Share) ---
# This is the best chart to prove your 'Appetite has gone away' hypothesis
ggplot(revenue_trends, aes(x = Release_Year, y = Revenue_Share, fill = Price_Tier)) +
  geom_col(position = "fill", width = 0.8) +
  geom_text(aes(label = percent(Revenue_Share, accuracy = 1)), 
            position = position_fill(vjust = 0.5), 
            color = "white", size = 3, fontface = "bold", check_overlap = TRUE) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  theme_minimal() +
  labs(
    title = "Market Share of Revenue: The Shift to Cheaper Games",
    subtitle = "What % of total spending goes to each price bracket?",
    x = "Release Year",
    y = "Share of Total Revenue",
    fill = "Price Tier"
  )

print(tier_trends, n = 100)

library(scales)

# --- Step 1: Prepare the Data ---
cusum_data <- all_years_data_final %>%
  filter(Price_True <= 50) %>%
  mutate(
    Price_Tier = cut(
      Price_True,
      breaks = c(0, 10, 20, 25, 30, 40, 50),
      labels = c("<$10", "$11-$20", "$21-$25", "$26-$30", "$31-$40", "$41-$50"),
      include.lowest = TRUE,
      right = TRUE
    ),
    Release_Year = year(Release_Date)
  ) %>%
  filter(!is.na(Price_Tier)) %>%
  # Aggregate Total Volume per Year/Tier
  group_by(Release_Year, Price_Tier) %>%
  summarise(Total_Volume = sum(owner_estimates, na.rm = TRUE), .groups = "drop")

# --- Step 2: Calculate CUSUM ---
cusum_analysis <- cusum_data %>%
  group_by(Price_Tier) %>%
  mutate(
    # Calculate the long-term average volume for this specific tier
    Mean_Volume = mean(Total_Volume),
    
    # How much did this specific year deviate from the average?
    Deviation = Total_Volume - Mean_Volume,
    
    # Cumulative Sum of those deviations
    CUSUM = cumsum(Deviation)
  ) %>%
  ungroup()

# --- Step 3: Visualize the Shifts ---
ggplot(cusum_analysis, aes(x = Release_Year, y = CUSUM, color = Price_Tier)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") + # The 'Average' Line
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~Price_Tier, scales = "fixed", ncol = 3) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "CUSUM Analysis: Detecting Trend Shifts",
    subtitle = "Rising Line = Above Average Sales | Falling Line = Below Average Sales",
    y = "Cumulative Deviation from Mean",
    x = "Release Year"
  )

print(cusum_analysis, n = 100)


# --- Step 1: Prepare the Data (Using REVIEWS this time) ---
cusum_reviews <- all_years_data_final %>%
  filter(Price_True <= 50) %>%
  mutate(
    Price_Tier = cut(
      Price_True,
      breaks = c(0, 10, 20, 25, 30, 40, 50),
      labels = c("<$10", "$11-$20", "$21-$25", "$26-$30", "$31-$40", "$41-$50"),
      include.lowest = TRUE,
      right = TRUE
    ),
    Release_Year = year(Release_Date)
  ) %>%
  filter(!is.na(Price_Tier)) %>%
  # AGGREGATE by sum of REVIEWS (not owner estimates)
  group_by(Release_Year, Price_Tier) %>%
  summarise(Total_Reviews = sum(Reviews, na.rm = TRUE), .groups = "drop")

# --- Step 2: Calculate CUSUM ---
cusum_reviews_analysis <- cusum_reviews %>%
  group_by(Price_Tier) %>%
  mutate(
    # Calculate the long-term average review volume
    Mean_Reviews = mean(Total_Reviews),
    
    # Deviation and Cumulative Sum
    Deviation = Total_Reviews - Mean_Reviews,
    CUSUM = cumsum(Deviation)
  ) %>%
  ungroup()

# --- Step 3: Visualize ---
ggplot(cusum_reviews_analysis, aes(x = Release_Year, y = CUSUM, color = Price_Tier)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~Price_Tier, scales = "fixed", ncol = 3) + 
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  labs(
    title = "CUSUM Analysis: Review Volume (Raw Engagement)",
    subtitle = "Are players actually leaving reviews, or just buying silently?",
    y = "Cumulative Deviation (Reviews)",
    x = "Release Year"
  )

# --- Step 4: Print the values for inspection ---
print(cusum_reviews_analysis, n = 100)


# --- Step 1: Count Games per Year and Tier ---
volume_check <- all_years_data_final %>%
  filter(Price_True <= 50) %>%
  mutate(
    Price_Tier = cut(
      Price_True,
      breaks = c(0, 10, 20, 25, 30, 40, 50),
      labels = c("<$10", "$11-$20", "$21-$25", "$26-$30", "$31-$40", "$41-$50"),
      include.lowest = TRUE,
      right = TRUE
    ),
    Release_Year = year(Release_Date)
  ) %>%
  filter(!is.na(Price_Tier)) %>%
  count(Release_Year, Price_Tier, name = "Game_Count") %>%
  group_by(Release_Year) %>%
  mutate(Total_Games = sum(Game_Count)) %>%
  ungroup()

# --- Step 2: Plot the "Explosion" of Choice ---
ggplot(volume_check, aes(x = Release_Year, y = Game_Count, fill = Price_Tier)) +
  geom_col(color = "white", size = 0.2) + # Stacked bars
  
  # Add labels for the Total Count at the top of each bar
  geom_text(aes(y = Total_Games, label = Total_Games), 
            vjust = -0.5, check_overlap = TRUE, size = 4, fontface = "bold", color = "gray20") +
  
  scale_fill_viridis_d(option = "plasma", direction = -1) +
  theme_minimal() +
  labs(
    title = "Explosion of Choice: Number of Games Released per Year",
    subtitle = "Are there more options now than in 2018?",
    y = "Number of Games Released",
    x = "Release Year",
    fill = "Price Tier"
  )



# --- Step 1: Granular Binning ---
granular_price_data <- all_years_data_final %>%
  # Filter for the "Core" market range you requested
  filter(Price_True >= 9.5, Price_True <= 60.5) %>%
  mutate(
    # Round to nearest dollar (e.g., 19.99 -> 20, 24.99 -> 25)
    # This groups "psychological" pricing together
    Rounded_Price = round(Price_True)
  ) %>%
  # Focus on recent years to capture CURRENT market behavior (optional)
  filter(year(Release_Date) >= 2023)

# --- Step 2: Aggregate by Specific Price Point ---
price_point_summary <- granular_price_data %>%
  group_by(Rounded_Price) %>%
  summarise(
    Total_Volume = sum(owner_estimates, na.rm = TRUE),
    Median_Owners = median(owner_estimates, na.rm = TRUE),
    Game_Count = n(),
    Total_Revenue = sum(owner_estimates * Price_True, na.rm = TRUE)
  ) %>%
  ungroup()

# --- Step 3: Plot A - Total Market Volume (Where are the buyers?) ---
ggplot(price_point_summary, aes(x = Rounded_Price, y = Total_Volume)) +
  geom_col(fill = "#2a475e", width = 0.8) +
  scale_x_continuous(breaks = seq(10, 60, by = 5)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(
    title = "Total Market Volume by Price Point (2023-2025)",
    subtitle = "Aggregated owner estimates for each specific price tag",
    x = "Price ($)",
    y = "Total Estimated Owners"
  )

# --- Step 4: Plot B - The 'Efficiency' Curve (Median Sales) ---
# This reveals the "Magic Numbers" where players feel comfortable buying
ggplot(price_point_summary, aes(x = Rounded_Price, y = Median_Owners)) +
  geom_line(color = "#66c0f4", size = 1) +
  geom_point(color = "#1b2838", size = 3) +
  geom_text(aes(label = ifelse(Median_Owners > quantile(Median_Owners, 0.8), paste0("$", Rounded_Price), "")),
            vjust = -1, size = 3, fontface = "bold") +
  scale_x_continuous(breaks = seq(10, 60, by = 5)) +
  scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
  theme_minimal() +
  labs(
    title = "Sales Efficiency: Median Owners by Price Point",
    subtitle = "Which specific price tags correlate with higher sales success?",
    x = "Price ($)",
    y = "Median Owners per Game"
  )

# --- Step 5: Plot C - The 'Crowdedness' Map (Game Count) ---
ggplot(price_point_summary, aes(x = Rounded_Price, y = Game_Count)) +
  geom_area(fill = "gray80", alpha = 0.5) +
  geom_line(color = "gray50") +
  scale_x_continuous(breaks = seq(10, 60, by = 5)) +
  theme_minimal() +
  labs(
    title = "Competition Map: Releases by Price Point",
    subtitle = "Where are most developers pricing their games?",
    x = "Price ($)",
    y = "Number of Games Released"
  )

print(price_point_summary, n = 100)


# --- Step 1: Prepare the Granular Data (Same as before) ---
granular_price_data <- all_years_data_final %>%
  # Filter for the "Core" market range ($10 - $60)
  filter(Price_True >= 9.5, Price_True <= 60.5) %>%
  mutate(
    # Round to nearest dollar (e.g., 19.99 -> 20)
    Rounded_Price = round(Price_True)
  ) %>%
  # Focus on recent years (2023-2025) to match your previous view
  filter(year(Release_Date) >= 2023)

# --- Step 2: Summarize by Reviews ---
review_point_summary <- granular_price_data %>%
  group_by(Rounded_Price) %>%
  summarise(
    Total_Reviews = sum(Reviews, na.rm = TRUE),
    Median_Reviews = median(Reviews, na.rm = TRUE),
    Game_Count = n()
  ) %>%
  ungroup()

# --- Step 3: Print the Results ---
print(review_point_summary, n = 100)
