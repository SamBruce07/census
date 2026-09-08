#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(tidycensus)
library(sf)
#
#
#
#| message: false
income_tx <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "TX",
  year = 2020,
  geometry = TRUE
)
#
#
#
#| message: false
#| cache: true
edu_state <- get_acs(
  geography = "state",
  variables = c("B15003_022", "B15003_023", "B15003_024", "B15003_025"),
  summary_var = "B15003_001",
  year = 2020
)
#
#
#
edu_state |>
  group_by(GEOID, NAME) |>
  summarise(
    adults_with_bachelors_or_higher = sum(estimate),
    total_adults = first(summary_est),
    percent_bachelors_or_higher =
      100 * adults_with_bachelors_or_higher / total_adults,
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = percent_bachelors_or_higher,
    y = reorder(NAME, percent_bachelors_or_higher)
  )) +
  geom_col(fill = "steelblue") +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Share of Adults with a Bachelor's Degree or Higher",
    x = "Adults with a bachelor's degree or higher",
    y = NULL,
    caption = "Source: U.S. Census Bureau, 2020 ACS 5-year estimates (B15003), via tidycensus."
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )
#
#
#
#| message: false
age_ca <- get_acs(
  geography = "county",
  variables = c(
    median_age = "B01002_001",
    population = "B01003_001"
  ),
  state = "CA",
  year = 2020,
  geometry = FALSE
)
#
#
#
print(age_ca)
#
#
#
#
#
