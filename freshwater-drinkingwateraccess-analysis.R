library(WDI)
library(tidyverse)
library(ggplot2)
library(plotly)

# comparing water stress (level of freshwater withdrawal) 
# with drinking water access (% of population) 
# based on World Bank Group data from the WHO/AQUASTAT
water_data <- WDI(
  country = "all",
  indicator = c(
    freshwater_stress = "ER.H2O.FWST.ZS",
    drinking_water_access = "SH.H2O.BASW.ZS", 
    population = "SP.POP.TOTL"
  ),
  start = 2015,
  end = 2024,
  extra = TRUE
)

glimpse(water_data)

water_2022 <- water_data %>%
  filter(
    year == 2022,
    region != "Aggregates",
    !is.na(freshwater_stress),
    !is.na(drinking_water_access)
  )

View(water_2022)

p <- ggplot(
  water_2022,
  aes(
    x = freshwater_stress,
    y = drinking_water_access,
    text = paste0(
      "Country: ", country,
      "<br>Water Stress: ", round(freshwater_stress, 1), "%",
      "<br>Water Access: ", round(drinking_water_access, 1), "%"
    )
  )
) +
  geom_point(
    color = "#168A94",
    size = 3,
    alpha = 0.7
  ) + 
  coord_cartesian(xlim = c(0, 150)
  ) +
  labs(
    title = "Water Stress vs Drinking Water Access",
    x = "Freshwater Stress (%)",
    y = "Access to Basic Drinking Water (%)"
  ) +
  theme_minimal()

ggplotly(p, tooltip = "text")

# linear correlation measurement
cor.test(
  water_2022$freshwater_stress,
  water_2022$drinking_water_access,
  method = "pearson"
)

# monotonic correlation measurement
cor.test(
  water_2022$freshwater_stress,
  water_2022$drinking_water_access,
  method = "spearman",
  exact = FALSE
)

# comparing drought severity (in terms of annaul 
# number of people impacted, data from EM-DAT/Our 
# World in Data) to the level of basic drinking water 
# access (data from AQUASTAT/United Nations)

drought_raw <- read_csv("annual-number-of-people-affected-by-droughts.csv")
glimpse(drought_raw)

# normalize drought dataset by country population
drought <- drought_raw %>%
  rename(
    country = Entity,
    iso3c = Code,
    year = Year,
    people_affected = all_of("Droughts")
  ) %>%
  filter(
    !is.na(iso3c),
    str_detect(iso3c, "^[A-Z]{3}$"),
    year >= 2015,
    year <= 2024
  )

glimpse(drought)

world_bank_clean <- water_data %>%
  filter(region != "Aggregates") %>%
  select(
    iso3c,
    country,
    year,
    population,
    drinking_water_access,
    freshwater_stress
  )

world_bank_clean %>%
  count(iso3c, year) %>%
  filter(n > 1)

drought %>%
  count(iso3c, year) %>%
  filter(n > 1)

combined <- drought %>%
  select(iso3c, year, people_affected) %>%
  inner_join(
    world_bank_clean,
    by = c("iso3c", "year"),
    relationship = "one-to-one"
  ) %>%
  filter(
    !is.na(population),
    population > 0
  ) %>%
  mutate(
    drought_affected_pct =
      people_affected / population * 100
  )

View(combined)

# create scatterplot of relationship
drought_2022 <- combined %>%
  filter(
    year == 2022,
    !is.na(drought_affected_pct),
    !is.na(drinking_water_access)
  )

p_drought <- ggplot(
  drought_2022,
  aes(
    x = drought_affected_pct,
    y = drinking_water_access
  )
) +
  geom_point(
    aes(
      text = paste0(
        "Country: ", country,
        "<br>People affected: ",
        format(people_affected, big.mark = ","),
        "<br>Drought impacts: ",
        round(drought_affected_pct, 2), "%",
        "<br>Drinking water access: ",
        round(drinking_water_access, 1), "%"
      )
    ),
    color = "#168A94",
    size = 3,
    alpha = 0.7
  ) +
  labs(
    title = "Drought Impacts vs Drinking Water Access",
    x = "Reported drought impacts (% of population)",
    y = "Basic drinking water access (%)"
  ) +
  theme_minimal(base_size = 12)

ggplotly(p_drought, tooltip = "text")

p_drought +
  geom_smooth(
    method = "lm",
    color = "#D46A55",
    fill = "#F0C7BD",
    se = TRUE
  )

nrow(drought_2022)

drought_2022 %>%
  select(
    country,
    drought_affected_pct,
    drinking_water_access
  ) %>%
  arrange(desc(drought_affected_pct))

# linear correlation measurement
cor.test(
  drought_2022$drought_affected_pct,
  drought_2022$drinking_water_access,
  method = "pearson"
)

# monotonoic correlation measurement
cor.test(
  drought_2022$drought_affected_pct,
  drought_2022$drinking_water_access,
  method = "spearman",
  exact = FALSE
)

library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)

deep_navy <- "#18344A"
teal <- "#50B4AD"
coral <- "#BF7B6E"
light_grid <- "#E8EEF0"
muted_text <- "#60717D"

p_drought <- ggplot(
  drought_2022,
  aes(
    x = drought_affected_pct,
    y = drinking_water_access
  )
) +
  
  geom_point(
    aes(
      text = paste0(
        "Country: ", country,
        "<br>People affected: ",
        comma(people_affected),
        "<br>Drought impact: ",
        round(drought_affected_pct, 2), "%",
        "<br>Drinking water access: ",
        round(drinking_water_access, 1), "%"
      )
    ),
    shape = 21,
    fill = teal,
    color = "white",
    stroke = 0.6,
    size = 3.5,
    alpha = 0.85
  ) +
  
  geom_smooth(
    aes(
      x = drought_affected_pct,
      y = drinking_water_access
    ),
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = coral,
    fill = coral,
    alpha = 0.12,
    linewidth = 1.1,
    inherit.aes = FALSE
  ) +
  
  scale_x_continuous(
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0.03, 0.06))
  ) +
  
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    breaks = seq(0, 100, 20),
    limits = c(0, 100)
  ) +
  
  labs(
    title = "Drought Impacts and Drinking Water Access",
    subtitle = paste(
      "Countries reporting drought impacts in 2022",
      "| Pearson r = -0.660 | p < 0.001"
    ),
    x = "Reported drought impacts (% of population)",
    y = "Population with basic drinking water access (%)",
    caption = paste(
      "Sources: EM-DAT / Our World in Data, World Bank"
    )
  ) +
  
  theme_minimal(base_size = 12, base_family = "Arial") +
  
  theme(
    text = element_text(
      family = "Arial",
      color = deep_navy
    ),
    
    plot.title = element_text(
      size = 15,
      face = "bold",
      color = deep_navy,
      margin = margin(b = 10)
    ),
    
    plot.subtitle = element_text(
      size = 11,
      color = muted_text,
      margin = margin(b = 18)
    ),
    
    axis.title = element_text(
      size = 10,
      face = "bold",
      color = deep_navy
    ),
    
    axis.title.x = element_text(
      margin = margin(t = 0)
    ),
    
    axis.title.y = element_text(
      margin = margin(r = 15)
    ),
    
    axis.text = element_text(
      size = 10,
      color = muted_text
    ),
    
    panel.grid.major = element_line(
      color = light_grid,
      linewidth = 0.4
    ),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    plot.caption = element_text(
      size = 9,
      color = muted_text,
      hjust = 0,
      margin = margin(t = 18)
    ),
    
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    
    plot.margin = margin(
      t = 20, r = 25, b = 20, l = 20
    )
  )

print(p_drought)

interactive_plot <- ggplotly(
  p_drought,
  tooltip = "text"
)

interactive_plot

library(htmlwidgets)

saveWidget(
  interactive_plot,
  file = "drought_water_access.html",
  selfcontained = TRUE,
  title = "Drought Impacts and Drinking Water Access"
)
