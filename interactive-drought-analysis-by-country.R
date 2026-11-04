# drought impact interactive bar chart

install.packages(c(
  "ggiraph",
  "patchwork",
  "sf",
  "rnaturalearth",
  "htmlwidgets"
))

library(tidyverse)
library(ggplot2)
library(ggiraph)
library(patchwork)
library(sf)
library(rnaturalearth)
library(htmlwidgets)

# loading data
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

# Filter to 2022
drought_2022 <- combined %>%
  filter(
    year == 2022,
    !is.na(drought_affected_pct),
    !is.na(drinking_water_access),
    !is.na(iso3c)
  ) %>%
  mutate(
    tooltip_text = paste0(
      "Country: ", country,
      "\nDrought impact: ",
      round(drought_affected_pct, 2), "%",
      "\nPeople affected: ",
      format(people_affected, big.mark = ","),
      "\nDrinking water access: ",
      round(drinking_water_access, 1), "%"
    )
  )

drought_2022 %>%
  count(iso3c) %>%
  filter(n > 1)

p1 <- ggplot(
  drought_2022,
  aes(
    x = drought_affected_pct,
    y = drinking_water_access
  )
) +
  geom_point_interactive(
    aes(
      tooltip = tooltip_text,
      data_id = iso3c
    ),
    color = "#8cadab",
    size = 3,
    alpha = 0.8
  ) +
  labs(
    title = "Drought Impact vs. Water Access",
    x = "Reported drought impact (% of population)",
    y = "Basic drinking water access (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )


top_15 <- drought_2022 %>%
  slice_max(
    drought_affected_pct,
    n = 15,
    with_ties = FALSE
  )

p2 <- ggplot(
  top_15,
  aes(
    x = reorder(country, drought_affected_pct),
    y = drought_affected_pct
  )
) +
  geom_col_interactive(
    aes(
      tooltip = tooltip_text,
      data_id = iso3c,
      fill = drought_affected_pct
    ),
    width = 0.7
  ) +
  scale_fill_gradient(
    low = "#B4DFDC",
    high = "#B74B36",
    guide = "none"
  ) +
  coord_flip() +
  labs(
    title = "Highest Reported Drought Impacts",
    x = NULL,
    y = "Reported drought impact (% of population)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank()
  )

world <- ne_countries(
  scale = 110,
  returnclass = "sf"
)

world_sf <- world %>%
  left_join(
    drought_2022 %>%
      select(
        iso3c,
        drought_affected_pct,
        tooltip_text
      ),
    by = c("iso_a3" = "iso3c")
  )

world_sf <- world_sf %>%
  filter(name != "Antarctica")

sum(!is.na(world_sf$drought_affected_pct))

anti_join(
  drought_2022,
  st_drop_geometry(world_sf),
  by = c("iso3c" = "iso_a3")
) %>%
  select(country, iso3c)

p3 <- ggplot() +
  geom_sf(
    data = world_sf,
    fill = "grey92",
    color = "white",
    linewidth = 0.15
  ) +
  geom_sf_interactive(
    data = world_sf %>%
      filter(!is.na(drought_affected_pct)),
    aes(
      fill = drought_affected_pct,
      tooltip = tooltip_text,
      data_id = iso_a3
    ),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "#B4DFDC",
    high = "#B74B36",
    name = "Drought impact (%)"
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-58, 85),
    expand = FALSE
  ) +
  labs(
    title = "Geographic Distribution of Reported Drought Impacts",
    caption = "Grey = no matched drought observation"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    legend.position = "bottom"
  )

combined_plot <- (p1 + p2) / p3 +
  plot_layout(
    heights = c(1, 3)
  ) +
  plot_annotation(
    title = "Global Drought Impact Exploration by Country",
    subtitle = "An examination of geographic localization of drought effects based on number of people impacted (2022)",
    caption = paste(
      "Sources: EM-DAT / Our World in Data,",
      "World Bank Population and Drinking Water Access"
    )
  )

interactive_plot <- girafe(
  code = print(combined_plot),
  width_svg = 14,
  height_svg = 12
)

interactive_plot <- girafe_options(
  interactive_plot,
  opts_hover(
    css = "stroke:#222222;stroke-width:2px;"
  ),
  opts_tooltip(
    css = paste(
      "background-color:white;",
      "color:#222222;",
      "padding:10px;",
      "border:1px solid #cccccc;",
      "border-radius:5px;"
    )
  ),
  opts_sizing(
    rescale = TRUE
  )
)

interactive_plot

dir.create(
  "HtmlWidget",
  showWarnings = FALSE
)

saveWidget(
  interactive_plot,
  "HtmlWidget/drought_dashboard.html",
  selfcontained = TRUE
)

