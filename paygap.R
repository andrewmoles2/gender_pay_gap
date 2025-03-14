# load libs ----
library(tidyverse)
library(lubridate)
library(geomtextpath)
library(showtext)
library(PostcodesioR)
library(sf)
library(rnaturalearth) 
library(rnaturalearthdata)
library(ggdist)
library(patchwork)
library(ggtext)
library(here)
library(tidytext)
library(glue)
library(MetBrewer)

# fetch a few cool fonts ----
font_add_google(c("Fira Code", "Source Code Pro",
                "Baloo 2", "MuseoModerno"))
showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)
f1 <- "Fira Code"
f2 <- "Source Code Pro"
f3 <- "Baloo 2"
f4 <- "MuseoModerno"

# load in data ----
paygap <- readr::read_csv("data/paygap.csv")
SIC_codes <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-28/SIC07_CH_condensed_list_en.csv')

glimpse(paygap)
# inspiration - https://twitter.com/nrennie35/status/1541861448237842433

# Read in grid shapefile ----
gb_grid <- sf::read_sf(file.path(getwd(), "data/gb_grid/gb_grid.shp"))
gb_post <- read_sf("data/postcode_polygons.gpkg")

# description using sic codes ----
paygap_sic <- left_join(paygap, SIC_codes, by = c("sic_codes" = "SIC Code")) %>%
  mutate(year = year(date_submitted))

paygap_token <- paygap_sic %>%
  unnest_tokens(word, Description) %>%
  anti_join(get_stopwords()) %>%
  na.omit()

paygap_prep <- paygap_token %>%
  select(employer_name, post_code, diff_mean_hourly_percent, 
         diff_median_hourly_percent, word, year) %>%
  mutate(pc_area = str_extract(post_code, "[A-Z][A-Z]*")) %>%
  filter(!word %in% c("activities", "n.e.c", "general", "non"))

# create aggregates for plotting ----
paygap_agg <- paygap_prep %>%
  group_by(pc_area) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_agg_year <- paygap_prep %>%
  group_by(pc_area, year) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_sic_agg <- paygap_prep %>%
  group_by(pc_area, word) %>%
  summarise(n = n(),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE),
            avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE)) %>%
  group_by(pc_area) %>%
  slice_max(order_by = n, n = 1) %>%
  ungroup() %>%
  add_count(pc_area, name = "ties") %>%
  mutate(word = if_else(ties > 1, "Multiple", word))

# join data to geo data - average over all years ----
paygrap_grid_sic <- gb_grid %>%
  left_join(paygap_sic_agg, relationship = "many-to-many")

paygap_post_sic <- gb_post %>%
  left_join(paygap_sic_agg, relationship = "many-to-many")

paygap_post <- gb_post %>%
  left_join(paygap_agg)

paygap_post_year <- gb_post %>%
  left_join(paygap_agg_year, relationship = "many-to-many")

# plot - two types, grid and post code areas ----
# import coolors function
devtools::source_gist("ffa7f0bae82f9df88c6c1d9458d980a8")

lvls <- sort(unique(paygap_sic_agg$word))

set.seed(2025)
sic_pal <- sample(met.brewer("Signac", n = 17))

ggplot(paygrap_grid_sic) +
  geom_sf(aes(fill = word), color = "grey90") +
  scale_fill_manual(values = sic_pal) +
  guides(fill = guide_legend(title = "Most common industry", 
                             ncol = 2,
                             override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = glue('Geogrid of most common industry per\narea between years {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}'),
    subtitle = glue("Company descriptions from SIC codes used \nto aggregate most common industries"),
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position.inside = c(1.6, 0.6),
    plot.margin = margin(0, 40, 5, 0),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
    #legend.key.spacing.x = unit(-100, "pt"),
    #legend.key.spacing.y = unit(2, "pt"),
    #legend.box.spacing = unit(1, "pt")
  )

ggsave("geogrid_sic_industry.png", dpi = 320, units = "px", 
       width = 2750, height = 3000, device = ragg::agg_png
)

ggplot(paygap_post_sic) +
  geom_sf(aes(fill = word), color = "white") +
  #scale_fill_manual(values = pal, breaks = lvls, drop = FALSE) +
  guides(fill = guide_legend(title = "Industry", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1)

# plot of all years ----
grad_pal <- coolors("https://coolors.co/gradient-maker/ffe985-75fae2-6492f5")
scales::show_col(grad_pal)

(median_diff_post <- ggplot(paygap_post) +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient(low = grad_pal[1], high = grad_pal[3]) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  #theme(
  #  plot.background = element_rect(fill = "grey95", color = NA),
  #  legend.position = c(2.0, 0.6),
  #  plot.margin = margin(0, 180, 0, 0)
  #) +
  plot_annotation(
    title = stringr::str_wrap('Difference in median men and womens pay by postal code area', 37),
    subtitle = glue("Average difference from {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}\nAbove 0 indicates men are paid more, 0 indicates equality"),
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position.inside = c(1.2, 0.6),
    plot.margin = margin(5, 40, 5, 0),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ))

ggsave("median_pay_diff_postcode.png", median_diff_post, 
       dpi = 320, units = "px", width = 2500, height = 3000, device = ragg::agg_png
       )

# plot change over years ----
paygap_post_year %>%
  filter(!is.na(year)) %>%
  ggplot() +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  facet_wrap(vars(year), ncol = 3) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = glue('Difference in median men and womens pay\nby postal code area between {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}'),
    subtitle = "Above 0 indicates men are paid more, 0 indicates equality",
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(1.25, 0.5), legend.direction = "vertical",
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ) -> median_diff_post_year
median_diff_post_year

ggsave("median_pay_diff_postcode_year.png", median_diff_post_year, 
       dpi = 320, units = "px", width = 2500, height = 3000, device = ragg::agg_png
)


# a plot for each year with gif ----
year_range <- range(paygap_post_year$year, na.rm = TRUE)
years <- seq(year_range[1], year_range[2])

for (i in years) {
  p <- paygap_post_year %>%
    filter(!is.na(year)) %>%
    filter(year == i) %>%
    ggplot() +
    geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
    scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
    guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
    coord_sf(clip = "off") +
    theme_void(base_family = f1) +
    plot_annotation(
      title = stringr::str_wrap('Yearly difference in median men and womens pay by postal code area', 37),
      subtitle = paste0("Above 0 indicates men are paid more, 0 indicates equality\n", "Year: ", i),
      caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
    ) &
    theme(
      plot.background = element_rect(fill = "grey97", color = NA),
      legend.position = c(1.2, 0.6), legend.direction = "vertical",
      plot.margin = margin(0, 70, 5, 1),
      plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
      plot.subtitle = element_text(family = f1, size = 12),
     plot.caption = element_text(family = f1, size = 10, hjust = 0)
    )

  print(p)

  ggsave(here("yearly_figures", paste0("paygap_plot_", i, ".png")), dpi = 320, 
       units = "px", width = 2500, height = 3000, device = ragg::agg_png
  )

}

library(magick)

list.files(
  path = here("yearly_figures"),
  full.names = TRUE,
  pattern = "paygap_plot"
) %>%
  lapply(., image_read) %>%
  image_join() %>%
  image_animate(fps = 1) %>%
  image_write(path = here("paygap.gif"))

# just universities ----
other_unis <- c(15943, 14878, 7818, 10903, 7715, 15789, 5609, 14976, 16008, 16801, 6539)

paygap_unis <- paygap |>
  filter(grepl("university", employer_name, ignore.case = TRUE)) |>
  bind_rows(
    paygap |>
      filter(employer_id %in% other_unis)
  ) |>
  mutate(year = year(date_submitted)) |>
  mutate(pc_area = str_extract(post_code, "[A-Z][A-Z]*"))

paygap_unis_agg <- paygap_unis %>%
  group_by(pc_area) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_unis_post <- gb_post %>%
  left_join(paygap_unis_agg)

paygap_unis_year <- paygap_unis %>%
  group_by(pc_area, year) %>%
  summarise(avg_diff_mean_hour = mean(diff_mean_hourly_percent, na.rm = TRUE),
            avg_diff_median_hour = median(diff_median_hourly_percent, na.rm = TRUE))

paygap_unis_post_year <- gb_post %>%
  left_join(paygap_unis_year, relationship = "many-to-many")

# * all years ----
ggplot(paygap_unis_post) +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient(low = grad_pal[1], high = grad_pal[3]) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  #theme(
  #  plot.background = element_rect(fill = "grey95", color = NA),
  #  legend.position = c(2.0, 0.6),
  #  plot.margin = margin(0, 180, 0, 0)
  #) +
  plot_annotation(
    title = stringr::str_wrap('Difference in median men and womens pay in UK universities by postal code area', 37),
    subtitle = glue("Average difference from {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}\nAbove 0 indicates men are paid more, 0 indicates equality"),
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position.inside = c(1.2, 0.6),
    plot.margin = margin(5, 40, 5, 0),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  )

ggsave("unis_median_pay_diff_postcode.png", dpi = 320, units = "px", 
       width = 2500, height = 3000, device = ragg::agg_png
)

# * change over years ----
paygap_unis_post_year %>%
  filter(!is.na(year)) %>%
  ggplot() +
  geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  facet_wrap(vars(year), ncol = 3) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = glue('Difference in median men and womens pay\nin UK universities\nby postal code area between {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}'),
    subtitle = "Above 0 indicates men are paid more, 0 indicates equality",
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(1.25, 0.5), legend.direction = "vertical",
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0))

ggsave("unis_median_pay_diff_postcode_year.png", dpi = 320, units = "px", 
       width = 2500, height = 3000, device = ragg::agg_png
)

# interactive image of gender pay gap for a single year - pull latest year ----
library(ggiraph)

# https://www.doogal.co.uk/PostcodeDistricts
postcode_info <- read_csv("data/Postcode_districts.csv")

postcode_info <- postcode_info %>%
  mutate(pc_area = gsub("[[:digit:]]+", "", Postcode)) %>%
  select(pc_area, Postcodes, Population, Region, `UK region`) %>%
  group_by(pc_area, Region, `UK region`) %>%
  summarise(n_postcodes = sum(Postcodes, na.rm = TRUE), 
            n_pop = sum(Population, na.rm = TRUE))

m_year <- max(paygap_post_year$year, na.rm = TRUE)

paygap_post_year <- paygap_post_year %>%
  left_join(postcode_info, by = join_by(pc_area), relationship = "many-to-many")

paygap_post_year %>%
  filter(!is.na(year)) %>%
  mutate(plot_text = paste0("PostCode area: ", pc_area, " has average median difference in pay of ", round(avg_diff_mean_hour, 2))) %>%
  ggplot() +
  ggiraph::geom_sf_interactive(aes(fill = avg_diff_median_hour,
                                   data_id = pc_area,
                                   tooltip = plot_text), 
                               colour = "white",
                               hover_nearest = TRUE) +
  #geom_sf(aes(fill = avg_diff_median_hour), colour = "white") +
  scale_fill_gradient2(low = grad_pal[1], mid = "#CBBFBD", high = grad_pal[3], midpoint = 10) +
  guides(fill = guide_legend(title = "Average median\ndifference in\npay per hour", override.aes = list(color = "grey95"))) +
  facet_wrap(vars(year), ncol = 3) +
  coord_sf(clip = "off") +
  theme_void(base_family = f1) +
  plot_annotation(
    title = glue('Difference in median men and womens pay\nby postal code area between {min(paygap_agg_year$year)}-{max(paygap_agg_year$year)}'),
    subtitle = paste0("Above 0 indicates men are paid more, 0 indicates equality\n"),
    caption = "Source: gender-pay-gap.service.gov.uk · Graphic: Andrew Moles"
  ) &
  theme(
    plot.background = element_rect(fill = "grey97", color = NA),
    legend.position = c(1.2, 0.6), legend.direction = "vertical",
    plot.margin = margin(0, 70, 5, 1),
    plot.title = element_text(family = f1, face = "bold", size = 20, margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = f1, size = 12),
    plot.caption = element_text(family = f1, size = 10, hjust = 0)
  ) -> pay_int

css_default_hover <- girafe_css_bicolor(primary = "#E2DBAA", secondary = "#ABB3E2")
set_girafe_defaults(opts_hover = opts_hover(css = css_default_hover))

girafe(ggobj = pay_int, fonts = list("Arial"),
       options = list(opts_toolbar(position = "bottom")))

