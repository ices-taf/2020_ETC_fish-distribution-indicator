## Prepare plots and tables for report

## Before:
## After:

library(icesTAF)
library(ggplot2)
library(sf)
library(dplyr)

mkdir("report")

# read in raw data
fig1_data <- read.taf("output/fig1_data.csv")
fig1_data <- sf::st_as_sf(fig1_data, wkt = "WKT", crs = 4326)

fig1_data %>%
  filter(F_CODE == "27.6.b") %>%
  as.data.frame()

sampled_statrecs <- unique(fig1_data["StatRec"])

plot(sampled_statrecs)

# equal area projection
crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

# read in areas
areas <- sf::read_sf("bootstrap/data/ICES-areas/ICES_Areas_20160601_cut_dense_3857.shp")

areas_sub <- areas[paste0("27.", gsub("([.][0-9]*)*$", "", areas$Area_27)) %in% unique(fig1_data$F_CODE), ]

# tranform projection
areas <- sf::st_transform(areas, crs = crs)
areas_sub <- sf::st_transform(areas_sub, crs = crs)
sampled_statrecs <- sf::st_transform(sampled_statrecs, crs = crs)


# -------------------------------------------------------------------
# Figure 1. Temporal development of the Lusitanian/Boreal species
#           ratio with 5 years interval, 1965-2018
# -------------------------------------------------------------------

# read in LB ratio by stat sq and year
fig1_data <- read.taf("output/fig1_data.csv")
fig1_data <-
  st_as_sf(
    fig1_data,
    coords = c("Lat", "Lon"),
    crs = 4326
  )
fig1_data <- sf::st_transform(fig1_data, crs = crs)

fig1_data <- fig1_data %>% dplyr::filter(Year %in% seq(1965, 2020, by = 5))

ggplot() +
  #geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
  geom_sf(
    data = fig1_data %>% st_transform(4326) %>% mutate(lbratio = cut(ratio, c(-Inf, 1, 2, Inf))),
    aes(fill = lbratio, col = lbratio),
    size = 1
  ) +
  facet_wrap(~ Year) +
  scale_fill_manual(
    name = "Lusitanian/Boreal species",
    values = c(rgb(141, 180, 226, maxColorValue = 255), rgb(255, 255, 133, maxColorValue = 255), rgb(255, 51, 0, maxColorValue = 255)),
    labels = c("Boreal dominance", "Lusitanian dominance", "High Lusitanian dominance"),
    aesthetics = c("fill", "colour"),
     guide = guide_legend(reverse = TRUE)
  )

ggplot2::ggsave("Figure1_temporal_ratio_map.png", path = "report/", width = 170*2, height = 100.5*2, units = "mm", dpi = 600)



# -------------------------------------------------------------------
# Figure 2. Temporal development in the number of species of each
#           biogeographical affinity group
# -------------------------------------------------------------------

# read in counts year and affinity
fig2_data <- read.taf("output/fig2_data.csv")

fig2_data <-
  tidyr::pivot_longer(
    fig2_data,
    cols = Atlantic:Unknown,
    names_to = "affinity",
    values_to = "count"
  ) %>%
  mutate(
    count = ifelse(is.na(count), 0, count)
  )

ggplot(fig2_data,
  aes(x = Year, y = count, col = factor(affinity))) +
  geom_line() +
  facet_wrap(~ F_CODE, scales = "free") +
  theme_minimal()

ggplot2::ggsave("Figure2_temporal_species_count.png", path = "report/", width = 170*2, height = 100.5*2, units = "mm", dpi = 600)


# -------------------------------------------------------------------
# Figure 3. Temporal development of the ratio between the number of
#           Lusitanian and Boreal species
# -------------------------------------------------------------------

fig3_data <- read.taf("output/fig3_data.csv")

# scale temperature to have same mean and slope as ratio??
fig3_data <-
  fig3_data %>%
    group_by(F_CODE) %>%
    mutate(
      sst1_scaled = (sst1 - mean(sst1)) / sd(sst1) * sd(ratio) + mean(ratio)
    ) %>%
    ungroup()

ggplot(fig3_data) +
  geom_line(aes(x = Year, y = ratio), col = rgb(0, 44, 86, maxColorValue = 255)) +
  geom_line(aes(x = Year, y = sst1_scaled), col = rgb(172, 0, 62, maxColorValue = 255)) +
  facet_wrap(~ F_CODE, scales = "free") +
  scale_y_continuous(
    "L/B ratio"#,
#    sec.axis = sec_axis(~ . * 1, name = "temperature anomaly (oC)")
  ) +
  theme_minimal()

ggplot2::ggsave("Figure3_temporal_ratio_sst.png", path = "report/", width = 170*2, height = 100.5*2, units = "mm", dpi = 600)


# -------------------------------------------------------------------
# Table 1. survey overview
# -------------------------------------------------------------------

# load control file
data_overview <- read.taf("bootstrap/data/control_file/control_file.csv")

# summarise the surveys used
table1_survey_overview <-
  data_overview %>%
  group_by(Division, Survey.name) %>%
  mutate(
    Quarter = paste(Quarter, collapse = ", "),
    Start.year = paste(unique(Start.year), collapse = ", ")) %>%
  unique() %>%
  arrange(Division, Quarter) %>%
  ungroup()

write.taf(table1_survey_overview, dir = "report", quote = TRUE)


# -------------------------------------------------------------------
# illustration 1. survey overview map
# -------------------------------------------------------------------

box <- sf::st_bbox(areas_sub)
xlims <- c(box[1], box[3])
ylims <- c(box[2], box[4])

p <-
  ggplot() +
    geom_sf(data = areas, color = "grey90", fill = "lightblue") +
    geom_sf(data = areas_sub, color = "grey90", fill = "grey60") +
    #geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
    geom_sf(data = sampled_statrecs, color = "grey60", alpha = 0.5) +
    theme(
      plot.caption = element_text(size = 6),
      plot.subtitle = element_text(size = 7),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    theme_bw(base_size = 8) +
    ggtitle("Overview of the ICES divisions and statistical rectangles sampled")

p
ggplot2::ggsave("Illustration1_sample_overview_map.png", path = "report/", width = 150, height = 150, units = "mm", dpi = 600)

