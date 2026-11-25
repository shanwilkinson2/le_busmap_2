library(data.table)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(tidyr)
library(tidytext)
library(forcats)
library(stringr)

# change these 
  wanted_route_num <- "533"
  wanted_route_agency_name <- "Bee Network"
# replace file_location wtih "" if file is in the project location
  file_location <- "C:/users/wilkinsons5/Downloads/"

# download 'stop & route data' from the app https://shanwilkinson2.shinyapps.io/bus_le_hle_app/

wanted_route <- fread(paste0(file_location, "bus_le_hle_full_route_data.csv")) %>%
  filter(route_short_name == wanted_route_num & agency_name == wanted_route_agency_name)

wanted_route_summary <- wanted_route %>%
  group_by(sex, metric, MSOA21CD, area_name) %>%
  summarise(value = max(value)) %>%
  filter(!is.na(value)) %>%
  ungroup() %>%
  group_by(sex, metric) %>%
  mutate(is_extreme = value == max(value) | value == min(value)) %>%
  ungroup() %>%
  group_by(MSOA21CD, area_name, sex) %>%
  mutate(any_extreme = any(is_extreme)) %>%
  ungroup() %>%
  filter(any_extreme == TRUE) %>%
  select(-c(any_extreme, is_extreme, MSOA21CD)) %>%
  # smithills is slighly lower years in poor health for m but only tiny
  filter(!(sex == "Male" & area_name == "Smithills")) %>% 
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(area_order = reorder_within(area_name, desc(`Life Expectancy`), sex)) %>%
  droplevels()

ext <- wanted_route_summary %>%
  group_by(sex) %>%
  summarise(
    le_row = area_order[which.min(`Life Expectancy`)],
    le_lo = min(`Life Expectancy`), 
    le_hi = max(`Life Expectancy`), 
    hle_row = area_order[which.max(`Healthy Life Expectancy`)],
    hle_lo = min(`Healthy Life Expectancy`),
    hle_hi = max(`Healthy Life Expectancy`)
  )

off <- 1 # gap so the line starts after the dot's value label

  #myplot <- 
    ggplot(wanted_route_summary, aes(y = area_order)) +
    # solid bar - difference between LE & HLE  
    geom_segment(aes(x = `Healthy Life Expectancy`, xend = `Life Expectancy`, yend = area_order), 
                 colour = "grey70", linewidth = 1.5) +
    # extender bar LE  
    geom_segment(data = ext, 
                 aes(x = le_lo + off, xend = le_hi, y = le_row, yend = le_row),
                 linetype = "dotted", linewidth = 1.5, colour = "grey70", inherit.aes = FALSE) +
    # extender bar HLE  
    geom_segment(data = ext, 
                 aes(x = hle_hi - off, xend = hle_lo, y = hle_row, yend = hle_row),
                 linetype = "dotted", linewidth = 1.5, colour = "grey70", inherit.aes = FALSE) +  
    # HLE dumbell  
    geom_point(aes(x = `Healthy Life Expectancy`, colour = "Healthy Life Expectancy"), size = 8) +
    # LE dumbell  
    geom_point(aes(x = `Life Expectancy`, colour = "Life Expectancy"), size = 8) +  
    # yrs not in good health value  
    geom_text(aes(x = (`Healthy Life Expectancy` + `Life Expectancy`)/2, label = paste(round(`Years not in good health`), "yrs")),
              vjust = -1, fontface = "bold", size = 3) +
    # HLE value  
    geom_text(aes(x = `Healthy Life Expectancy`, y = area_order, label = round(`Healthy Life Expectancy`)), 
                size = 3, fontface = "bold") +
    # LE value  
    geom_text(aes(x = `Life Expectancy`, y = area_order, label = round(`Life Expectancy`)), 
                size = 3, fontface = "bold") +  
    # inequality in LE value  
    geom_text(data = ext, aes(x= (le_lo + le_hi)/2 + off/2, y = le_row,
                            label = paste(round(le_hi - le_lo), "yrs")),
              vjust = -0.8, size = 3, inherit.aes = FALSE)  +  
    # inequality in HLE value  
    geom_text(data = ext, aes(x= (hle_lo + hle_hi)/2 + off/2, y = hle_row,
                              label = paste(round(hle_hi - hle_lo), "yrs")),
              vjust = -0.8, size = 3, inherit.aes = FALSE)  + 
    scale_x_continuous(limits = c(48, 90)) +
    facet_wrap2(~sex, nrow =2, scales = "free_y", 
               strip = strip_themed(
                 background_x = elem_list_rect(fill = c("#75000F", "#0F3933")), 
                                               text_x = elem_list_text(
                                                 colour = "white", face = "bold", size = 11))
               ) +
    scale_y_reordered() +
    scale_colour_manual(values = c("Healthy Life Expectancy" = "#B6CFAE", 
                                   "Life Expectancy" = "#EFBE7D")) +
    labs(title = paste0("Travelling along the ", wanted_route_agency_name, " ", wanted_route_num, "..."), 
         subtitle = paste0("\u2022 ", 
                          round(min(ext$le_hi - ext$le_lo)),
                          "-",
                          round(max(ext$le_hi - ext$le_lo)),
                          " years difference in life expectancy\n",
                          "\u2022 ", 
                          round(min(ext$hle_hi - ext$hle_lo)),
                          "-",
                          round(max(ext$hle_hi - ext$hle_lo)),
                          " years difference in healthy life expectancy\n",
                          "\u2022 Up to ", 
                          round(max(wanted_route_summary$`Years not in good health`)),
                          " years not in good health\n"
                          ),
         x = "Years", 
         y = NULL, 
         colour = NULL) +
    theme_minimal(base_size = 12)+
    theme(legend.position = "bottom", 
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.title.position = "plot",
          plot.subtitle = element_text(margin = margin (l = 20))
          )

  ggsave(paste0(file_location, "plot.png"), plot = myplot, width = 14.39, height = 12.09, 
         units = "cm", dpi = 300, bg = "white")  
  