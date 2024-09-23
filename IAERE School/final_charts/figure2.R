rm(list = ls())

library(readr)
library(tidyverse)
library(magrittr)
library(ggtext)
library(ggallin)
library(ggpattern)
library("rnaturalearth")
library("rnaturalearthdata")

#set working directory
setwd("~/Documents/Teaching/IAERE School/climate_inequality-teaching")

data <- read_csv("data_revision/data_scenarios_projections_withSSP_novariazb50.csv")


pal_bot <- c(
  "High agricultural intensity"="#E58601",
  "Low agricultural intensity" ="#46ACC8"
)

#load map
world <- ne_countries(scale = "medium", returnclass = "sf")
world %<>% filter(continent!="Antarctica")

#load data for agri top/bottom classification
d2010<-read_csv("data_revision/d2010_rev_new.csv")#d2010_rev.csv")
d2010$country[is.na(d2010$country)]<-"NA"

#preliminaries on data
data %<>% 
  mutate(
    country2 = substr(country,1,2) #retrieve isoa2 country code
  ) %>% 
  mutate(
    country2 = ifelse(is.na(country2),"NA",country2), #fix namibia 
    country = ifelse(is.na(country), "NA", country)
  ) %>% 
  group_by(country2) %>% #variables that have one value per country, fill relative columns
  mutate(
    trend_impact_b50 = trend_impact_b50[country==country2], 
    trend_impact_gdp = trend_impact_gdp[country==country2],
    agri_top20 = agri_top20[country==country2],
    gdp_rate = gdp_rate[country==country2],
    b50_diff = b50_diff[country==country2]
  ) %>% ungroup() %>% 
  left_join( #merge data for top/bottom classification
    .,
    d2010 %>% select(country, top20_string) %>% distinct(), 
    by=c("country2"="country")
  ) %>% 
  mutate(agr_int= case_when(
    top20_string == "Bottom 80%" ~ "Low agricultural intensity",
    top20_string == "Top 20%" ~ "High agricultural intensity")
  ) 


#### Scatter data wrangling ####

#historical variations
data %>% 
  mutate(
    final_b50_all = initial_b50*(1+all_impact_prec_b50), 
    final_gdp_all_prec = initial_gdp*(1+all_impact_prec_gdp),
    final_gdp_all_prec_temp = initial_gdp*(1+all_impact_prec_and_temp_gdp),
    final_gdp_trend = initial_gdp*(1+trend_impact_gdp)
  ) %>% 
  mutate(
    ratio_b50 = final_b50_all/initial_b50, #bottom + prec + agrishare OVER bottom
    ###
    ratio_gdp_prec = final_gdp_all_prec/final_gdp_trend, #gdp+prec+trend OVER gdp+trend
    ratio_gdp_prec_2010 = final_gdp_all_prec/initial_gdp, #gdp+prec+trend OVER gdp2020
    ratio_gdp_prec_temp = final_gdp_all_prec_temp/final_gdp_trend, #gdp+prec+temp+trend OVER gdp+trend
    ratio_gdp_prec_temp_2010 = final_gdp_all_prec_temp/initial_gdp #gdp+prec+temp+trend OVER gdp2020
  ) -> data_scatter




#### Plot Scatter ####
data_scatter %>% 
  filter(country2 %in% c("RW", "UG", "GW", "NE", "LB", "FR", "NO", "IT", "YE", "TZ")) %>% 
  group_by(country2) %>% 
  summarise(
    mean_x = mean((ratio_gdp_prec*100)-100),
    mean_y = mean((ratio_b50*100)-100)
  ) -> data_text


#baseline scatter 
data_scatter %>%
  mutate(
    x = (ratio_gdp_prec*100)-100,
    y = (ratio_b50*100)-100
  ) %>% 
  mutate(title= "Effects of precipitations (country-model pairs)") %>% 
  ggplot() +
  theme_light() +
  geom_point(aes(x=x, y=y,color=agr_int), key_glyph = "point", size=0.8) +
  geom_density_2d_filled(aes(x=x, y=y, fill=agr_int, alpha=..level..), contour_var = "ndensity", bins=6)+
  geom_vline(xintercept=0, color="#969696") +
  geom_hline(yintercept=0, color="#969696") +
  geom_text(data = data_text, aes(x=mean_x, y=mean_y, label = country2), size=2.5) +
  xlab("Percentage change in GDP per capita") +
  ylab("Percentage change in bottom 50% income share") +
  labs(color = "Q agri emp")  +
  facet_wrap(.~title) +
  scale_x_continuous(
    breaks=c(-100, -50, -25, -10, -5, -2, -1, 0, 1, 2, 5, 10, 25, 50, 100, 500),
    trans = pseudolog10_trans) +
  scale_y_continuous(
    breaks=c(-100, -50, -25, -10, -5, -2, -1, 0, 1, 2,5, 10, 25, 50),
    trans = pseudolog10_trans) +
  scale_alpha_manual(values = c(0, 0.2,0.5, 0.75, 0.8, 0.9)) +
  scale_colour_manual(
    values = pal_bot,
    aesthetics = c("colour", "fill")
  ) +
  guides(color = "none", alpha = "none", fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.8,0.1),
    legend.background = element_rect(fill="transparent", colour = "transparent")
  ) -> scatter_main


# with temperature 
data_scatter %>%
  mutate(
    x = (ratio_gdp_prec_temp*100)-100,
    y = (ratio_b50*100)-100
  ) %>% 
  mutate(title= "Effects of precipitations and temperatures (country-model pairs)") %>% 
  ggplot() +
  theme_light() +
  geom_point(aes(x=x, y=y,color=agr_int), key_glyph = "point", size=0.8) +
  geom_density_2d_filled(aes(x=x, y=y, fill=agr_int, alpha=..level..), contour_var = "ndensity", bins=6)+
  geom_vline(xintercept=0, color="#969696") +
  geom_hline(yintercept=0, color="#969696") +
  xlab("Percentage change in GDP per capita") +
  ylab("Percentage change in bottom 50% income share") +
  labs(color = "Q agri emp")  +
  facet_wrap(.~title) +
  scale_x_continuous(
    breaks=c(-100, -50, -25, -10, -5, -2, -1, 0, 1, 2, 5, 10, 25, 50, 100, 500),
    trans = pseudolog10_trans) +
  scale_y_continuous(
    breaks=c(-100, -50, -25, -10, -5, -2, -1, 0, 1, 2,5, 10, 25, 50),
    trans = pseudolog10_trans) +
  scale_alpha_manual(values = c(0, 0.2,0.5, 0.75, 0.8, 0.9)) +
  scale_colour_manual(
    values = pal_bot,
    aesthetics = c("colour", "fill")
  ) +
  guides(color = "none", alpha = "none", fill = guide_legend(nrow = 2)) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.8,0.1),
    legend.background = element_rect(fill="transparent", colour = "transparent")
  ) -> scatter_temp




#### Maps data wrangling ####

#historical variations 

#create means for map, and code them with 1,2,3
data_scatter %>% 
  group_by(country2) %>% 
  summarise(
    across(
      starts_with("ratio"), ~ mean(.x), .names = "mean_{.col}"
    )
  ) %>% 
  group_by(country2) %>% 
  mutate(
    across(everything(), ~ case_when(
      .>1 ~ 1,
      is.na(.) ~ as.numeric(NA), #captures both NA and NaN. There are no NaNs at this stage btw
      T ~ 2
    ))
  ) -> data_map



#merge with world and transform to factors
left_join(
  world,
  data_map,
  by = c("iso_a2"="country2")
) %>% 
  mutate(
    mean_ratio_b50 = factor(
      mean_ratio_b50, 
      levels = c(2,1),
      labels = c("More Unequal","Less Unequal")
    ),
    across(
      c("mean_ratio_gdp_prec", "mean_ratio_gdp_prec_temp", "mean_ratio_gdp_prec_2010", "mean_ratio_gdp_prec_temp_2010"),
      ~ factor(
        .x,
        levels = c(2,1),
        labels = c("Poorer","Richer")
      )
    )
  ) -> world_data



#### Plot Maps ####

world_data %>% 
  mutate(
    title = "Average effects of precipitations on income distribution"
  ) %>% 
  ggplot() +
  theme_light() +
  geom_sf(aes(fill = mean_ratio_b50), size=0.2) +
  scale_fill_brewer(palette = "Pastel1", na.translate=F, direction = 1) + #or Dark2
  facet_wrap(.~title) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.box.margin=margin(-10,0,0,0),
    legend.key.size = unit(1,"line"),
    legend.text = element_text(size=9),
    plot.margin = margin(0,0,0,0, "pt"),
  ) -> map_b50_main


world_data %>% 
  mutate(
    title = "Average effects of precipitations and temperature on GDP p.c."
  ) %>% 
  ggplot() +
  theme_light() +
  geom_sf(aes(fill = mean_ratio_gdp_prec_temp), size=0.2) +
  scale_fill_brewer(palette = "Pastel2", na.translate=F, direction = -1) + #or Dark2
  facet_wrap(.~title) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid = element_blank(),
    legend.box.margin=margin(-10,0,0,0),
    legend.key.size = unit(1,"line"),
    legend.text = element_text(size=9),
    plot.margin = margin(0,0,0,0, "pt"),
  ) -> map_gdp_main


cowplot::plot_grid(map_b50_main, map_gdp_main, labels=c("B", "C"), nrow = 2) -> bottom_row
cowplot::plot_grid(scatter_main, bottom_row, ncol = 2, rel_widths = c(0.6,0.4), labels=c("A")) -> fig2
#ggsave("final_charts/charts/figure2.pdf", fig2, width = 17.8, height = 8.4, units = "cm", scale = 1.4)


#cowplot::plot_grid(map_b50_trend, map_gdp_main, labels=c("B", "C"), nrow = 2) -> bottom_row2
#cowplot::plot_grid(scatter_trend_b50, bottom_row2, ncol = 2, rel_widths = c(0.6,0.4), labels=c("A")) -> fig2_trend
#ggsave("final_charts/charts_si/figure2_b50trend.pdf", fig2_trend, width = 17.8, height = 8.4, units = "cm", scale = 1.4)


#ggsave("final_charts/charts_si/scatter_temp.pdf", scatter_temp, width = 14.8, height = 8.4, units = "cm", scale = 1.4)


