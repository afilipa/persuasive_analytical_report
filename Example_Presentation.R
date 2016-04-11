
## Data set size: ~ 2.7 million rows, ~ 41 million cells
## "Cells" aren't really data, but it gives approx sense of scale.



setwd("~/Documents/Visual\ Storytelling/ANLT234-Visual_Storytelling/Demonstrations/Overall_Prof_Mike")

library(readr)
library(ggplot2)
library(dplyr)

air_qual <- read_tsv('./data/AirBase_v6_statistics.csv')
air_qual_stations <- read_tsv('./data/AirBase_v6_stations.csv')
#dim(air_qual)


chems_with_signif_data <- c(
  "Arsenic (aerosol)",
  "Benzene (air)",
  "Black smoke (air)",
  "Cadmium (aerosol)",
  "Carbon monoxide (air)",
  "Lead (aerosol)",
  "Nickel (aerosol)",
  "Nitrogen dioxide (air)",
  "Nitrogen monoxide (air)",
  "Nitrogen oxides (air)",
  "Ozone (air)",
  "Particulate matter < 10 µm (aerosol)",
  "Particulate matter < 2.5 µm (aerosol)",
  "Strong acidity (air)",
  "Sulphur dioxide (air)",
  "Toluene (air)",
  "Total suspended particulates (aerosol)"
)

# Some of the interesting signals:
# Arsenic (aerosol)
# Benzene (air)
# Black smoke (air)
# Cadmium (aerosol)
# Carbon monoxide (air)
# Lead (aerosol)
# Nickel (aerosol)
# Nitrogen dioxide (air)
# Nitrogen monoxide (air)
# Nitrogen oxides (air)
# Particulate ammonium (aerosol)
# Particulate matter < 10 µm (aerosol)
# Particulate matter < 2.5 µm (aerosol)
# Strong acidity (air)
# Sulphur dioxide (air)
# Toluene (air)
# Total suspended particulates (aerosol)
# 
# 
# 
# 
# 


air_qual_df <- air_qual %>% filter(component_name %in% chems_with_signif_data)
air_qual_df <- inner_join(air_qual_stations, air_qual_df)

key_stats <- "50 percentile"
time_frames <- "day"

for (key_chem in chems_with_signif_data) {
  for (key_stat in key_stats) {
    for (time_frame in time_frames) {
      air_qual_subset <- air_qual_df %>% filter(component_name %in% key_chem &
                                                  statistic_name %in% key_stat &
                                                  statistics_average_group %in% time_frame)
      my_plt <- ggplot(air_qual_subset,
                       aes(factor(statistics_year),
                           statistic_value)) +
        geom_boxplot() +
        labs(title = paste0(key_chem, ", ", key_stat, " in 1 ", time_frame))
      print(my_plt)
    }
  }
}



#print(dim(air_qual_subset))
#print(dim(air_qual_stations))


# Ones that looked promising:

# Total suspended particulates (aerosol)
# Toluene (air)
# Sulphur dioxide (air)
# Strong acidity (air)
# (Somewhat of an improvement: Nitrogen oxides (air)  )
# Nitrogen monoxide (air)
# Nitrogen dioxide (air)
# (Somewhat of an improvement: Nickel (aerosol)  )
# Cadmium (aerosol)
# Black smoke (air)
# Benzene (air)


chems_with_signif_imprv <- c(
  "Total suspended particulates (aerosol)",
  "Toluene (air)",
  "Sulphur dioxide (air)",
  "Strong acidity (air)",
  "Nitrogen monoxide (air)",
  "Nitrogen dioxide (air)",
  "Cadmium (aerosol)",
  "Black smoke (air)",
  "Benzene (air)"
)

#662506
#993404
#cc4c02
#ec7014

environ_clrs <- rev(c(
  "#662506",
  "#800000", # full brown
  "#993404",
  "#BF812D",
  "#DFC27D",
  "#B8E186",
  "#7FBC41",
  "#4D9221" # full green
))


for (key_chem in chems_with_signif_imprv) {
  for (key_stat in key_stats) {
    for (time_frame in time_frames) {
      air_qual_subset <- air_qual_df %>%
        filter(component_name %in% key_chem &
                 statistic_name %in% key_stat &
                 statistics_average_group %in% time_frame) %>%
        group_by(statistics_year) %>%
        mutate(median_val = median(statistic_value))
      my_lims <- quantile(air_qual_subset$statistic_value, probs = c(0.1,0.9))
      max_val <- max(air_qual_subset$statistic_value)
      my_plt <- ggplot(air_qual_subset,
                       aes(factor(statistics_year, ordered = TRUE,
                                  levels=rev(sort(unique(statistics_year)))),
                           statistic_value)) +
        geom_boxplot(aes(fill=median_val)) +
        geom_text(data = summarize(air_qual_subset,
                                   med_value = median(statistic_value)),
                  aes(x = factor(statistics_year, ordered = TRUE,
                               levels=rev(sort(unique(statistics_year)))),
                      y = max_val,
                      label = round(med_value,
                                    -ceiling(log10(med_value)) + 3))) +
        labs(title = paste0(key_chem, ", ", key_stat, " in 1 ", time_frame)) +
        scale_fill_gradientn(colors = environ_clrs) +
        scale_x_discrete(breaks = years,
                         labels = years)
      print(my_plt)
    }
  }
}

year_labels <- c(
  1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010
)
key_stats <- "50 percentile"


air_qual_subset <- air_qual_df %>% filter(component_name %in% key_chems &
                                            statistic_name %in% key_stats)



#rhg_cols <- c("#771C19", "#AA3929", "#E25033", "#F27314", "#F8A31B", 
#              "#E2C59F", "#B6C5CC", "#8E9CA3", "#556670", "#000000")


#scale_fill_gradientn(colors = environ_clrs


  > dim(air_qual_df[grepl("croatia", air_qual_df]),])
Error: unexpected ']' in "dim(air_qual_df[grepl("croatia", air_qual_df]"
> dim(air_qual_df[grepl("croatia", air_qual_df),])



#############################################

# create a list of choropleths of presidential election results for each year
choropleths = list()
for (i in 2:ncol(df_president_ts)) {
  df           = df_president_ts[, c(1, i)]
  colnames(df) = c("region", "value")
  title        = paste0("Presidential Election Results: ", colnames(df_president_ts)[i])
  choropleths[[i-1]] = state_choropleth(df, title=title)
}


choroplethr_animate(choropleths)



#############################################


#acidity <- region, statistic_name, statistics_average_group, component_name, statistics_year


empty_df <- data.frame(region = euro_countries, value = as.numeric(NA),
                     stringsAsFactors = FALSE)
for (key_chem in chems_with_signif_imprv) {
  for (key_stat in key_stats) {
    for (time_frame in time_frames) {
      map_df <- air_qual_df %>%
        filter(component_name == key_chem &
                 statistic_name == key_stat &
                 statistics_average_group == time_frame &
                 region %in% euro_countries) %>%
        unique
      dir_name <- map_df$component_caption[1]
      if (!dir.exists(paste0("./", dir_name))) {
        dir.create(paste0("./", dir_name))
      }
      setwd(paste0("./", dir_name))
      plt_unit <- map_df$measurement_unit[1]
      map_df <- map_df %>% group_by(region, statistics_year) %>%
        summarize(value = median(statistic_value))
      legend_lims <- quantile(map_df$value, probs = c(0,1))
      choropleths = list()
      for (year in sort(unique(map_df$statistics_year))) {
        plt_df <- empty_df
        tmp_df <- map_df[map_df$statistics_year == year,]
        plt_title <- paste0(key_chem, "\nin ", plt_unit,
                            "\nYear: ", year)
        plt_df[plt_df$region %in% tmp_df$region, "value"] <-
          tmp_df$value
        my_plt <- country_choropleth(plt_df, zoom = euro_countries,
                                     num_colors = 1,
                                     title = plt_title)
        my_plt <- my_plt + xlim(-25, 45) + ylim(35, 75) +
          coord_map("lambert", lat0 = 28, lat1 = 81) +
          scale_fill_gradientn(name = key_chem,
                               colors = environ_clrs,
                               limits = legend_lims,
                               labels = scales::comma) +
          theme(plot.title = element_text(size = rel(2)),
                legend.title = element_blank(),
                legend.text = element_text(size = rel(2)))
        choropleths[[as.character(year)]] <- my_plt
        
      }
      choroplethr_animate(choropleths)
      setwd("..")
    }
  }
}

      
      

acidity <- air_qual_df %>%
  filter(component_name == chems_with_signif_imprv[1] &
           statistic_name == key_stats[1] &
           statistics_average_group == time_frames[1]  &
           statistics_year == 2009) %>%
  unique

acidity <- acidity %>% group_by(region) %>%
  summarize(value = median(statistic_value))
#  select(region, value = statistic_value) %>%
  
tmp <- data.frame(region = euro_countries, value = as.numeric(NA),
                  stringsAsFactors = FALSE)
tmp[tmp$region %in% acidity$region, "value"] <- acidity$value

my_plt <- country_choropleth(tmp, zoom=euro_countries, num_colors = 1)

my_plt <- my_plt + xlim(-25, 45) + ylim(35, 75) +
  coord_map("lambert", lat0 = 28, lat1 = 81) +
  scale_fill_gradientn(colors = environ_clrs)

################################

choro_obj <- CountryChoropleth$new(tmp)
choro_obj

coord_map("lambert", lat0 = 27.636, lat1 = 81.009) +
  

