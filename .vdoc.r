#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::opts_chunk$set(warning = FALSE)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| warning: false
#| fig-cap: "Map of 5 fires in Greater Yellowstone of different severity"
#| label: fig-fires
#| fig-width: 8
#| fig-height: 12

library(landscapemetrics)
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyterra)
library(patchwork)

fires <- list.files('data', pattern = 'fire', full.names = T)

# set NAflag
flagged <- lapply(fires, function(f) {
  r <- rast(f)
  NAflag(r) <- 99
  crs(r) <- 'epsg:32612'  # manually assing CRS because originally missing
  levels(r) <- c("nodata", "unburned", "burned")
  return(r)
})

# plot
ggplot() +
  geom_spatraster(data = rast(flagged)) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_manual(values = c('unburned' = '#bfbc78', 'burned' = '#9f0a1a', 'modata' = '#ababab'))
#
#
#
# explore 1 raster data
flagged[[1]]
#
#
#
#
#
check_landscape(flagged[[1]])
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
lsm_p_area(flagged[[1]], directions = 8)
#
#
#
#
#
#
#
#
#
# all metrics by level and type
patch_level <- calculate_lsm(landscape = flagged[[1]], 
                             level = "patch", type = "area and edge metric")
patch_level

# metrics of interest
multiple_metrics <- calculate_lsm(landscape = flagged[[1]], 
                                  what = c("lsm_p_area", "lsm_p_para"))
multiple_metrics
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
# compute metrics
metrics_landscape <- calculate_lsm(landscape=flagged, what = c("lsm_l_ed", "lsm_l_contag"), neighbourhood = 8, directions = 8)
metrics_class <- calculate_lsm(landscape=flagged, what = c("lsm_c_pland", "lsm_c_np", "lsm_c_area_mn"), neighbourhood = 8, directions = 8)

# recode metrics
metrics_landscape <- metrics_landscape %>%
     mutate(layer = recode(layer, "1" = "fire_1_high",
         "2" = "fire_1_low",
         "3" = "fire_2_high",
         "4" = "fire_2_low",
         "5" = "fire_3_high",
         "6" = "fire_3_low",
         "7" = "fire_4_high",
         "8" = "fire_4_low",
         "9" = "fire_5_high",
         "10" = "fire_5_low"))
metrics_class <- metrics_class %>%
     mutate(layer = recode(layer, "1" = "fire_1_high",
         "2" = "fire_1_low",
         "3" = "fire_2_high",
         "4" = "fire_2_low",
         "5" = "fire_3_high",
         "6" = "fire_3_low",
         "7" = "fire_4_high",
         "8" = "fire_4_low",
         "9" = "fire_5_high",
         "10" = "fire_5_low"))
metrics_landscape$layer <- as.factor(metrics_landscape$layer)
metrics_landscape$metric <- as.factor(metrics_landscape$metric)
metrics_class$layer <- as.factor(metrics_class$layer)
metrics_class$class <- as.factor(metrics_class$class)
metrics_class$metric <- as.factor(metrics_class$metric)
metrics_landscape <- metrics_landscape %>% 
  separate(col=layer, into = c("fire", "landscape", "threshold"), sep="_") %>%
  mutate(rule = "8")
metrics_class <- metrics_class %>% 
separate(col=layer, into = c("fire", "landscape", "threshold"), sep="_") %>%
mutate(rule = "8")

# plot
pl <- ggplot(metrics_landscape, aes(x=threshold, y=value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(col=landscape), size=2, alpha=0.8) +
  facet_wrap(metric~., scales="free_y") +
  theme_bw()
pc <- ggplot(metrics_class, aes(x=threshold, y=value, fill=class)) +
  geom_boxplot(alpha = 0.7, outlier.shape=NA) +
  geom_jitter(alpha=0.4) +
  facet_grid(metric~class, scales="free_y") +
  scale_fill_manual(values=c("darkgreen", "tan4"), 
                       name="Class",
                       labels=c("Unburned", "Burned")) +
  xlab("dNBR Threshold") +
  theme_bw()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
