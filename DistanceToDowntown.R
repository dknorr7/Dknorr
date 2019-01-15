library(pacman)
p_load(tidyverse, ggplot2, tigris, tmap)

raw_distances <- read.csv("FinalDistances.csv") %>% select(GEOID, DISTANCE)

average_dist <- raw_distances %>% group_by(GEOID) %>% summarise(value = mean(DISTANCE)) %>% mutate(variable = "Euclidean Distance to Downtown")

saveRDS(average_dist, "C://School//Research//Predictive Variables//Master//DistToDowntown.rds")

#quick Map
tracts_sp <- tracts("TN", county = "Davidson") 
joined <- geo_join(tracts_sp, average_dist, "GEOID", "GEOID", how = "inner")

map <- tm_shape(joined) + tm_fill("value", style = "quantile", palette = "Reds",
                                  title = "Median Home Value Sales") +
  tm_borders()+
  tm_layout(
    inner.margins = c(.06, .06, .06, .06),
    legend.title.size = .75,
    legend.text.size = .5,
    legend.bg.color = "grey",
    legend.bg.alpha = 0,
    legend.width = 0.235)