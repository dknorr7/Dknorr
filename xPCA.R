###PCA
library(pacman)
p_load(tidyverse, lubridate, ggplot2, pROC,
       tigris, tmap, corrplot, mlbench, caret, Boruta, verification, randomForest, e1071, sf, factoextra, cowplot)

All_vars_10_16 <- readRDS("AllVars10to16.rds")
All_Census <- readRDS("All_census.rds")
Geolytics90 <- readRDS("Geolytics90.rds")
Geolytics00 <- readRDS("Geolytics00.rds")


#Rename Geolytics Columns
Geolytics00x <- Geolytics00 %>% dplyr::select(CensusTractID, rent = RentVal00, Income = HH_income00, homeValue = HomeVal00, Pct_MultiUnit = PctMulti00, PctNonFam = pctNonfamily00, PctNonWhite = nonWhite00, PctRenters = Pct_Renter00, PctVacant = pctVacant00, Pct_Unemployed = Unemployment_00, Pct_Poverty = Pov_00, PctCollege = PctCollege00, Under18 = under18_00, Over65x = Over65_00)

Geolytics90x <- Geolytics90 %>% dplyr::select(CensusTractID, rent = RentVal90, Income = HH_income90, homeValue = HomeVal90, Pct_MultiUnit = PctMulti90, PctNonFam = pctNonfamily90, PctNonWhite = nonWhite90, PctRenters = Pct_Renter90, PctVacant = pctVacant90, Pct_Unemployed = Unemployment_90, Pct_Poverty = Pov_90, PctCollege = PctCollege90, Under18 = Under18_90, Over65x = Over65_90)


#PCA
trainPCA2010 <- All_Census %>% filter(year == 2010 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) 
PCA11<- All_Census %>% filter(year == 2011 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA12<- All_Census %>% filter(year == 2012 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA13<- All_Census %>% filter(year == 2013 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA14<- All_Census %>% filter(year == 2014 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA15<- All_Census %>% filter(year == 2015 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA16<- All_Census %>% filter(year == 2016 & complete.cases(.)) %>% dplyr::select(-CensusTractID, -year, -Evictions) %>% scale()
PCA90<- Geolytics90x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID) %>% scale()
PCA00<- Geolytics00x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID) %>% scale()


#PCA Named in Order to join back to after PCA results
PCA10n <- All_Census %>% filter(year == 2010 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA11n<- All_Census %>% filter(year == 2011 & complete.cases(.)) %>% dplyr::select(CensusTractID, year) 
PCA12n<- All_Census %>% filter(year == 2012 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA13n<- All_Census %>% filter(year == 2013 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA14n<- All_Census %>% filter(year == 2014 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA15n<- All_Census %>% filter(year == 2015 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA16n<- All_Census %>% filter(year == 2016 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA90n<- Geolytics90x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 1990)
PCA00n<- Geolytics00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 2000)

AllPCAscaled <- rbind(trainPCA2010, PCA11, PCA12, PCA13, PCA14, PCA15, PCA16)
#Transform all years to Base year PCA Loadings
prin_comp <- prcomp(trainPCA2010, scale. = T)
PCAval10 <- prin_comp$x %>% cbind(PCA10n)
PCAval10x <- predict(prin_comp, trainPCA2010)
PCAval11 <- predict(prin_comp, PCA11) %>% cbind(PCA11n)
PCAval12 <- predict(prin_comp, PCA12) %>% cbind(PCA12n)
PCAval13 <- predict(prin_comp, PCA13) %>% cbind(PCA13n)
PCAval14 <- predict(prin_comp, PCA14) %>% cbind(PCA14n)
PCAval15 <- predict(prin_comp, PCA15) %>% cbind(PCA15n)
PCAval16 <- predict(prin_comp, PCA16) %>% cbind(PCA16n)
PCAval90 <- predict(prin_comp, PCA90) %>% cbind(PCA90n)
PCAval00 <- predict(prin_comp, PCA00) %>% cbind(PCA00n)


ALL_PCA <- rbind(PCAval10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16) %>% reshape2::melt(id.vars = c("CensusTractID", "year")) 
ALL_PCA_wide <- rbind(PCAval10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16)


ALL_PC1 <- ALL_PCA %>% filter(variable == "PC1" & year != 2010)
ALL_PC2 <- ALL_PCA %>% filter(variable == "PC2" & year != 2010)
ALL_PC3 <- ALL_PCA %>% filter(variable == "PC3" & year != 2010)

ALL_PC1x <- ALL_PCA %>% filter(variable == "PC1")
ALL_PC2x <- ALL_PCA %>% filter(variable == "PC2")


tracts <- tracts("Tennessee", "Davidson County")
t <- st_as_sf(tracts)

tracts_sf_PC1 <- merge(t, anomaly_pc_df, by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC2 <- merge(t, ALL_PC2, by.x = "GEOID", by.y = "CensusTractID")

ggplot(data = tracts_sf_PC1) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()

ggplot(data = tracts_sf_PC2) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()


biplot10 <-fviz_pca_biplot(prin_comp)


#plot all PC distributions 

my_plots <- lapply(names(ALL_PCA_wide), function(var_x){
  p <- 
    ggplot(ALL_PCA_wide) +
    aes_string(var_x)
  
  if(is.numeric(ALL_PCA_wide[[var_x]])) {
    p <- p + geom_density()
    
  } 
  
})

plot_grid(plotlist = my_plots)


#With no 2010
All_PCA_No2010 <- ALL_PCA_wide %>% filter(year != 2010)

my_plots2 <- lapply(names(All_PCA_No2010), function(var_x){
  p <- 
    ggplot(All_PCA_No2010) +
    aes_string(var_x)
  
  if(is.numeric(All_PCA_No2010[[var_x]])) {
    p <- p + geom_density()
    
  } 
  
})

plot_grid(plotlist = my_plots2)




#Check if 2010 is a bad start year
prin_comp <- prcomp(PCA11, scale. = T)
PCAval11 <- prin_comp$x %>% cbind(PCA11n)
PCAval10x <- predict(prin_comp, trainPCA2010)


PCAval11 <- predict(prin_comp, PCA11) %>% cbind(PCA11n)
PCAval12 <- predict(prin_comp, PCA12) %>% cbind(PCA12n)
PCAval13 <- predict(prin_comp, PCA13) %>% cbind(PCA13n)
PCAval14 <- predict(prin_comp, PCA14) %>% cbind(PCA14n)
PCAval15 <- predict(prin_comp, PCA15) %>% cbind(PCA15n)
PCAval16 <- predict(prin_comp, PCA16) %>% cbind(PCA16n)
PCAval90 <- predict(prin_comp, PCA90) %>% cbind(PCA90n)
PCAval00 <- predict(prin_comp, PCA00) %>% cbind(PCA00n)

ALL_PCA <- rbind(PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16) %>% reshape2::melt(id.vars = c("CensusTractID", "year")) 
ALL_PCA_wide <- rbind(PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16)


ALL_PC1 <- ALL_PCA %>% filter(variable == "PC1" & year != 2010)
ALL_PC2 <- ALL_PCA %>% filter(variable == "PC2" & year != 2010)
ALL_PC3 <- ALL_PCA %>% filter(variable == "PC3" & year != 2010)

ALL_PC1x <- ALL_PCA %>% filter(variable == "PC1")
ALL_PC2x <- ALL_PCA %>% filter(variable == "PC2")


tracts <- tracts("Tennessee", "Davidson County")
t <- st_as_sf(tracts)

tracts_sf_PC1 <- merge(t, ALL_PC1x, by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC2 <- merge(t, ALL_PC2, by.x = "GEOID", by.y = "CensusTractID")

ggplot(data = tracts_sf_PC1) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()

ggplot(data = tracts_sf_PC2) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()



my_plots <- lapply(names(PCA11), function(var_x){
  p <- 
    ggplot(ALL_PCA_wide) +
    aes_string(var_x)
  
  if(is.numeric(ALL_PCA_wide[[var_x]])) {
    p <- p + geom_density()
    
  } 
  
})

plot_grid(plotlist = my_plots)




#Jonathan I think

pc_test <- trainPCA2010 %>% mutate_all(funs((. - mean(.)) / sd(.))) %>% prcomp(scale = FALSE)

mx = prin_comp$x
mx = pc_test$x


trainPCA2010 %>% as_data_frame() %>% mutate_all(funs((.- mean(.)) / sd(.))) %>% as.matrix() %>% { . %*% pc_test$rotation } %>% { .[1:3,1:3] }

PCA11 %>% as_data_frame() %>% mutate_all(funs((.- mean(.)) / sd(.))) %>% as.matrix() %>% { . %*% pc_test$rotation } %>% { .[1:3,1:3] }

center = prin_comp$center %>% set_names(names(trainPCA2010))
scale = prin_comp$scale %>% set_names(names(trainPCA2010))

trainPCA2010 %>% { (. - center) * scale }
sdev = prin_comp$sdev %>% set_names(names(trainPCA2010))


trainPCA2010 %>% { (. - center) / scale } %>% as.matrix() %>% { . %*% pc_test$rotation } %>% { .[1:3,1:3] }

pc_test$x[1:3,1:3]
means = trainPCA2010 %>% summarize_all(mean)
sds = trainPCA2010 %>% summarize_all(sd)

round(scale) == round(sds)

trainPCA2010 %>% { (. - center) / scale } %>% as.matrix() %>% { . %*% pc_test$rotation } %>% { .[1:3,1:3] }
prin_comp$x[1:3,1:3]

pc_test$x[1:3,1:3]


center = prin_comp$center %>% set_names(names(trainPCA2010))
scale = prin_comp$scale %>% set_names(names(trainPCA2010))

trainPCA2010 %>% as.matrix() %>% { t((t(.) - center) / scale) } %>%  { . %*% pc_test$rotation } %>% { .[1:3,] } %>% round(4)

PCAval11 <- predict(prin_comp, PCA11)[5,]

trainPCA2010 <- All_Census %>% filter(year == 2010 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA11n<- All_Census %>% filter(year == 2011 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA12n<- All_Census %>% filter(year == 2012 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA13n<- All_Census %>% filter(year == 2013 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA14n<- All_Census %>% filter(year == 2014 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA15n<- All_Census %>% filter(year == 2015 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA16n<- All_Census %>% filter(year == 2016 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA90<- Geolytics90x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 1990)
PCA00<- Geolytics00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 2000)


PCAval11 <- predict(prin_comp, PCA11) %>% cbind(PCA11n)

PCA90<- Geolytics90x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID)
PCA00<- Geolytics00x %>% filter(complete.cases(.)) %>% dplyr::select(-CensusTractID)
trainPCA2010n <- All_Census %>% filter(year == 2010 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA11n<- All_Census %>% filter(year == 2011 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA12n<- All_Census %>% filter(year == 2012 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA13n<- All_Census %>% filter(year == 2013 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA14n<- All_Census %>% filter(year == 2014 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA15n<- All_Census %>% filter(year == 2015 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA16n<- All_Census %>% filter(year == 2016 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCA90n<- Geolytics90x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 1990)
PCA00n<- Geolytics00x %>% filter(complete.cases(.)) %>% dplyr::select(CensusTractID) %>% mutate(year = 2000)

PCAval11 <- predict(prin_comp, PCA11) %>% cbind(PCA11n)
PCAval12 <- predict(prin_comp, PCA12) %>% cbind(PCA12n)
PCAval13 <- predict(prin_comp, PCA13) %>% cbind(PCA13n)
PCAval14 <- predict(prin_comp, PCA14) %>% cbind(PCA14n)
PCAval15 <- predict(prin_comp, PCA15) %>% cbind(PCA15n)
PCAval16 <- predict(prin_comp, PCA16) %>% cbind(PCA16n)
PCAval90 <- predict(prin_comp, PCA90) %>% cbind(PCA90n)
PCAval00 <- predict(prin_comp, PCA00) %>% cbind(PCA00n)
PCAVal10 <- prin_comp$x

#PCA Named
PCA10n <- All_Census %>% filter(year == 2010 & complete.cases(.)) %>% dplyr::select(CensusTractID, year)
PCAVal10 <- prin_comp$x %>% cbind(PCA10n)

PCAval12 <- predict(prin_comp, PCA12) %>% cbind(PCA12n)



ALL_PCA <- rbind(PCAVal10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16, PCAval90, PCAval00) %>% reshape2::melt(id.vars = c("CensusTractID", "year"))


t <- st_as_sf(tracts)
str(t)

ALL_PCA <- rbind(PCAVal10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16, PCAval90, PCAval00) %>% reshape2::melt(id.vars = c("CensusTractID", "year")) %>% filter(variable == "PC1" | variable == "PC2" | variable == "PC3" | variable == "PC4")

tracts <- tracts("Tennessee", "Davidson County")
t <- st_as_sf(tracts)
tracts_sf <- merge(t, ALL_PCA, by.x = "GEOID", by.y = "CensusTractID")

ggplot(data = tracts_sf) + geom_sf(aes(fill = value, color = "green2red")) + facet_grid(year~variable)


ALL_PCA <- rbind(PCAVal10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16) %>% reshape2::melt(id.vars = c("CensusTractID", "year")) %>% filter(variable == "PC1" | variable == "PC2" | variable == "PC3" | variable == "PC4")


ALL_PCA <- rbind(PCAVal10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16) %>% reshape2::melt(id.vars = c("CensusTractID", "year")) %>% filter(variable == "PC1" | variable == "PC2" | variable == "PC3" | variable == "PC4" & year != 2010)



ALL_PCA <- rbind(PCAval10, PCAval11, PCAval12, PCAval13, PCAval14, PCAval15, PCAval16) %>% reshape2::melt(id.vars = c("CensusTractID", "year"))
ALL_PC1 <- ALL_PCA %>% filter(variable == "PC1" & year != 2010)
ALL_PC2 <- ALL_PCA %>% filter(variable == "PC2" & year != 2010)
ALL_PC3 <- ALL_PCA %>% filter(variable == "PC3" & year != 2010)

tracts_sf_PC1 <- merge(t, ALL_PC1, by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC2 <- merge(t, ALL_PC2, by.x = "GEOID", by.y = "CensusTractID")



ggplot(data = tracts_sf_PC2) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()
ALL_PC1 <- ALL_PCA %>% filter(variable == "PC1" & year != 2010)
ALL_PC2 <- ALL_PCA %>% filter(variable == "PC2" & year != 2010)
ALL_PC3 <- ALL_PCA %>% filter(variable == "PC3" & year != 2010)
tracts_sf_PC2 <- merge(t, ALL_PC2, by.x = "GEOID", by.y = "CensusTractID")
ggplot(data = tracts_sf_PC2) + geom_sf(aes(fill = value)) + facet_grid(variable~year) + scale_fill_gradientn(colours = rev(rainbow(7))) + theme_void()

##other meeting anomolies

bar <- prin_comp$rotation[,"PC1"]
simplify(bar) %>% abs %>% sort(decreasing = TRUE) %>% names()


AllPCAscaled <- rbind(mutate(as_tibble(scale(trainPCA2010)), year = 2010, CensusTractID = PCA10n$CensusTractID), mutate(as_tibble(PCA11), year = 2011, CensusTractID = PCA11n$CensusTractID), mutate(as_tibble(PCA12), year = 2012, CensusTractID = PCA12n$CensusTractID))



foo <- AllPCAscaled %>% mutate(year = as.integer(year)) %>% filter(year %in% 2010:2012) %>% dplyr::select(CensusTractID, year, rent, Income, PctRenters, Pct_Poverty, PctCollege) %>% left_join(ALL_PCA, by = c("CensusTractID", "year")) %>% filter(variable %in% c("PC1")) %>% spread(key = variable, value = value) %>% arrange(CensusTractID, year)


foo %>% mutate(x = Income * bar$Income + Pct_Poverty * bar$Pct_Poverty + rent * bar$rent)


x <- Joined_all_complete %>% filter(year < yoi) %>% gather(-CensusTractID, -year, key = variable, value = value) %>% dplyr::select(variable, value) %>% group_by(variable) %>% summarize(avg = mean(value), SD = sd(value))



anomalies <- Joined_all_complete %>% gather(-CensusTractID, -year, key = variable, value = value) %>% left_join(x, by = "variable") %>% mutate(anomaly = (value - avg) / SD)

anomaly_df <- anomalies %>% dplyr::select(-value, -avg, - SD) %>% spread(key = variable, value = anomaly)

anomaly_matrix <- anomaly_df %>% dplyr::select(-year, -CensusTractID) %>% as.matrix()

anomaly_pr <- prcomp(anomaly_matrix)
anomaly_pr2 <- prcomp(anomaly_matrix, center = FALSE, scale. = FALSE)
anomaly_pr$rotation
anomaly_pr$rotation[,1:5]
anomaly_pr2$rotation[,1:5]

anomaly_rot <- anomaly_pr$rotation
anomaly_pcs <- anomaly_matrix %*% anomaly_rot

anomaly_pc_df <- as_tibble(anomaly_pcs) %>% bind_cols(dplyr::select(anomaly_df, CensusTractID, year), .)

tracts_sf_PC1 <- merge(t, anomaly_pc_df, by.x = "GEOID", by.y = "CensusTractID")

tracts_sf_PC1 <- anomaly_pc_df %>% gather(-CensusTractID, -year, key = variable, value = value) %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")


anomaly_pc_df <- as_tibble(anomaly_pcs) %>% bind_cols(dplyr::select(anomaly_df, CensusTractID, year), .) %>% gather(-CensusTractID, -year, key = variable, value = value)

pc_2010_df <- anomaly_pc_df %>% filter(year == 2010)



delta_pc <- anomaly_pc_df %>% filter(year != 2010) %>% full_join(dplyr::select(pc_2010_df, -year), by = c("CensusTractID", "variable"), suffix = c("", ".2010")) %>% mutate(delta = (value - value.2010) / (year - 2010))

tracts_sf_PC1_delta <- delta_pc %>% filter(variable == "PC1") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")

ggplot(data = tracts_sf_PC1_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()

tracts_sf_PC2_delta <- delta_pc %>% filter(variable == "PC2") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")

ggplot(data = tracts_sf_PC2_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()

delta_pc <- anomaly_pc_df %>% arrange(CensusTractID, year) %>% group_by(CensusTractID) %>% mutate(delta = value - lag(value))

tracts_sf_PC2_delta <- delta_pc %>% filter(variable == "PC2") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")



delta_pc <- anomaly_pc_df %>% filter(year != 2010) %>% full_join(dplyr::select(pc_2010_df, -year), by = c("CensusTractID", "variable"), suffix = c("", ".2010")) %>% mutate(delta = (value - value.2010) / (year - 2010))

new_delta_pc <- anomaly_pc_df %>% arrange(CensusTractID, year) %>% group_by(CensusTractID) %>% mutate(delta = value - lag(value))

#Merge new delta pc and spatial tracts layer

tracts_sf_PC2_delta <- new_delta_pc %>% filter(variable == "PC2") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC1_delta <- new_delta_pc %>% filter(variable == "PC1") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC3_delta <- new_delta_pc %>% filter(variable == "PC3") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")
tracts_sf_PC4_delta <- new_delta_pc %>% filter(variable == "PC4") %>% merge(t, ., by.x = "GEOID", by.y = "CensusTractID")



#View rotation matrices for different PCs, arranged by loading
anomaly_pr$rotation[,1:5] %>% as_data_frame(rownames = "variable") %>% arrange(desc(PC1))
anomaly_pr$rotation[,1:5] %>% as_data_frame(rownames = "variable") %>% arrange(desc(PC2))
anomaly_pr$rotation[,1:5] %>% as_data_frame(rownames = "variable") %>% arrange(desc(PC3))
anomaly_pr$rotation[,1:5] %>% as_data_frame(rownames = "variable") %>% arrange(desc(PC4))
anomaly_pr$rotation[,1:5] %>% as_data_frame(rownames = "variable") %>% arrange(desc(PC5))



ggplot(data = tracts_sf_PC1_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()

ggplot(data = tracts_sf_PC2_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()

ggplot(data = tracts_sf_PC3_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()

ggplot(data = tracts_sf_PC4_delta) + geom_sf(aes(fill = delta)) + facet_grid(variable~year) + scale_fill_distiller(palette = "RdYlBu") + theme_void()











