###PREDICTIONS
library(pacman)
p_load(tidyverse, lubridate, ggplot2, pROC,
       tigris, tmap, corrplot, mlbench, caret, Boruta, verification, randomForest, e1071)

yoi <- 2010

All_Census <- readRDS("All_census.rds")
All_vars_10_16 <- readRDS("AllVars10to16.rds")
Dependant <- readRDS("Dependant.rds")

Dep <- read_rds("DEPEND.rds") %>% inner_join(., Joined_all_complete, by = c("GEOID" = "CensusTractID")) %>% filter(year == yoi) %>% dplyr::select(GEOID, CHNG_White, CHNG_Income, CHNG_HomeVal, CHNG_college, GentClass)

#correlation matrices
res <- cor(as.matrix(All_Census))
round(res, 2)
mydata <- Joined_all_complete[, c(3:18)]
rquery.cormat(mydata)

#Boruta on start year
x2010_data <- All_vars_10_16 %>% filter(year == 2010) %>% dplyr::select(-year)
dep <- Dependant %>% dplyr::select(CensusTractID, Gent00_16)

all_2010 <- inner_join(x2010_data, Dep, by= c('CensusTractID'='GEOID'))
traindata <- all_2010 [complete.cases(all_2010),]

traindata$GentClass <- as.factor(FULL_SET$GentClass)
traindata$BooleanGent <- ifelse(FULL_SET$GentClass == "Gentrified", 1, 0)

traindata$Gent00_16 <- as.factor(traindata$Gent00_16)

set.seed(123)
boruta.train <- Boruta(Gent00_16~.-CensusTractID, data = traindata, doTrace = 2)
print(boruta.train)

plot(boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i) boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

boruta.df <- attStats(final.boruta)
print(boruta.df)


#####Combine Independant and Dependant in wide Table for Logistic Regression
#Extra columns are ok (formula is specified in function)
Ind <- All_vars_10_16 %>% dplyr::select(-PctNonWhite, -PctCollege, -Income, -homeValue) 
Ind <- All_vars_10_16

FULL_SET <- inner_join(x = Dep, y = Ind, by = c("GEOID" = "CensusTractID")) %>% ungroup() %>% filter(year==2010) 

FULL_SET$GentClass <- as.factor(FULL_SET$GentClass)
FULL_SET$BooleanGent <- ifelse(FULL_SET$GentClass == "Gentrified", 1, 0)
FULL_SET$BooleanGent <- as.factor(FULL_SET$BooleanGent)

###logistic regression
fitall <- glm(data = FULL_SET, BooleanGent ~ MedRent + Pct_MultiUnit + NonFamPct + Pct_Renters + Pct_Vacant + EvictionRate + UnemploymentPct + PovertyPct + Under18 + Over65x + PermitVal + Demos + Complaints + Dist2Downtown + RentBurdenPct + PctPubCommute + MedianAgeBuilt + PctBlueCollar + MedianPopAge + NoCarPct + PctCommuteless20min + Park_PctCoverage10 + Transit_PctCoverage + Pct3Vehicles)

summary(fitall)

glm_probs <- predict(fitall, type = "response")
glm_pred <- ifelse(glm_probs < 0.3, 0, 1)
attach(FULL_SET)
table(glm_pred, GentClass)


df <- data.frame(glm_probs, glm_pred)
df$glm_pred <- as.factor(df$glm_pred)

roc.plot(FULL_SET$BooleanGent, df$glm_pred)
SDMTools::optim.thresh(FULL_SET$BooleanGent, glm_probs)

SDMTools::confusion.matrix(FULL_SET$BooleanGent, glm.probs)

confusionMatrix(data = df$glm_pred, FULL_SET$BooleanGent)

###########
#More Prediction Methods

#linear regression on index 
Index <- readRDS("Indexcalcs.rds")
Index_slt <- Index %>% dplyr::select(GEOID, sum2000)


index_df <- merge(x = Geolytics_all, y = Index_slt, by.x = "CensusTractID", by.y = "GEOID")
index_df2 <- left_join(index_df, AllTimeVars)
index_datax <- index_df2 [complete.cases(index_df2),]
index_data<- index_datax[,-1]


#Linear Regression
linearMod <- lm(sum2000 ~ ., data=index_data)  # build linear regression model on full data
print(linearMod)
summary(linearMod)

#Random Forest
set.seed(103)
train=sample(1:nrow(index_data),70)
gent_rf=randomForest(sum2000 ~ . , data = index_data , subset = train)
gent_rf

cf <- ctree(sum2000~., data = index_data)
plot(cf, type= "simple")

#SVM
train_svm <- index_data[1:70,]
test_svm <- index_data[71:156,]
gent_svm <- svm(as.formula(paste("sum2000", "~ .", sep = " ")), data = index_data)
pred <- predict(gent_svm, test_svm)


write.csv(index_data, "INDEX_DATA_GWR.csv")


####################
#Spatial Regression

p_load(spdep, rgdal, rgeos, INLA)
myshp <- readOGR("C:\\School\\Research\\census\\Davidson_Tracts_utm.shp")
myshp@data$JOINID = as.character(myshp@data$GEOID)
sp_jn1 <- geo_join(spatial_data = myshp, data_frame = index_datax, by_sp = "JOINID", by_df = "CensusTractID")

spplot(sp_jn1, "sum2000")
queen.nb = poly2nb(sp_jn1, queen = FALSE)
queen.listw = nb2listw(queen.nb)
listw1 <- queen.listw

reg.eq1 <- HomeVal ~ RentVal00 +  HH_income00 + PctCollege00 + Pct_Renter00 + PctMulti00 + pctVacant00 + pctNonfamily00 

reg1 = lm(reg.eq1, data = sp_jn1)
lm.morantest(reg1, listw1)
lm.LMtests(reg1, listw1, test="all")

reg2 = lmSLX(reg.eq1, data = sp_jn1, listw1)

reg3 = lagsarlm(reg.eq1, data = sp_jn1, listw1)
reg4 = errorsarlm(reg.eq1, data = sp_jn1, listw1)
reg5 = errorsarlm(reg.eq1, data = sp_jn1, listw1, etype = "emixed")
reg6 = lagsarlm(reg.eq1, data = sp_jn1, listw1, etype = "mixed")
reg7 = sacsarlm(reg.eq1, data = sp_jn1, listw1, type = "sacmixed")

summary(reg1)
summary(reg2)
summary(reg3)
summary(reg4)
summary(reg5)
summary(reg6)

Hausman.test(reg4)



###########
#Inla setup

Davidson_adj <- poly2nb(sp_jn1, queen = FALSE)
adj <- nb2mat(Davidson_adj, style = "B")
adj <- as(adj, "dgTMatrix")

sp_jn1$idx <- 1:nrow(sp_jn1)
form <- sum2000 ~ HomeVal00 + RentVal00 +  HH_income00 + PctCollege00 + Pct_Renter00 + PctMulti00 + pctVacant00 + pctNonfamily00 + f(idx, model = "besag", graph = adj)
btdf <- as.data.frame(sp_jn1)
m1 <- inla(form, data = btdf, control.predictor = list(compute = TRUE))


############
####Spatial lag on census and permits
Indall_x <- inner_join(x = Joined_all_complete, y = Dep, by = c("CensusTractID"= "GEOID")) %>% ungroup() %>% filter(year == 2010 | year == 2011) %>% dplyr::select(-year)


p_load(spdep, rgdal, rgeos, INLA)
myshp <- readOGR("C:\\School\\Research\\census\\Davidson_Tracts_utm.shp")
myshp@data$JOINID = as.character(myshp@data$GEOID)
sp_jn1 <- geo_join(spatial_data = myshp, data_frame = Indall_x, by_sp = "JOINID", by_df = "CensusTractID")

spplot(sp_jn1, "CHNG_HomeVal")
queen.nb = poly2nb(sp_jn1, queen = FALSE)
queen.listw = nb2listw(queen.nb)
listw1 <- queen.listw

reg.eq1 <- CHNG_HomeVal ~ rent + Pct_MultiUnit + PctNonFam + PctRenters + PctVacant + Evictions + Pct_Unemployed + Pct_Poverty + Under18 + Over65x + PermitVal + Demos + Complaints + Distance + HomeAge + BelowAvgQuality

reg1 = lm(reg.eq1, data = sp_jn1)
lm.morantest(reg1, listw1)
lm.LMtests(reg1, listw1, test="all")

reg2 = lmSLX(reg.eq1, data = sp_jn1, listw1)
impacts(reg2, listw = listw1)
summary(impacts(reg2, listw = listw1, R = 500, zstats=TRUE))


reg3 = lagsarlm(reg.eq1, data = sp_jn1, listw1)
reg4 = errorsarlm(reg.eq1, data = sp_jn1, listw1)
reg5 = errorsarlm(reg.eq1, data = sp_jn1, listw1, etype = "emixed")
reg6 = lagsarlm(reg.eq1, data = sp_jn1, listw1, etype = "mixed")
reg7 = sacsarlm(reg.eq1, data = sp_jn1, listw1, type = "sacmixed")