
rm(list = ls())

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('ade4')) install.packages("ade4")
if (!require('factoextra')) install.packages("factoextra")
if (!require('NbClust')) install.packages("NbClust")
if (!require('randomForest')) install.packages("randomForest")
if (!require('ggplot2')) install.packages("ggplot2")
if (!require('ggspatial')) install.packages("ggspatial")
if (!require('tidyterra')) install.packages("tidyterra")

library(openxlsx)
library(ade4)
library(factoextra)
library(NbClust)
library(randomForest)
library(ggplot2)
library(ggspatial)
library(tidyterra)


# Top-down approach-------------------------------------------------------------

stacked = c(elevation, sand, tavg, prec, pop, travel_time)

stacked_original = stacked

# terra::boxplot(stacked$elevation)
# terra::boxplot(stacked$sand)
# terra::boxplot(stacked$tavg)
# terra::boxplot(stacked$prec)
# terra::boxplot(stacked$pop)
# terra::boxplot(stacked$travel_time)

# hist(stacked$elevation)
# hist(stacked$sand)
# hist(stacked$tavg)
# hist(stacked$prec)
# hist(stacked$pop)
stacked$pop = log10(stacked$pop + (0.5 * global(stacked$pop, min, na.rm = T)$min))
# hist(stacked$pop)
# hist(stacked$travel_time)
stacked$travel_time <- log10(stacked$travel_time + abs(global(stacked$travel_time, min, na.rm = TRUE)$min) + 1e-6)
# hist(stacked$travel_time)

stacked = scale(stacked)

set.seed(123)
sr = terra::spatSample(stacked, min(10000, ncell(stacked)), method = "random", na.rm = F, as.raster = F, as.df = T, xy = T)
sr = na.omit(sr)

pca = dudi.pca(sr[, -c(1:2)], scannf = F, nf = 2)
(eig.val = data.frame(eigenvalue = c(pca$eig)))

(loading = pca$co)

fviz_pca_var(pca, col.var="contrib") +
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 17.5) +
  theme_minimal()

fviz_pca_ind(pca)

outliers = subset(pca$li, Axis1 < mean(pca$li$Axis1) - 1.5 * IQR(pca$li$Axis1) | Axis1 > mean(pca$li$Axis1) + 1.5 * IQR(pca$li$Axis1) | Axis2 < mean(pca$li$Axis2) - 1.5 * IQR(pca$li$Axis2) | Axis2 > mean(pca$li$Axis2) + 1.5 * IQR(pca$li$Axis2))

sr$new = row.names(sr)
'%!in%' = function(x,y)!('%in%'(x,y))

sr = subset(sr, new %!in% row.names(outliers))

pca = dudi.pca(sr[, -c(1:2, 9)], scannf = F, nf = 2)
(eig.val = data.frame(eigenvalue = c(pca$eig)))

fviz_pca_var(pca, col.var = "contrib") +
  scale_color_gradient2(low = "blue", mid = "purple", high = "red", midpoint = 17) +
  theme_minimal()

fviz_pca_ind(pca)

pc_rast = terra::predict(stacked, pca)
names(pc_rast) = c('pc1', 'pc2')

plot(pc_rast)

pc_scores = pca$li

hclust_r = hclust(dist(pc_scores), method = "ward.D2")
plot(hclust_r, hang = -1, labels = FALSE)

# NbClust(pc_scores, diss = dist(pc_scores), distance = NULL, method = "complete")
k = 5
clusters = cutree(hclust_r, k = k)

train = data.frame(pc1 = pc_scores[, 1], pc2 = pc_scores[, 2],
                   cluster = as.factor(clusters))

rf_model = randomForest(cluster ~ pc1 + pc2, data = train)

cluster_rast = terra::predict(pc_rast, rf_model)

plot(zwe0)
plot(cluster_rast, add = T)
plot(zwe1, add = T)

all = c(cluster_rast, stacked_original)
all_df = terra::as.data.frame(all)
all_df = na.omit(all_df)
rf = randomForest(as.factor(class) ~ ., all_df)
varImpPlot(rf)

cluster_cols = c("1" = "#440154", "2" = "#3b528b", "3" = "#21918c", "4" = "#5ec962", "5" = "#fde725")

all_df$class = factor(all_df$class, levels = c(1, 2, 3, 4, 5))

par(mfrow = c(2,3), xaxs='i', yaxs='i')

boxplot(all_df$elevation ~ all_df$class,
        ylab = 'Elevation (masl)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$sand ~ all_df$class,
        ylab = 'Sand (%)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$tavg ~ all_df$class,
        ylab = 'Average temperature (°C)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$prec ~ all_df$class,
        ylab = 'Annual precipitation (mm)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$pop ~ all_df$class,
        ylab = 'Population (inhabitants)', xlab = '',
        col = cluster_cols[levels(all_df$class)])
boxplot(all_df$travel_time ~ all_df$class,
        ylab = 'Travel time to city (hrs)', xlab = '',
        col = cluster_cols[levels(all_df$class)])

cluster_rast_df = as.data.frame(cluster_rast, xy = TRUE)

cluster_rast_df$class = factor(
  cluster_rast_df$class,
  levels = c(1, 2, 3, 4, 5))

ggplot() + theme_bw() +
  geom_raster(data = na.omit(cluster_rast_df), aes(x = x, y = y, fill = class)) +
  geom_spatvector(data = zwe1, fill = NA, linewidth = 1, color = "black") +
  scale_fill_viridis_d(option = "inferno") +
  labs(fill = "Domains") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = ggplot2::margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"), style = north_arrow_fancy_orienteering(text_size = 10))

area_tbl = zonal(cellSize(cluster_rast, unit = "km"), cluster_rast, "sum", na.rm = TRUE)
colnames(area_tbl) = c("Domain", "Area_km2")

pop_tbl = zonal(pop, cluster_rast, fun = "sum", na.rm = TRUE)
colnames(pop_tbl) = c("Domain", "Population")

result = merge(area_tbl, pop_tbl, by = "Domain")

result


# Bottom-up approach------------------------------------------------------------

data = read.xlsx("dataset_mech_zimbabwe.xlsx")

nrow(data)

data_spatial = vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")

cols = c(no = "grey70", yes = "forestgreen")

plot(zwe0)
plot(data_spatial, col = cols[as.character(data_spatial$Tractor)], pch = 4, add = T)
plot(zwe1, add = T)

par(xpd = NA)
legend("topright", legend = names(cols),
       col = cols, pch = 4, bty = "n")

data = cbind(data, terra::extract(stacked_original, data_spatial))

data$Tractor = ifelse(data$Tractor == "yes", 1, 0)

rf_tract = randomForest::randomForest(Tractor ~ elevation + sand + tavg + prec + pop + travel_time, data = na.omit(data[, c("Tractor", "elevation", "sand", "tavg", "prec", "pop", "travel_time")]))

rf_tract_pred = predict(stacked_original, rf_tract, na.rm = T)

plot(zwe0)
plot(rf_tract_pred, add = T)
plot(zwe1, add = T)

varImpPlot(rf_tract)

partialPlot(rf_tract, na.omit(data[, c("Tractor", "elevation", "sand", "tavg", "prec", "pop", "travel_time")]), x.var = "prec")

rf_tract_pred_df = as.data.frame(rf_tract_pred, xy = TRUE)

ggplot() + theme_bw() +
  geom_raster(data = na.omit(rf_tract_pred_df), aes(x = x, y = y, fill = lyr1)) +
  geom_spatvector(data = zwe1, fill = NA, linewidth = 1, color = "black") +
  scale_fill_viridis_c(option = "inferno") +
  labs(fill = "Probability") +
  theme(plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 12),
        axis.text.x = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right",
        plot.margin = ggplot2::margin(10, 10, 10, 10)) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.25, "cm"), text_cex = 1, pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm")) +
  annotation_north_arrow(location = "tl", which_north = "true", height = unit(1, "cm"), width = unit(1, "cm"), pad_x = unit(0.15, "cm"), pad_y = unit(0.15, "cm"), style = north_arrow_fancy_orienteering(text_size = 10))

