
data = read.xlsx("dataset_mbire.xlsx")


# Calculation of transferability potential--------------------------------------

all = c(elevation, sand, tavg, prec, pop, travel_time)

means = as.numeric(global(all, "mean", na.rm = TRUE)[,1])
sds = as.numeric(global(all, "sd", na.rm = TRUE)[,1])
all_z = (all - means) / sds
names(all_z) = paste0(names(all), "_n")

data_vect = vect(data, geom = c("longitude", "latitude"), crs = "EPSG:4326")

vals = terra::extract(all_z, data_vect)

mean_site = colMeans(vals[, -1], na.rm = TRUE)

mean_site_df = data.frame(
  variable = names(all_z),
  mean_value = mean_site)

mean_site_df

dist_to_site = sqrt(sum((all_z - mean_site)^2))

centroid = colMeans(crds(data_vect))

plot(dist_to_site, main = "Euclidean distance to project archetype")
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)

tp_site = 1 / (1 + dist_to_site)
names(tp_site) = "tp"

plot(tp_site, main = "Transferability potential")
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)


## Interpretation---------------------------------------------------------------

tp_site_q = classify(tp_site$tp, c(
  quantile(values(tp_site$tp, na.rm=T))[1],
  quantile(values(tp_site$tp, na.rm=T))[2],
  quantile(values(tp_site$tp, na.rm=T))[3],
  quantile(values(tp_site$tp, na.rm=T))[4],
  quantile(values(tp_site$tp, na.rm=T))[5]))

plot(tp_site_q, type = 'classes', levels = c('Low', 'Moderate', 'High', 'Very high'), main ='Transferability potential')
plot(zwe1, add = T)
points(centroid["x"], centroid["y"], pch = 21, bg = 'orangered', cex = 2)

tp_site_q_df = as.data.frame(tp_site_q, xy = TRUE)

names(tp_site_q_df)[3] = "class_id"

tp_site_q_df$class_id_num = as.numeric(tp_site_q_df$class_id)

tp_site_q_df$class_label = factor(
  tp_site_q_df$class_id_num,
  levels = 1:4,
  labels = c("Low", "Moderate", "High", "Very high"),
  ordered = TRUE)

centroid_df = data.frame(x = centroid["x"], y = centroid["y"])

ggplot() + theme_bw() +
  geom_raster(data = na.omit(tp_site_q_df), aes(x = x, y = y, fill = class_label)) +
  geom_spatvector(data = zwe1, fill = NA, linewidth = 1, color = "black") +
  geom_point(data = centroid_df, aes(x = x, y = y), color = "black", fill = "tomato", shape = 21, size = 5, stroke = 2) +
  scale_fill_manual(values = c("Low" = "#420A68FF", "Moderate" = "#932667FF", "High" = "#DD513AFF", "Very high" = "#FCA50AFF")) +
  labs(fill = "Transferability") +
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

area_tbl = zonal(cellSize(tp_site_q, unit = "km"), tp_site_q, "sum", na.rm = TRUE)
colnames(area_tbl) = c("Class", "Area_km2")

pop_tbl = zonal(pop, tp_site_q, fun = "sum", na.rm = TRUE)
colnames(pop_tbl) = c("Class", "Population")

result = merge(area_tbl, pop_tbl, by = "Class")

result$Class = c("Low", "Moderate", "High", "Very high")

result


