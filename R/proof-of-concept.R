
library(terra)

# load qudrats
q40 <- vect('./data/shapefiles/LFDP/LFDP_window_40.shp')

# load high-res imagery 
pre <- rast('./data/imagery/cropped/LFDP_2017.tif')
post <- rast('./data/imagery/cropped/LFDP_2018.tif')

# load CHMs 
pre_chm <- rast('./data/imagery/cropped/LFDP_CHM_2017_1m.tiff')
post_chm <- rast('./data/imagery/cropped/LFDP_CHM_2018_1m.tiff')
abs_loss <- rast('./data/imagery/cropped/LFDP_CHM_2017_2018_Absolute_Height_Loss.tif')
rel_loss <- rast('./data/imagery/cropped/LFDP_CHM_2017_2018_Relative_Height_Loss.tif')

# plotting
tile <- 1
tile_window <- q40[tile]

par(mfrow = c(2,2))
plotRGB(crop(pre, tile_window))
plotRGB(crop(post, tile_window))
plot(crop(abs_loss*-1, tile_window), axes = F)
# plot(crop(rel_loss, tile_window), axes = F)
# plot(crop(pre_chm, tile_window), axes = F, col = map.pal("grey", 100))
plot(crop(post_chm, tile_window), axes = F, col = map.pal("grey", 100))

