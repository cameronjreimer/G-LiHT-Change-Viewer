#
# Description:
#    Merge and crop high-res imagery tiles to LFDP extent
#
# Author(s):
#    Cam Reimer (cr657@cornell.edu)
#
#  Last Edited:
#     2026-09-29
#
### ----------------------------------------------------------------------------

library(terra)

# LFDP extent
lfdp <- vect('./data/shapefiles/LFDP/LFDP.shp')

# load image tiles into VRT
pre_tiles <- list.files(path = './data/imagery/tiles/PR_5March2017_EV1/',
                        pattern = '*.tif$',
                        full.names = T) |> vrt()
post_tiles <- list.files(path = './data/imagery/tiles/PR_25April2018_EV1/',
                         pattern = '*.tif$',
                         full.names = T) |> vrt()

# write options for the cropped imagery:
#  - keep RGB only (band 4 is an alpha band that is constant inside the plot)
#  - JPEG compression (~9x smaller than LZW, no visible loss at quality 90)
#  - tiled, so cropping to a single quadrat only reads nearby blocks
#  - no NoData value; INT1U would otherwise default to 255, turning real
#    white pixels into NA. For the same reason, crop to the LFDP bounding box
#    without masking.
rgb_opts <- c('COMPRESS=JPEG', 'PHOTOMETRIC=YCBCR', 'JPEG_QUALITY=90', 'TILED=YES')

# crop merged tiles to LFDP extent and write out
pre_LFDP <- terra::crop(pre_tiles[[1:3]], lfdp, mask = F,
                        filename = './data/imagery/cropped/LFDP_2017.tif',
                        datatype = 'INT1U', NAflag = NA, gdal = rgb_opts,
                        overwrite = T)
post_LFDP <- terra::crop(post_tiles[[1:3]], lfdp, mask = F,
                         filename = './data/imagery/cropped/LFDP_2018.tif',
                         datatype = 'INT1U', NAflag = NA, gdal = rgb_opts,
                         overwrite = T)

# plot for fun
plotRGB(pre_LFDP)
plotRGB(post_LFDP)

