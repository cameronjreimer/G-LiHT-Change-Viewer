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

# crop merged tiles to LFDP extent and write out
pre_LFDP <- terra::crop(pre_tiles, lfdp, mask = T, touches = F,
                        filename = './data/imagery/cropped/LFDP_2017.tif', 
                        datatype = 'INT1U', overwrite = T)
post_LFDP <- terra::crop(post_tiles, lfdp, mask = T, touches = F,
                         filename = './data/imagery/cropped/LFDP_2018.tif',
                         datatype = 'INT1U', overwrite = T)

# plot for fun
plotRGB(pre_LFDP)
plotRGB(post_LFDP)

