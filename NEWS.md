# FireHistory 0.1.0.0

* Package has now migrated to DBCA Organisational GitHub (https://github.com/dbca-wa/FireHistory)
* In `yslb`, rasterising the fire history polygons was not ordering by year. Now fixed.
* Interval metrics calculated using `fire_interval` will return an area plot if products are requested.
* Added unit testing.
* Output folder only created if `products = TRUE` in metric functions. 

# FireHistory 0.0.2.0

* Package now includes`fire_interval`, a package to calculate a variety of fire interval 
measures, including minimum, maximum and mean intervals.
* Raster products written to file now can be added to GIS software and the crs 
system will be recognised without further work. 
* Addressed issue where `fire_freq` was not cropping and masking raster output to aoi.

# FireHistory 0.0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Package now includes `find_block()` function to allow search for forest block names.
* Function `DBCA_aoi()` now can return a spatial boundary for a queried forest block.
* Package now uses `magrittr::%>%` pipe instead of base R `|>` to support older R installations.
