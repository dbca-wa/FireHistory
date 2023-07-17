# FireHistory 0.2.0.0

This is a major release adding new features and fixing a number of bugs

### Bug fixes

* Error when trying to run `fire_freq` (#7)
* Error when trying to run `fire_interval` (#8)

### Major changes

* Cropping of raster data was previously done using the aoi vector, which could 
introduce inconsistencies in reporting areas. An aoi raster mask addresses this.
* The time periods are financial years (July-June) not calendar years (January-December). 
This is due to the fact that the fire season in the south west predominantly 
occurs during the summer months. Bear this in mind when analysing fires in the 
north.
* YSLB now contains an "unknown" category. This category is applied to any area 
in the aoi where there is no burn history for the time period chosen. It is 
reported on in exported stats and is represented in the map as a grey region 
bordered by a red boundary.
* Better axis labeling for column charts.

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
