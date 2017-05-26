METsteps Tutorial
Samuel Saxe

April 28, 2017

Introduction
The METsteps package demonstrated in this documented was created as a way to streamline and simplify the process of taking time-series water balance products and converting them to a standardized format. To date, three formats are available for production:

HUC-level aggregation, in which gridded data is mean-aggregated to the HUC 2 - 10 levels. This allows for easy translation between resolutions and simplifies statistical analysis. Datasets are transformed to the NAD83 projection for processing.

HRU table values typically produced through PRMS.

Grid-level, in which two gridded datasets are formatted to a standardized resolution and projection. The highest resolution data determines the output dataset resolution and all datasets are transformed to the NAD83 projection.

The standardized products of the following workflow are immediately ready for interactive exploration and analysis in the associated rShiny application.

This workflow is significantly impacted by the power of the user’s machine, so the greater the number of cores and total RAM, the greater the speed increases. This has only been tested on Windows 10.

Step 1: Installing package
Since you’re probably installing from within a USGS system and because the package is hosted on Github, you can’t use the usual devtools::install_github(‘ssaxe-usgs/METsteps’ ) function. Instead, you will have to download the package at https://github.com/ssaxe-usgs/METsteps, click on the green ‘Clone or download’ dropdown button, and click download ZIP. Then, install the package using:

install.packages('C:/User/Downloads/METsteps.zip')
Don’t forget to use forward slashes or double backslashes in your directory path.

Step 2: (Beginner) Loading packages
You can skip this if you are familiar with loading packages into the R environment.

Packages are loading into the global R environment using either the library() or the require() function. The fundamental difference between these two packages is that library() returns an error when a package is not available to load. require() returns a warning. In R, errors STOP a code. Warnings only PRINT a message to the console. Because of this, it is suggested to use library() when loading packages at the beginning of scripts. Here’s an example loading in the METsteps package:

library('METsteps')
One more difference between the library() and require() functions is that require() is significantly faster. Normally this doesn’t matter but when loading a package within a function that will be iterated thousands or millions of times, shaving off microseconds is important. To wit:

library('microbenchmark')
library('ggplot2')
st <- microbenchmark::microbenchmark('require(x)' = require(ggplot2),
                                     'library(x)' = library(ggplot2))
ggplot2::autoplot(st)


One more quick note: The ‘::’ syntax directs R to load a function from a specified package. I prefer using this when writing up scripts I’ll be using often, as functions from different packages can have the same name and the ‘::’ syntax reduces errors.

Step 3: Creating a parallel cluster object
Now that you’ve imported METsteps, you’ll need one more package: parallel. This allows your computer to process datasets in parallels across multiple cores, yielding significant speed increases.

library('parallel')
We will want to initiate what’s called a cluster, that directs R to create environments on multiple nodes. To see how many cores your computer has, use:

parallel::detectCores()
## [1] 32
As you can see, my computer has 32 cores. Typically, you will want to leave at least one core untouched so your computer doesn’t get bogged down and you can still work while processing data. Since I have so many cores, I am limited by RAM (32 gb) and disk transfer speed and will only use 20 cores. For computers with less cores, I would suggest using:

no_cores <- parallel::detectCores() - 1
print(no_cores)
## [1] 31
# I'll just use 20 though
no_cores <- 20
Now we will initialize a cluster and assign it the name ‘cl’:

cl <- parallel::makeCluster(no_cores)
print(cl)
To close a cluster (later), use:

parallel::stopCluster(cl)
Step 4: Aggregating to HUCs
In this section, we will aggregate a gridded dataset to the HUC 2-10 levels using three functions from the METsteps package. The first function we will use is aggregateRasterToPolygons(). This function can serve two similar purposes:

First, to aggregate gridded data by mean to the HUC-10 level by using the argument ‘MET.HUC10 = TRUE’. This tells the function to use the included HUC-10 shapefile as the extraction polygons.

Or, the user can provide a file path and other information to use their own shapefile. This won’t be compatible with the other functions in the package, but if you just want a simple way to get mean values it works fine.

NOTE: To look at the help file for a function, use:

?aggregateRasterToPolygons
Now, let’s give the function the required information. For use with the METsteps HUC-10 dataset, this is what you’ll need:

x <- METsteps::aggregateRasterToPolygons(dataPath        = 'C:/Path/To/Folder/',
                                         dataName        = 'SSEBop',
                                         dataExtension   = 'tif',
                                         dataCategory    = 'AET',
                                         startDate       = '2000-01-01',
                                         timeStep        = 'month',
                                         cancelReproject = TRUE,
                                         cl              = cl)
‘dataPath’ is the directory to the containing folder of your gridded datasets. ‘dataCategory’, ‘startDate’, and ‘timeStep’ are required metadata to pass along to further functions and will ultimately be used in a Shiny app. ‘cl’ is the cluster object you created earlier. If you choose not to operate the function in parallels, simply leave out the ‘cl’ argument or set it equal to NULL (without quotation marks).

Now we’ll talk about the ‘cancelReproject’ argument for a minute. This defaults to FALSE, which means the function will transform the projection of your gridded dataset if it’s projection is not NAD83 (which is set as default in the ‘projManual’ argument). When a dataset does not need to be reprojected, processing is MUCH faster and generally error-free.

Many datasets will come in a WGS84 projection. While this is different from NAD83, most of these formats will actually be almost exactly the same as NAD83. I highly suggest plotting a single one of your raster images first, then plotting a HUC shapefile over it to see if the regions match up correctly over your dataset:

library('raster')

# Load a single raster file
x <- raster::raster('C:/Users/Documents/Path/To/Your/Data.tif')

# Check projection
raster::projection(x)

# If projection is WGS84, plot
plot(x)
plot(METsteps::polyHUC2, add = T)
If the plot looks good, just set ‘cancelReproject’ equal to TRUE like so:

x <- METsteps::aggregateRasterToPolygons(dataPath        = 'C:/Path/To/Folder/',
                                         dataName        = 'SSEBop',
                                         dataExtension   = 'tif',
                                         dataCategory    = 'AET',
                                         startDate       = '2000-01-01',
                                         timeStep        = 'month',
                                         cancelReproject = TRUE,
                                         cl              = cl)
If your dataset is in WGS84 and doesn’t plot correctly in the above example, then you might be better off just processing it in ESRI’s ArcMap or QGIS. Sometimes GDAL (the C library used by R for geospatial datasets) can’t convert from WGS84 to NAD83 correctly. Also, ArcMap is just faster most of the time for this. If it’s some other, non-sinusoidal projection just go ahead and leave ‘cancelReproject’ as FALSE and let R take care of it. Sinusoidal projections also need to be converted in ArcMap or QGIS (convert to GCS_NAD83).

Once you’ve processed through the aggregateRasterToPolygons() function, downscale that HUC-10 dataset to HUCS 2-8 by running:

y <- METsteps::downscaleHUC10(x)
And finally to write results out (in ‘feather’ format), simply provide a path to a folder you have, or would like to create, that will contain your data to be accessed by the Shiny app:

METsteps::zooHUCtoFeather(zoo.obj    = y,
                          Shiny.path = 'C:/Path/To/Shiny/Folder')
And you’re done and ready to explore the results in the Shiny app.

Here is a complete workflow from start to finish that includes a simple timing function:

library('METsteps')
library('parallel')
library('raster')

# Start timing (Just for the purposes of this writeup)
METsteps::tic()

# Initialize cluster
cl <- parallel::makeCluster(20)

# If WGS84, check plotting (OPTIONAL)
x <- raster::raster('C:/Users/Documents/Path/To/Your/Data.tif')
raster::projection(x)
plot(x)
plot(METsteps::polyHUC2, add = T)

# Aggregate to HUC-10
x <- METsteps::aggregateRasterToPolygons(dataPath        = 'C:/Path/To/Folder/',
                                         dataName        = 'SSEBop',
                                         dataExtension   = 'tif',
                                         dataCategory    = 'AET',
                                         startDate       = '2000-01-01',
                                         timeStep        = 'month',
                                         cancelReproject = TRUE,
                                         cl              = cl)

# Downscale to HUCs 2-8
y <- METsteps::downscaleHUC10(x)

# Save to folder to access via Shiny app
METsteps::zooHUCtoFeather(zoo.obj    = y,
                          Shiny.path = 'C:/.../folder/')

# Close cluster
parallel::stopCluster(cl)

# Report run time
METsteps::toc()

##  Elapsed Time: 2.32 minutes
Step 5: Exploring in Shiny
To interactively explore your datasets via Shiny, provide the same directory path you used for your zooHUCtoFeather() function and run:

METsteps::exploreShiny('C:/.../folder/')
Good luck and hope it doesn’t bug out on you too much. It probably will…
