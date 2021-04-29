# tbme
TBC


## Notes on running the analysis

### R package requirements

The following packages are required and can be installed from CRAN:

(analysis & utilities)
- library(here)
- library(data.table)
- library(metafor)
- library(rms)

(plotting)
- library(ggplot2)
- library(ggpubr)
- library(ggthemes)
- library(scales)

(mapping)
- library(sf)
- library(tmap)
- library(maptools)
- library(rgeos)



### Directory structure

Most analysis files are at the top level. However, metaanalysis/ contains the data and analysis for the meta-analysis, and returns the meta-analysis figure and results at this level (as well as the cleaned input count data).

To run without modification, two additional directories must be created:

1. metaanalysis/metaplots/
2. outdata/
3. graphs/

### Order

The analyses must be run in the following order:

1. metaanalysis/metaanalysis.R (this cleans the input data and runs the meta-analysis)
2. Estimates.R (this sources functions in the utilities.R, file which is not a separate analysis, and generates the main estimates results)
3. ProcessingResults.R ()
4. Plots.R (makes remaining plots)


# TODO

- decide how to publish indata/ & describe above
- consider what in gitignore may need to be unignored
- check what indata/BB.Rdata is

- SA bug
- clean output at country level
- look at change in overall incidence under new data
- table on counts data
