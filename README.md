# tbme

The global burden of tuberculous meningitis in adults: a modelling study

Peter J Dodd,
Muhammad Osman,
Fiona V Cresswell,
Anna M Stadelman,
Nguyen Huu Lan,
Nguyen Thuy Thuong Thuong,
Morris Muzyamba,
Lisa Glaser,
Sicelo S Dlamini,
James A Seddon


## License

This work is licensed under a Creative Commons Attribution 4.0 International License

http://creativecommons.org/licenses/by/4.0/

![http://creativecommons.org/licenses/by/4.0/](https://i.creativecommons.org/l/by/4.0/88x31.png)

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

*NOTE* a condition of release of US data was that cells with count under 5 were labelled under 5. Therefore the raw US data is not included in this repository & the clean output data has relevant cells labelled '<5'. Other aspects of the metaanalysis should be reproducible, and the estimation process above the metaanalysis level also has no restrictions. Modifying these cells to a value such as 2 should allow approximate reproduction of the meta-analysis. Onward modelling should be fully reproducible.

The analyses must be run in the following order:

1. metaanalysis/metaanalysis.R (this cleans the input data and runs the meta-analysis)
2. Estimates.R (this sources functions in the utilities.R, file which is not a separate analysis, and generates the main estimates results)
3. ProcessingResults.R (making tables etc)
4. Plots.R (makes remaining plots)


