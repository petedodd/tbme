# tbme
TBC


## Notes on running the analysis

### R package requirements

The following packages are required and can be installed from CRAN:
- here
- blah

### Directory structure

Most analysis files are at the top level. However, metaanalysis/ contains the data and analysis for the meta-analysis, and returns the meta-analysis figure and results at this level (as well as the cleaned input count data).

To run without modification, two additional directories must be created:

1. metaanalysis/metaplots/
2. outdata/


### Order

The analyses must be run in the following order:

1. metaanalysis/metaanalysis.R (this cleans the input data and runs the meta-analysis)
2. Estimates.R (this sources functions in the utilities.R, file which is not a separate analysis, and generates the main estimates results)
3. ProcessingResults.R ()
4. Plots.R (makes remaining plots)


# TODO

(reg,global) X

Incidence:
HIV-negative treated
HIV-positive treated
Total treated
HIV-negative untreated
HIV-positive untreated
Total untreated
Total HIV-positive
Total HIV-negative
Total



- uncertainty
- SA outputs
- correct denominator for map
