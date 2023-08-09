# Introduction

This repository was used to develop analyses for a specific WHO request to look at risk of YF in moderate risk countries. It mainly relies on code developed for RAPTORX, a shiny application to look at the risk of spread of YF given a radiation model of movement.

# Running the code

The code can be foudn in `src` folder- there are two reports, one for risk per country and one for risk in all countries. The risk per country must be run for all relevant countries first, then the risk for all countries (see orderly documentation for running a report). The outputs can be found in the archive folder.

# orderly

This is an [`orderly`](https://github.com/vimc/orderly) project.  The directories are:

* `src`: create new reports here
* `archive`: versioned results of running your report
* `data`: copies of data used in the reports
