defineModule(sim, list(
  name = "extractLand",
  description = "Point extraction of landscape values",
  keywords = "",
  authors = c(person("Julie", "Tuner", email = "", role = c("aut", "cre")),
              person("Rory", "McInnes", email = "", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(extractLand = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "extractLand.Rmd"),
  reqdPkgs = list("SpaDES.core (>= 2.1.8.9001)", "ggplot2", "terra", "data.table", "distanceto"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("caribouYears", "integer", NULL, NA, NA,
                    paste0("This is the year range of data we want to run models for.")),
    defineParameter("histLandYears", "integer", 2005:2023, NA, NA,
                    paste0("This is the year range we use past (not simulated) landscape layers.")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                    "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "landscapeYearly", objectClass = 'spatRaster',
                 desc = 'list of spatRaster stacks of the yearly landscape layers'),
    expectsInput(objectName = "landscape5Yearly", objectClass = 'spatRaster',
                 desc = 'list of spatRaster stacks of the 5 yearly landscape layers'),
    expectsInput(objectName = "tracks", objectClass = 'data.table',
                 desc = 'tracks of used and random steps to extract environmental covariates for')
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = 'extractedLand', objectClass = "data.table",
                  desc = "Landscape values and distance calculations matched by year to points")
  )
))


doEvent.extractLand <- function(sim, eventTime, eventType, priority) {
  switch(
    eventType,
    init = {
      # run data harmonization
      sim <- scheduleEvent(sim, time(sim), "extractLand", "extractingLandscapeFeatures")
    },
    extractingLandscapeFeatures = {
      # run data harmonization
      sim <- extractLandFeatures(tracks = sim$tracks, 
                                 landscapeYearly = sim$landscapeYearly, 
                                 landscape5Yearly = sim$landscape5Yearly,
                                 histLandYears = P(sim)$histLandYears)
    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")


  return(invisible(sim))
}
