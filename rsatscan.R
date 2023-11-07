# ---- 0: Load Packages & Data ----

  # WARNING: Clearing your R environment will delete the 'ssenv' object 
  #          created when loading rsatscan and cause an error
  require(rsatscan)
  require(sf)
  require(tidyverse)
  require(rnaturalearth)
  require(rnaturalearthdata)

# ---- 1: Prepare Input Files ----

  # Read in CSV
  csv = read.csv(file.choose())
  
  # CasFile Requires: locationid, numcases
  casfile = csv
  
  # CtlFile Requires: locationid, numcontrols
  # PopFile Requires: 
  
  # GeoFile Requires: locationid, x-coordinate, y-coordinate
  geofile <- csv %>%
    select(ID, Latitude, Longitude) %>%     # Select only Required Columns
    filter(complete.cases(.)) %>%           # Filter out incomplete rows
    rename(`locationid` = ID,               # Rename columns for SatScan
           `x-coordinate` = Longitude,
           `y-coordinate` = Latitude) 
  
  # Write Input Files
  dir.create("ss")
  ss_folder = here::here("ss")
  write.cas(casfile, ss_folder, "CasFile")  # Case File
  # write.ctl(ctlfile, ss_folder, "CtlFile")  # Control File (Bernoulli)
  # write.pop(popfile, ss_folder, "PopFile")  # Population File (Poisson)
  write.geo(geofile, ss_folder, "GeoFile")  # Geo File
  

# ---- 2: Set Parameters ----

  # Reset Parameter File
  invisible(ss.options(reset=TRUE))
  
  # Set Input Parameters
  ss.options(list(CaseFile =        "Casfile.cas", 
                  # ControlFile =     "CtlFile.ctl",
                  CoordinatesFile = "GeoFile.geo",
                  CoordinatesType = 1,      # Latitude/Longitude 
                  ))
  
  # Set Analysis Parameters
  ss.options(list(AnalysisType = 1, # Purely Spatial
                  ModelType = 1,    # 1 = Bernoulli
                  ScanAreas = 3     # Both High & Low Rates Detect
                  ))   
  
  # Set Output Parameters
  ss.options(list(ResultsFile = "SSResult.txt",
                  OutputGoogleEarthKML = "n",
                  OutputShapefiles = "y",
                  MostLikelyClusterEachCentroidASCII = "y", # Output Hotspot Raster
                  MostLikelyClusterEachCentroidDBase = "n",
                  MostLikelyClusterCaseInfoEachCentroidASCII = "y",
                  MostLikelyClusterCaseInfoEachCentroidDBase = "n",
                  ))
  
  # Optional: Set Advanced Options Parameters
  
  # Inspect Parameter File
  head(ss.options(),3)
  
  # Write Parameter File
  write.ss.prm(ss_folder, "Parameters")

# ---- 3: Run SatScan ----

  # Input SatScan Program File Location
  sslocation = "C:/Program Files/SaTScan"
  
  # Run SatScan
  satscan = satscan(ss_folder, 
                    "Parameters", 
                    sslocation = sslocation, 
                    ssbatchfilename = "SaTScanBatch64",
                    verbose = TRUE)
  
  # View Summary
  summary(satscan)
  
  # ---- 4: Plot Output ----
  
  # Load basemap
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Plot Hot-Spot Clusters
  ggplot() +
    geom_sf(data = world) +
    geom_sf(data = satscan$shapeclust, color = "red", size = 3) +
    coord_sf(xlim = c(min(st_bbox(satscan$shapeclust)[c("xmin", "xmax")]), 
                      max(st_bbox(satscan$shapeclust)[c("xmin", "xmax")])), 
             ylim = c(min(st_bbox(satscan$shapeclust)[c("ymin", "ymax")]), 
                      max(st_bbox(satscan$shapeclust)[c("ymin", "ymax")]))) +
    theme_dark()
