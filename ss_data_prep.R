# ---- 0: Load Packages ----

  require(sf)
  require(tidyverse)
  require(adehabitatLT)

# ---- 1: Load GPX Data From Folder ----

  read_gpx_folder = function(folder_path) {
    # List all GPX files in the folder
    gpx_files = list.files(path = folder_path, pattern = "\\.gpx$", full.names = TRUE)
    
    # Extract just the file names without the .gpx extension
    file_names = sapply(gpx_files, function(file) {
      file_name = basename(file)
      sub("\\.gpx$", "", file_name)
    })
    
    # Read each file into an sf object and store them in a list
    sf_list = setNames(lapply(gpx_files, function(file) {
      sf_object = tryCatch({
        st_read(file, layer = "track_points", quiet = TRUE)
      }, error = function(e) {
        warning(paste("Error reading file:", file, "\n", e))
        return(NULL)
      })
      
      if (is.null(sf_object)) {
        return(NULL)
      }
      
      coords = st_coordinates(sf_object)
      sf_object$Longitude = coords[, "X"]
      sf_object$Latitude = coords[, "Y"]
      return(sf_object)
    }), file_names)
    
    # Remove NULL elements
    sf_list = sf_list[!sapply(sf_list, is.null)]
    
    if (length(sf_list) == 0) {
      stop("No valid GPX files found in the directory.")
    }
    
    # Standardize columns in all sf objects
    all_cols = unique(unlist(lapply(sf_list, names)))
    sf_list = lapply(sf_list, function(sf) {
      missing_cols = setdiff(all_cols, names(sf))
      for (col in missing_cols) {
        sf[[col]] = NA
      }
      sf[all_cols]
    })
    
    # Combine all sf objects into a single sf object
    combined_sf = do.call(rbind, sf_list)
    
    # Convert the combined sf object to a dataframe
    combined_df = as.data.frame(st_drop_geometry(combined_sf))
    
    # Return both the combined dataframe and the list of individual sf objects
    return(list(combined_df = combined_df, sf_list = sf_list))
  }
  
  # Extract Data from Folder
  gpx_folder = read_gpx_folder(choose.dir())
  gpx_list = gpx_folder$sf_list
  track_all = gpx_folder$combined_df

# ---- 2: Isolate Trajectory Movement Points ----
  
  # 2.0: Check for time duplicates
  duplicates = duplicated(track_all$time)
  track_all[duplicates, ]
  track_all = track_all[!duplicated(track_all$time), ]
  
  # 2.1: Create Trajectory Element
  traj = as.ltraj(xy = track_all[, c("Longitude", "Latitude")], 
                   date = track_all$time,
                   id = "animal1", 
                   # proj4string = crs(), # Use for meters in dist output
                   typeII = TRUE)
  
  # 2.2: Remove Points w/ Long Idle Times
  traj_filt = traj[[1]] %>% 
    filter(dt < 300) %>%    # 300 seconds = 5 minutes of idle
    st_as_sf(coords = c("x", "y"), 
             crs = 4326)    # WGS84 Assumptions **** CHECK FOR UPDATE
  
  # 2.3: Prepare Filtered Data for SatScan
  # coords = st_coordinates(traj_filt)
  # traj_filt$Longitude = coords[, "X"]
  # traj_filt$Latitude = coords[, "Y"]
  # ss_data = st_drop_geometry(traj_filt)
  
  # 2.4: Combine Project Columns w/ Traj Output
  traj_filt$ID <- rownames(traj_filt)
  track_all$ID <- rownames(track_all)
  final_df <- merge(traj_filt, track_all, by = "ID")
  st_write(final_df, "Tick_Points.shp")
  
  # 2.6: Create Case & Geo Files
  ss_case = ss_data %>%
    mutate(cases = 1) %>%
    select(cases, date)
  
  ss_geo = ss_data %>%
    select(Latitude, Longitude) %>%
    rename(lat = Latitude, 
           long = Longitude)

  # 2.7: Write Case & Geo File to CSV
  write.csv(ss_case, "ss_case.csv")
  write.csv(ss_geo, "ss_geo.csv")
  
