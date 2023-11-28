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
  
  # 2.1: Batch Process Individual Trajectories
  batch_ltraj = function(gpx_list) {
    # Apply as.ltraj to each individual
    ltraj_list = lapply(names(gpx_list), function(name) {
      # Load Dataframe
      df = gpx_list[[name]]
      df = st_drop_geometry(df)
      
      # Quality Control Dataframe
      duplicates = duplicated(df$time)
      df = df[!duplicated(df$time), ]
      df = df[complete.cases(df$Longitude, df$Latitude, df$time), ]
      
      # Run Trajectory Function
      as.ltraj(xy = df[, c("Longitude", "Latitude")], 
               date = df$time, 
               id = name, 
               typeII = TRUE)
    })
    
    # Set the names of the ltraj_list to be the same as those of gpx_list
    names(ltraj_list) = names(gpx_list)
    
    return(ltraj_list)
  }
  ltraj_list = batch_ltraj(gpx_list)
  
  # 2.2: Convert Trajectories to Dataframes & Combine
  ltraj_df_list = lapply(names(ltraj_list), function(name) {
    # Access the ltraj object in the nested structure
    ltraj_obj = ltraj_list[[name]][[1]]
    # Convert the ltraj object to a dataframe
    df = as.data.frame(ltraj_obj)
    # Add the ID column
    df$ID = name
    return(df)
  })
  PTS = do.call(rbind, ltraj_df_list)
  
  # 2.2: Remove Points w/ Long Idle Times
  PTS_Filt = PTS %>% 
    filter(dt < 420)
  
  PTS_Filt_SF = PTS %>% 
    filter(dt < 900) %>%    
    st_as_sf(coords = c("x", "y"), 
             crs = 4326)    # WGS84 Assumptions **** CHECK FOR UPDATE
  
  # 2.3: Write PTS Shapefile
  st_write(PTS_Filt_SF, "PTS.shp")
  
  # 2.4: Create Case & Geo Files
  ss_case = PTS_Filt_SF %>%
    mutate(cases = 1) %>%
    select(cases, date)
  
  ss_geo = PTS_Filt_SF %>%
    select(Latitude, Longitude) %>%
    rename(lat = Latitude, 
           long = Longitude)

  # 2.5: Write Case & Geo File to CSV
  write.csv(ss_case, "ss_case.csv")
  write.csv(ss_geo, "ss_geo.csv")
  
