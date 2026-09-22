eval = FALSE does not execute it in R but still shows the code

## R map of adder samples

    # set working directory
    setwd("C:/Users/dange/map")

    # READ SAMPLE INFORMATION

    samples <- read.csv(
      "sample_coordinates_final.csv",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Remove accidental spaces from sample names
    samples$sample <- trimws(samples$sample)


    # RETAIN S10 AND SET ITS CORRECT COORDINATES

    # S10 is intentionally NOT removed.
    # Set its coordinates explicitly to the new location.

    s10_rows <- grepl("^S10", samples$sample)

    if (any(s10_rows)) {
      
      samples$latitude[s10_rows]  <- 50.837883
      samples$longitude[s10_rows] <- -1.04272
      
    } else {
      
      warning(
        "S10 was not found in the CSV. ",
        "If S10 needs to be added as a new sample, its other attributes ",
        "(e.g. sitecol and symbol) will also need to be supplied."
      )
    }


    # Check S10 coordinates
    samples %>%
      filter(grepl("^S10", sample))


    # CONVERT SAMPLE DATA TO SPATIAL DATA

    samples_sf <- st_as_sf(
      samples,
      coords = c("longitude", "latitude"),
      crs = 4326,
      remove = FALSE
    )


    # CONVERT TO BRITISH NATIONAL GRID

    samples_bng <- st_transform(
      samples_sf,
      27700
    )


    # TOWN DATA FOR MAP LABELLING

    towns <- data.frame(
      name = c("Lyndhurst", "Brockenhurst"),
      longitude = c(-1.576, -1.574),
      latitude  = c(50.872, 50.819)
    )


    # Convert town data to spatial data
    towns_sf <- st_as_sf(
      towns,
      coords = c("longitude", "latitude"),
      crs = 4326
    )


    # Transform to British National Grid
    towns_bng <- st_transform(
      towns_sf,
      27700
    )


    # CREATE MAIN MAP EXTENT

    # IMPORTANT:
    # This bbox now includes ALL samples, including S10.
    # Because S10 is much further east, the xmax value will
    # automatically increase to include it.

    bbox <- st_bbox(samples_bng)


    # Add 5 km padding around ALL samples

    bbox["xmin"] <- bbox["xmin"] - 5000
    bbox["xmax"] <- bbox["xmax"] + 5000
    bbox["ymin"] <- bbox["ymin"] - 5000
    bbox["ymax"] <- bbox["ymax"] + 5000


    # Convert bbox to spatial polygon
    bbox_sf <- st_as_sfc(
      bbox,
      crs = 27700
    )


    # CHECK MAP EXTENT

    print(bbox)

    cat("\nMap extent including S10:\n")
    cat("xmin:", bbox["xmin"], "\n")
    cat("xmax:", bbox["xmax"], "\n")
    cat("ymin:", bbox["ymin"], "\n")
    cat("ymax:", bbox["ymax"], "\n")


    # DOWNLOAD SATELLITE IMAGERY

    # S10 substantially increases the east-west extent of the map.
    # Zoom 10 is used instead of 11 so that the larger area does
    # not require an unnecessarily large number of satellite tiles.

    satellite <- get_tiles(
      bbox_sf,
      provider = "Esri.WorldImagery",
      crop = TRUE,
      zoom = 10,
      project = TRUE
    )


    # MAIN MAP

    main_map <- ggplot() +
      
    # Satellite imagery
      
    layer_spatial(satellite) +
      
      
    # Sample points
      
    geom_sf(
      data = samples_bng,
      aes(
        fill = sitecol,
        shape = factor(symbol)
      ),
      colour = "black",
      size = 4,
      stroke = 0.8
    ) +
      
      
    # Town names
      
    geom_sf_text(
      data = towns_bng,
      aes(label = name),
      colour = "white",
      fontface = "bold",
      size = 4,
      nudge_y = 1200
    ) +
      
      
    # Use colours supplied in CSV
      
    scale_fill_identity() +
      
      
    # Sample symbols
      
    scale_shape_manual(
      values = c(
        "0" = 21,   # circle
        "1" = 24,   # triangle
        "2" = 22    # square
      )
    ) +
      
      
    # Main map extent

    coord_sf(
      xlim = c(
        bbox["xmin"],
        bbox["xmax"]
      ),
      ylim = c(
        bbox["ymin"],
        bbox["ymax"]
      ),
      expand = FALSE
    ) +
      
      

    # Scale bar
      
    annotation_scale(
      location = "bl",
      width_hint = 0.25,
      text_cex = 0.8
    ) +
      
      
    # North arrow
      
    annotation_north_arrow(
      location = "tl",
      which_north = "true",
      style = north_arrow_fancy_orienteering()
    ) +
      
      

    # Theme
      
    theme(
      
      panel.background = element_blank(),
      
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.7
      ),
      
      panel.grid.major = element_line(
        colour = "white",
        linewidth = 0.3
      ),
      
      panel.grid.minor = element_blank(),
      
      axis.text = element_text(
        colour = "black",
        size = 10
      ),
      
      axis.title = element_blank(),
      
      legend.position = "none",
      
      plot.margin = margin(
        5, 5, 5, 5
      )
    )


    # UK MAP FOR INSET

    uk <- ne_countries(
      scale = "medium",
      country = "United Kingdom",
      returnclass = "sf"
    )


    # NEW FOREST EXTENT FOR INSET

    new_forest_box <- data.frame(
      xmin = -1.90,
      xmax = -1.35,
      ymin = 50.76,
      ymax = 50.92
    )


    # UK INSET

    uk_inset <- ggplot() +
      
      # UK outline
      geom_sf(
        data = uk,
        fill = "grey85",
        colour = "black",
        linewidth = 0.5
      ) +
      
      
    # New Forest box
      geom_rect(
        data = new_forest_box,
        aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax
        ),
        fill = NA,
        colour = "#00A651",
        linewidth = 1.2
      ) +
      
      
      coord_sf(
        xlim = c(-8.5, 2.5),
        ylim = c(49.5, 59.0),
        expand = FALSE
      ) +
      
      
      theme_void() +
      
      
      theme(
        panel.background = element_rect(
          fill = "white",
          colour = "black",
          linewidth = 0.7
        ),
        
        plot.margin = margin(
          0, 0, 0, 0
        )
      )


    # COMBINE MAIN MAP AND UK INSET

    final_map <- ggdraw() +
      
      # Main map
      draw_plot(
        main_map,
        x = 0,
        y = 0,
        width = 1,
        height = 1
      ) +
      
    # UK inset
      draw_plot(
        uk_inset,
        x = 0.73,
        y = 0.66,
        width = 0.25,
        height = 0.30
      )


    # DISPLAY FINAL MAP

    final_map
