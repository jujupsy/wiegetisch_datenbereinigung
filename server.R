## Wiegetisch Preprocess Data - server file


server <- function(input, output, session) {

  ### [process_data] monitoring status:
  # - not_loaded
  # - loading_data
  # - data_loaded
  processing_states <- reactiveValues(step = 0, step_name = "not_loaded")


  # Dateien einlesen
  observeEvent(input$start_loading, {
    processing_states$step <- 1
    processing_states$step_name <- "loading_data"
  })

  ## Einlesen print erste Zeilen und als binaere .rds Datei speichern
  output$inspect_loaded <- renderPrint({
    #req(load_event$start)
    if(processing_states$step < 1){
      cat("No data loaded.")
    }
    else {

      # load data and save as dat_raw.rds
      dat_raw <- load_and_save_raw_data(
        file_path_txt = "./raw_data",
        name_dat_raw = input$name_dat_raw,
        snack_names = input$snack_names)
      calc_cleaning_prameters(name_dat_raw = input$name_dat_raw)

      vp_codes <- unique(dat_raw$vp)
      cat(paste0("Data from ", length(vp_codes), " subjects loaded.\n"))
      cat("Head and tail of dataset: check if all columns (vp, time and snacks) are correct.\n")
      print(head(dat_raw))
      cat("...\n")
      print(tail(dat_raw))

      processing_states$step_name <- "data_loaded"

    }

  })

  ## Set meaningful range

  output$load_data <- renderUI({
    processing_states$step_name 
    files <- list.files("./data_temp", pattern = ".rds")
    files <- files[!substr(files, 1, 6) == "paras_"] # exclude paras_... files
    selectInput("input_files", "Choose data", files, multiple = FALSE, selected = files[1])
  })

  output$inspect_loaded_rds <- renderPrint({
    #req(load_event$start)
    data <- get_raw_data()

    if(nrow(data)< 1){
      cat("No data loaded.")
    }
    else {
      head(data, n = 5)

    }

  })


  get_raw_data <- reactive({
    req(input$input_files)
    if(input$input_files %in% dir("data_temp"))
      dat_raw <- readRDS(paste0("./data_temp/", input$input_files))
  })

  reload_paras <- reactiveVal(0) # reload paras
  get_paras <- reactive({
    req(input$input_files)
    reload_paras()
    if(input$input_files %in% dir("data_temp"))

      paras_raw <- readRDS(paste0("./data_temp/", paste0("paras_", input$input_files)))
  })

  # track current vp and snack

  track_vp <- reactiveValues(vp = 1, snack = 1, SNACKS = "XXX", n_snack = 6)

  observe({
    track_vp$SNACKS <- colnames(get_raw_data())[3:ncol(get_raw_data())]
    track_vp$n_snack <- length(track_vp$SNACKS)
  })


  # next plot
  observeEvent(input$next_range, {
    SNACKS <- track_vp$SNACKS 
    if(track_vp$snack == track_vp$n_snack){
      track_vp$vp <- track_vp$vp + 1
      track_vp$snack <- 1
    } else {
      track_vp$snack <- track_vp$snack + 1
    }
  })

  # previous plot
  observeEvent(input$previous_range, {
    SNACKS <- track_vp$SNACKS 
    if(track_vp$snack == 1){
      track_vp$vp <- track_vp$vp - 1
      track_vp$snack <- track_vp$n_snack
    } else {
      track_vp$snack <- track_vp$snack - 1
    }
  })

  # disable/enable previous/next buttons
  output$next_show_button <- reactive({
    !(track_vp$vp >= length(unique(get_raw_data()$vp)) & track_vp$snack == track_vp$n_snack)
  })
  outputOptions(output, "next_show_button", suspendWhenHidden = FALSE)  
  output$previous_show_button <- reactive({
    !(track_vp$vp <= 1 & track_vp$snack == 1)
  })
  outputOptions(output, "previous_show_button", suspendWhenHidden = FALSE)  


  output$track_status <- renderText({
    paste0("Vp: ", track_vp$vp, " | Snack: ", track_vp$snack)
  })


  observeEvent(c(input$tabs, input$input_files, input$previous_range, input$next_range),{
    data <- get_raw_data()
    vp_codes <- unique(data$vp)
    vp <- vp_codes[track_vp$vp]
    snack <- track_vp$SNACKS[track_vp$snack]

    paras <- get_paras()

    num_points <- 10
    y_max <- max(data[data$vp == vp, snack], na.rm = T)
    y_min <- min(data[data$vp == vp, snack], na.rm = T)

    # update  slider range
    updateSliderInput(session, "man_range", min = floor(y_min), max = ceiling(y_max), 
      value = c(paras[paras$vp == vp & paras$snack == snack, "range_min"], 
        paras[paras$vp == vp & paras$snack == snack, "range_max"]))
    # update slider delete first  points
    updateSliderInput(session, "man_delete_first", 
      value = paras[paras$vp == vp & paras$snack == snack, "delete_first"])
    # update sliders running mean parameters
    updateSliderInput(session, "man_first_lag", 
      value = paras[paras$vp == vp & paras$snack == snack, "first_lag"])
    updateSliderInput(session, "man_first_error", 
      value = paras[paras$vp == vp & paras$snack == snack, "first_error"])
    updateSliderInput(session, "man_second_lag", 
      value = paras[paras$vp == vp & paras$snack == snack, "second_lag"])
    updateSliderInput(session, "man_second_error", 
      value = paras[paras$vp == vp & paras$snack == snack, "second_error"])

  })

  ### show mouse in plot
  output$show_mouse <- renderText({
    req(input$plot_click$x)
    paste0("x=", round(input$plot_click$x), "\ny=", round(input$plot_click$y, 4))
  })


  #### clean plot
  output$range_plot <- renderPlot({ 
    req(input$input_files)
    data <- get_raw_data()

    vp_codes <- unique(data$vp)
    vp <- vp_codes[track_vp$vp]
    snack <- track_vp$SNACKS[track_vp$snack]
    paras <- get_paras()

    dat <- data[data$vp == vp, c("time", snack)] # nur datenreihe

    par(mfrow = c(2, 2), mgp = c(2, .7, 0), mar = c(3.5, 3.5, 0, 0))
    plot(as.formula(paste(snack, " ~ time")), dat, 
      pch = ".", cex = 4, xlim = c(0, 1200))
    abline(h = input$man_range, col = "red", lwd = 2)

    dat_preclean <- dat

    # values outside range to NA
    dat_preclean[dat_preclean[, snack] < (input$man_range[1]) |
      dat_preclean[, snack] > (input$man_range[2]),  snack] <- NA  

    if(input$man_delete_first > 0){
      dat_preclean[1:input$man_delete_first, snack] <- NA
    }

    plot(as.formula(paste(snack, " ~ time")), 
      dat_preclean, 
      pch = ".", cex = 4, ylim = input$man_range + c(-5, 5), xlim = c(0, 1200))
    abline(h = input$man_range, col = "red", lwd = 1)

    if(!all(is.na(dat_preclean[, snack]))){ # stop if all NA

      ####################################
      # running mean forward and backward
      lag <- input$man_first_lag
      error <- input$man_first_error
      dat_preclean[, snack] <- running_mean_forward(dat_preclean[, snack], lag, 
        propSD = TRUE, fehler = error)
      dat_preclean[, snack] <- running_mean_backward(dat_preclean[, snack], lag, 
        propSD = TRUE, fehler = error)
      lag <- input$man_second_lag
      error <- input$man_second_error
      dat_preclean[, snack] <- running_mean_forward(dat_preclean[, snack], lag, 
        propSD = TRUE, fehler = error)
      dat_preclean[, snack] <- running_mean_backward(dat_preclean[, snack], lag, 
        propSD = TRUE, fehler = error)

      # add manual points
      add_raw <- paras[paras$vp == vp & paras$snack == snack, "add_points"]
      if(nchar(add_raw) > 2){
        addi_points <- matrix(scan(text = add_raw), ncol = 2, byrow = TRUE)

        # change points
        for(i in seq_len(nrow(addi_points))){
          x <- addi_points[i, 1]
          y <- addi_points[i, 2]
          dat_preclean[dat_preclean$time == x, snack] <- y
        }
      }

      plot(as.formula(paste(snack, " ~ time")), 
        dat_preclean, 
        pch = ".", cex = 4, ylim = input$man_range + c(-5, 5), xlim = c(0, 1200))
      if(nchar(add_raw) > 2){
        points(addi_points[, 1], addi_points[, 2], col = "red", cex = 1.5)
      }

      ###################################
      # only decreasing and interpolation
      dat_preclean[, snack] <- interpolate_na(dat_preclean[, snack], cut = 0)
      dat_preclean[, snack] <- only_decreasing(dat_preclean[, snack])


      plot(as.formula(paste(snack, " ~ time")), 
        dat, col = "grey", ylim = input$man_range + c(-5, 5), xlim = c(0, 1200))
      points(as.formula(paste(snack, " ~ time")), dat_preclean, 
        pch = ".", cex = 4, type = "l")


      # changes in paras? --> safe in file paras_...
      if(!all(c(
            paras[paras$vp == vp & paras$snack == snack, "range_min"]    == input$man_range[1], 
            paras[paras$vp == vp & paras$snack == snack, "range_max"]    == input$man_range[2],
            paras[paras$vp == vp & paras$snack == snack, "delete_first"] == input$man_delete_first,
            paras[paras$vp == vp & paras$snack == snack, "first_lag"]    == input$man_first_lag,
            paras[paras$vp == vp & paras$snack == snack, "first_error"]  == input$man_first_error,
            paras[paras$vp == vp & paras$snack == snack, "second_lag"]   == input$man_second_lag,
            paras[paras$vp == vp & paras$snack == snack, "second_error"] == input$man_second_error
            ))){

        paras[paras$vp == vp & paras$snack == snack, "range_min"]    <- input$man_range[1]
        paras[paras$vp == vp & paras$snack == snack, "range_max"]    <- input$man_range[2]
        paras[paras$vp == vp & paras$snack == snack, "delete_first"] <- input$man_delete_first
        paras[paras$vp == vp & paras$snack == snack, "first_lag"]    <- input$man_first_lag
        paras[paras$vp == vp & paras$snack == snack, "first_error"]  <- input$man_first_error
        paras[paras$vp == vp & paras$snack == snack, "second_lag"]   <- input$man_second_lag
        paras[paras$vp == vp & paras$snack == snack, "second_error"] <- input$man_second_error

        saveRDS(paras, paste0("./data_temp/",  paste0("paras_", input$input_files)))
        reload_paras(reload_paras() + 1)  # reload paras
      }
    }




  })

  #### add points manually:
  ## add point
  observeEvent(input$add_point, {
    req(input$input_files)
    req(input$plot_click$x)

    data <- get_raw_data()

    vp_codes <- unique(data$vp)
    vp <- vp_codes[track_vp$vp]
    snack <- track_vp$SNACKS[track_vp$snack]
    paras <- get_paras()

    points <- paras[paras$vp == vp & paras$snack == snack, "add_points"] 

    paras[paras$vp == vp & paras$snack == snack, "add_points"] <- paste(points, 
      round(input$plot_click$x), round(input$plot_click$y, 4))

    saveRDS(paras, paste0("./data_temp/",  paste0("paras_", input$input_files)))
    reload_paras(reload_paras() + 1)  # reload paras

  })

  observeEvent(input$reset_points, {
    req(input$input_files)

    data <- get_raw_data()

    vp_codes <- unique(data$vp)
    vp <- vp_codes[track_vp$vp]
    snack <- track_vp$SNACKS[track_vp$snack]
    paras <- get_paras()
    paras[paras$vp == vp & paras$snack == snack, "add_points"] <- ""

    saveRDS(paras, paste0("./data_temp/",  paste0("paras_", input$input_files)))
    reload_paras(reload_paras() + 1)  # reload paras

  })

  ## disable/enable add_point button
  output$add_point_show_button <- reactive({
    isTruthy(input$plot_click$x)
  })
  outputOptions(output, "add_point_show_button", suspendWhenHidden = FALSE) 





  #### Export data 

  output$load_data_export <- renderUI({
    processing_states$step_name 
    files <- list.files("./data_temp", pattern = ".rds")
    files <- files[!substr(files, 1, 6) == "paras_"] # exclude paras_... files
    selectInput("input_files_export", "Choose data to export", files, multiple = FALSE, selected = files[1])
  })


  observeEvent(input$export_data_btn, {
    req(input$input_files_export)
    showModal(modalDialog(
        tagList(
          p("Are you sure you want to export the data? This process can take several minutes if your dataset is large.")
          ), 
        title="Export data",
        footer = tagList(actionButton("confirmExport", "Yes, export now."),
          modalButton("Cancel")
        )
        ))
  })

  observeEvent(input$confirmExport, {
    req(input$input_files_export)
    show_modal_spinner()
    # read data and parameters
    data <- readRDS(paste0("./data_temp/", input$input_files_export))  
    vp_codes <- unique(data$vp) # unique vp codes
    snacks <- colnames(data)[3:ncol(data)] # snack column names

    paras <- readRDS(paste0("./data_temp/", paste0("paras_", input$input_files_export)))

    for(vp in vp_codes){
      temp_data_vp <- data.frame(vp   = data[data$vp == vp, "vp"], 
        time = data[data$vp == vp, "time"])
      for(snack in snacks){

        dat <- data[data$vp == vp, c("time", snack)] # nur datenreihe    
        dat_preclean <- dat

        # values outside range to NA
        dat_preclean[dat_preclean[, snack] < paras[paras$vp == vp & paras$snack == snack, "range_min"] | dat_preclean[, snack] > paras[paras$vp == vp & paras$snack == snack, "range_max"],  snack] <- NA  

        if(paras[paras$vp == vp & paras$snack == snack, "delete_first"] > 0){
          dat_preclean[1:paras[paras$vp == vp & paras$snack == snack, "delete_first"], snack] <- NA
        }

        ####################################
        # running mean forward and backward
        lag <- paras[paras$vp == vp & paras$snack == snack, "first_lag"]
        error <- paras[paras$vp == vp & paras$snack == snack, "first_error"]
        dat_preclean[, snack] <- running_mean_forward(dat_preclean[, snack], lag, 
          propSD = TRUE, fehler = error)
        dat_preclean[, snack] <- running_mean_backward(dat_preclean[, snack], lag, 
          propSD = TRUE, fehler = error)
        lag <- paras[paras$vp == vp & paras$snack == snack, "second_lag"]
        error <- paras[paras$vp == vp & paras$snack == snack, "second_error"]
        dat_preclean[, snack] <- running_mean_forward(dat_preclean[, snack], lag, 
          propSD = TRUE, fehler = error)
        dat_preclean[, snack] <- running_mean_backward(dat_preclean[, snack], lag, 
          propSD = TRUE, fehler = error)

        # add manual points
        add_raw <- paras[paras$vp == vp & paras$snack == snack, "add_points"]
        if(nchar(add_raw) > 2){
          addi_points <- matrix(scan(text = add_raw), ncol = 2, byrow = TRUE)

          # change points
          for(i in seq_len(nrow(addi_points))){
            x <- addi_points[i, 1]
            y <- addi_points[i, 2]
            dat_preclean[dat_preclean$time == x, snack] <- y
          }
        }

        ###################################
        # only decreasing and interpolation
        dat_preclean[, snack] <- interpolate_na(dat_preclean[, snack], cut = 0)
        dat_preclean[, snack] <- only_decreasing(dat_preclean[, snack])


        # add to data frame
        temp_data_vp <- cbind(temp_data_vp, dat_preclean[, snack])
        colnames(temp_data_vp)[ncol(temp_data_vp)] <- snack # write snack as colname
      }

      # write to file
      write.table(temp_data_vp, file = paste0("./exported_data/", vp, "_", input$add_info_name, "_cleaned.txt"), quote = FALSE, row.names = FALSE) 

      # reset   
      temp_data_vp <- data.frame()


    }

    removeModal()
    remove_modal_spinner()
  })



}


