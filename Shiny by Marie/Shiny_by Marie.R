### Year : 2022
### Authors : Marie Joigneau, Amedee Roy, Sophie Bertrand
### Organism : IRD Sete
### Project : Shiny App for tropical bird movement dataset


### ----- I. INTRODUCTION --------------------
### -- I.1. LIBRARY --------------------------
library(shiny)
library(leaflet)
library(viridis)
library(leaflegend)
library(DT)
library(plotly)
library(RSQLite)
library(plyr)
library(dplyr)
library(stringr)
library(markdown)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyBS)
library(shinyjs)
library(GeoLight)
library(lubridate)
library(crosstalk)
library(raster)
library(htmlwidgets)
library(htmltools)

### -- I.2. BUSY INDICATOR --------------------

# Busy Indicators
# from https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    style="display:inline-block",
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the "busy" message, hide the other messages, disable the button
  loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

### -- I.3. DATA LOADING ----------------------

#setwd("D:/OneDrive/Stage CESURE - Sete oiseaux marins/Shiny/Shiny_by Marie")

# Metadata table = merging of all the metadata tables in the folder : 
list_meta <- list.files("./metadata")
#print(paste("list meta ",list_meta))
metadata <- read.csv(paste("./metadata",list_meta[1],sep="/"), header = TRUE, sep=";",dec=".")
if (length(list_meta)>1){
  for (i in 2:length(list_meta)){
    #print(paste("folder ",i))
    #print(list_meta[i])
    meta1 <- read.csv(paste("./metadata",list_meta[i],sep="/"), header = TRUE, sep=";",dec=".")
    metadata <- rbind(metadata,meta1)
  }
}

# We put away all the trash files :
idx_NA_file <- which(is.na(metadata$file_formatted_by_Marie)==TRUE)
idx_trash_file_renamed <- which(str_detect(metadata$file_renamed_by_Marie,"trash")==TRUE)
idx_trash_file_formatted <- which(str_detect(metadata$file_formatted_by_Marie,"trash")==TRUE)
idx_NA_trash_all <- sort(unique(c(idx_NA_file,idx_trash_file_renamed,idx_trash_file_formatted)))
#print(paste("idx_NA_trash_all ",idx_NA_trash_all))
if (length(idx_NA_trash_all)>0){
  metadata <- metadata[-idx_NA_trash_all,]
}
#print(metadata)

# We don't take abbreviation columns neither local_hour column
metadata <-data.frame(archipelago=metadata["archipelago"],date=metadata["date"],island=metadata["island"],species=metadata["species"],bird_id=metadata["bird_id"],band=metadata["band"],sex=metadata["sex"],sensor_name=metadata["sensor_name"],gps_id=metadata["gps_id"],file_formatted_by_Marie=metadata["file_formatted_by_Marie"])

# Rename
metadata["unique_name"] <- substr(metadata$file_formatted_by_Marie,1,nchar(metadata$file_formatted_by_Marie)-23)

# We take care of date and year
metadata$date = as.Date(metadata$date,format="%d/%m/%Y")
metadata$year = year(metadata$date)

# These ones don't have NA
#metadata$file_formatted_by_Marie <- as.character(metadata$file_formatted_by_Marie)
metadata$unique_name <- as.factor(metadata$unique_name)

### ------- PB WITH NA -----------

# We put in character to be able to change the factor columns
metadata$archipelago <- as.character(metadata$archipelago)
metadata$island <- as.character(metadata$island)
metadata$species <- as.character(metadata$species)
metadata$bird_id <- as.character(metadata$bird_id)
metadata$band <- as.character(metadata$band)
metadata$sex <- as.character(metadata$sex)
metadata$sensor_name <- as.character(metadata$sensor_name)
metadata$gps_id <- as.character(metadata$gps_id)
metadata$year <- as.character(metadata$year)

# We rename NA to put them into factor later
metadata$archipelago[which(is.na(metadata$archipelago))] <- "NA"
metadata$island[which(is.na(metadata$island))] <- "NA"
metadata$species[which(is.na(metadata$species))] <- "NA"
metadata$bird_id[which(is.na(metadata$bird_id))] <- "NA"
metadata$band[which(is.na(metadata$band))] <- "NA"
metadata$sex[which(is.na(metadata$sex))] <- "NA"
metadata$sensor_name[which(is.na(metadata$sensor_name))] <- "NA"
metadata$gps_id[which(is.na(metadata$gps_id))] <- "NA"
metadata$year[which(is.na(metadata$year))] <- "NA"

# Turn into factor
metadata$archipelago <- as.factor(metadata$archipelago)
metadata$island <- as.factor(metadata$island)
metadata$species <- as.factor(metadata$species)
metadata$bird_id <- as.factor(metadata$bird_id)
metadata$band <- as.factor(metadata$band)
metadata$sex <- as.factor(metadata$sex)
metadata$sensor_name <- as.factor(metadata$sensor_name)
metadata$gps_id <- as.factor(metadata$gps_id)
metadata$year <- as.factor(metadata$year)

# We have the list of levels for each factor
level_archipelago <- levels(metadata$archipelago)
level_island <- levels(metadata$island)
level_species <- levels(metadata$species)
level_band <- levels(metadata$band)
level_sex <- levels(metadata$sex)
level_sensor_name <- levels(metadata$sensor_name)
level_gps_id <- levels(metadata$gps_id)
level_year <- levels(metadata$year)


### -- I.4. COLOR PALETTE ----------------------

pal <- c(    '#1f77b4',  # muted blue
             '#ff7f0e',  # safety orange
             '#2ca02c',  # cooked asparagus green
             '#d62728',  # brick red
             '#9467bd',  # muted purple
             '#8c564b',  # chestnut brown
             '#e377c2',  # raspberry     yogurt pink
             '#7f7f7f',  # middle gray
             '#bcbd22',  # curry yellow-green
             '#17becf'   # blue-teal
)
pal <- rep(pal, 1000)

### ----- II. USER INTERFACE --------------------

ui <- dashboardPage(
  dashboardHeader(title = "SeaBirdMap_Marie"),
  
  # SideBar Menu  --------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("FIELDWORK", tabName = "tabFieldwork", icon = icon("binoculars"),
               selectizeInput("selected_archipelago", "Archipelago:",
                              choices = level_archipelago, multiple = TRUE,
                              selected=metadata$archipelago[1])),
      
      menuItem("BIRD", tabName = "tabBird", icon = icon("crow"),
               selectizeInput("selected_species", "Species:",
                              choices = level_species, multiple = TRUE,
                              selected=metadata$species[1]),
               radioGroupButtons("selected_sex", "Sex:",
                                 choices = c(`<i class='fas fa-venus-mars'></i>` = "Indifferent",
                                             `<i class='fas fa-mars'></i>` = "M",
                                             `<i class='fas fa-venus'></i>` = "F"),
                                 justified = TRUE),
               selectizeInput("selected_band", "Band:",
                              choices = level_band, multiple = TRUE,
                              selected=metadata$band[1]),
               prettyCheckbox(
                 inputId = "all_band",
                 label = " Load All Band",
                 status = "primary",
                 animation = "jelly"
               )),
      
      menuItem("DEPLOYMENT", tabName = "tabDeployment", icon = icon("route"),
               selectizeInput("selected_sensor_name", "Sensor name:",
                              choices = level_sensor_name, multiple = TRUE,
                              selected=metadata$sensor_name[1]),
               dateRangeInput("selected_date", "Date :",
                              start = min(metadata$date),
                              end = max(metadata$date),
                              min = min(metadata$date)-years(1),
                              max = max(metadata$date)+years(1)),
               prettyCheckbox(
                 inputId = "all_date",
                 label = " Any Time",
                 status = "primary",
                 animation = "jelly"
               )
               ),
      
      menuItem("TOOLS",tabName = "tabtools",icon = icon("wrench"),
               selectizeInput("selected_colour",
                              span(icon("palette"), "Colour"),
                              choices = c("Bird" = "unique_name","Species" = "species",
                                          "Sex" = "sex","Sensor Name" = "sensor_name","Year" = "year"),
                              selected = "id")
      ),
      
      br(),
      br(),
      
      prettyCheckbox(
        inputId = "all_data",
        label = " Load All Data",
        status = "primary",
        animation = "jelly"
      ),
      
        actionBttn(
          inputId = "menu_action",
          label = "load data",
          style = "material-flat",
          color = "primary" # “default”, “primary”, “warning”, “danger”, “success”, “royal”
        )
      )),
  
  # Body  --------------------------------------------
  dashboardBody(textOutput("Debugging"),
                dataTableOutput("table_debugging"),
                leafletOutput("map_bird", height = "850px"),
                fluidRow(
                  dataTableOutput("table_metadata"))
  )
)

### ----- III. SERVER -------------------------

server <- function(input, output, session) {
  
  ### Setting up the base leaflet map  ------------------------------
  output$map_bird <- renderLeaflet({
    leaflet(metadata) %>%

      ## Defining the center of our map
      setView(lng = -60,
              lat = -9,
              zoom = 3) %>%

      ## Choosing our map base
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addWMSTiles(
        "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
        layers = c("1-degree grid", "5-degree grid"),
        options = WMSTileOptions(format = "image/png8", transparent = TRUE),
        attribution = NULL,group = 'Graticules')

  })
  
  ### Data selection : the_chosen_ones ---------------------------------
  
  the_chosen_ones <- eventReactive( input$menu_action, { 
    # Use eventReactive to create a calculated VALUE that only updates in response to an event
    # eventReactive = reactive if push load data button (input$menu_action)
    selection <- rep(TRUE, nrow(metadata)) # selection is a vector of logical values, if we want the line it's TRUE, else it's FALSE
    load = TRUE
    
    #print("THE CHOSEN ONES (data selection) -------------------------------")
    
    #print(paste("basis selection ",selection))
    
    if(!input$all_data){ # if we don't choose to load all (! is the logical-NOT operator in R)
      
      #print("we don't choose to load all data ----------------")
      
      # archipelago selection
      if(is.null(input$selected_archipelago)){ # we first check if there is a selected one, if not ERROR
        load <- FALSE
      } else { # if it's the case, we check which line in metadata we have the selection
        #print(paste("selected archipelago ",input$selected_archipelago))
        selection <- selection & is.element(metadata$archipelago, input$selected_archipelago) #is.element(x1,x2) = are x1 elements in x2 (x1, x2 are vectors)
      }
      #print(paste("archipelago selection ",selection))
      
      # species selection
      if(is.null(input$selected_species)){
        load <- FALSE
      } else {
        selection <- selection & is.element(metadata$species, input$selected_species) 
      }
      #print(paste("species selection ",selection))
      
      # sex selection
        # pb with sex selection if only male or female in dataset
      if(is.null(input$selected_sex)){
        load <- FALSE
      } else {
        if(input$selected_sex != "Indifferent"){
          #print("sex not indifferent")
          selection <- selection & 
            (metadata$sex == input$selected_sex)  
        }
      }
      #print(paste("sex selection ",selection))
      
      # band selection
      if(!input$all_band){
        #print("we don't choose to load all band")
        if(is.null(input$selected_band)){
          load <- FALSE
        } else {
          selection <- selection & is.element(metadata$band, input$selected_band) 
        }
      }
      selection <- selection
      #print(paste("band selection ",selection))
      
      
      
      
      # sensor_name selection
      if(is.null(input$selected_sensor_name)){
        load <- FALSE
      } else {
        selection <- selection & is.element(metadata$sensor_name, input$selected_sensor_name) 
      }
      #print(paste("sensor selection ",selection))
      
      # date selection
      if(!input$all_date){
        selection <-  selection &
          (as.Date(metadata$date) >= as.Date(input$selected_date[1])) &
          (as.Date(metadata$date) <= as.Date(input$selected_date[2]))
      }
    }
    
    #selection <- c(TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
    
    withBusyIndicatorServer("menu_action", { #We select the id of the birds chosen by the selection above
      if(!load){
        stop("wrong request")
      } else {
        #print(paste("the chosen ones ",metadata$unique_name[which(selection)]))
        the_chosen_ones <- metadata$unique_name[which(selection)] # column of id chosen
        #print(the_chosen_ones)
      }
    })
  })
  
  
  ### The map---------- -----------------------------------------------
  observeEvent( input$menu_action, { # Use observeEvent whenever you want to perform an ACTION in response to an event
    
    ### Reactivity of the table for metadata ---------------------------------------------
    
    idx_meta_chosen <- c()
    for (i in 1:length(the_chosen_ones())){
      idx_meta_chosen <- c(idx_meta_chosen,which(the_chosen_ones()[i]==metadata$unique))
      #print(paste("i ",i))
      #print(paste("idx_meta_chosen ",idx_meta_chosen))
    }
    
    #print("----------TABLE FOR METADATA -------------------------")

    metadata_chosen <- metadata[idx_meta_chosen,] 
    output$table_metadata <- renderDT({
      datatable(metadata_chosen, class = 'cell-border stripe',
                options = list(paging = FALSE, lengthChange = FALSE, scrollY = "600px", scrollX = T),
                rownames = FALSE)})
    #print(metadata_chosen)
    
    ### Reactivity of the colony --------------------------------------------------------- 
    
    #print("----------COLONIES CHOSEN -------------------------")
    
    # We obtain the names of the colonies chosen by the use : colony_chosen
    colony_chosen <- metadata$archipelago[idx_meta_chosen]
    colony_chosen <- unique(colony_chosen)
    #print(paste("colony_chosen ",colony_chosen))
    
    # we open the colony metadata with the position of each colony
    all_colony <- read.csv("metadata_colony_shiny.csv",sep=";",dec=",")
    #print("all colonies")
    #print(all_colony)
    
    # We obtain in which line we have the colony in which we are interested
    idx_colony_chosen <- c()
    for (i in 1:length(colony_chosen)){
      idx_colony_chosen <- c(idx_colony_chosen,which(colony_chosen[i]==all_colony$archipelago))
      #print(paste("i ",i))
      #print(paste("idx_colony ",idx_colony_chosen))
    }
    #print(paste("idx_colony final ",idx_colony_chosen))
    
    # And finally we obtain the sub dataframe with only the colony we need
    position_colony_chosen <- all_colony[idx_colony_chosen,]
    #print("df all colony_chosen")
    #print(position_colony_chosen)
    #print("position?")
    #print(as.numeric(position_colony_chosen$lon[1]))
    #print(as.numeric(position_colony_chosen$lat[1]))
    
    ### Reactivity of the color --------------------------------------------------------
    
    #print("---------- COLOR -------------------------")
    
    # We select the column of metadata chosen for the colour
    if (input$selected_colour == "unique_name"){
      metadata_column_colour <- metadata_chosen$unique_name
    }
    if (input$selected_colour == "species"){
      metadata_column_colour <- metadata_chosen$species
    }
    if (input$selected_colour == "sex"){
      metadata_column_colour <- metadata_chosen$sex
    }
    if (input$selected_colour == "sensor_name"){
      metadata_column_colour <- metadata_chosen$sensor_name
    }
    if (input$selected_colour == "year"){
      metadata_column_colour <- metadata_chosen$year
    }
    metadata_column_colour <- as.character(metadata_column_colour)
    metadata_column_colour <- as.factor(metadata_column_colour)
    #print("metadata_column_colour")
    #print(metadata_column_colour)
    level_col <- levels(metadata_column_colour)
    #print("level_col")
    #print(level_col)
    
    # We create a vector of the color for each line of the metadata
    col_final <- c(1:length(metadata_column_colour))
    #print(col_final)
    
    # For each line of the metadata, a color is attributed, depending of the column chosen to be the color
    for (i in 1:length(level_col)){
      #print(paste("i ",i))
      idx_level <- which(metadata_column_colour==level_col[i])
      #print(paste("idx level ",idx_level))
      color_pal=pal[i]
      col_final[idx_level] <- color_pal
    }
    #print(col_final)
    
    ### Reactivity of the map ---------------------------------------------------------
    output$map_bird <- renderLeaflet({ 
      
      #print("----------FIRST MAP -------------------------")
      #print(as.numeric(position_colony_chosen$lon))
      #print(as.numeric(position_colony_chosen$lat))
      
      # Loading bar
      sendSweetAlert( 
        session,
        title = "LOADING DATA",
        text = tags$div(
          progressBar(
            id = "map_progress",
            value = 0,
            total = length(metadata_chosen$archipelago),
            title = "",
            display_pct = TRUE
          )
        ),
        btn_labels = NA,
        type = "success",
        closeOnClickOutside = FALSE
      )
      
      # we don't want legend if the color is different for each trajectory
      if (input$selected_colour != "unique_name"){
        m <-
          leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          setView(lng = mean(as.numeric(position_colony_chosen$lon)),
                  lat = mean(as.numeric(position_colony_chosen$lat)),
                  zoom = 3) %>%
          addMarkers(lng = as.numeric(position_colony_chosen$lon),
                     lat = as.numeric(position_colony_chosen$lat),
                     icon = makeIcon("./www/home.png", iconWidth = 18, iconHeight = 18)) %>%
          # the legend part (= only difference between the 2 paragraphs):
          addLegend("bottomright",colors=pal[1:length(level_col)],labels=level_col, title = input$selected_colour, opacity = 1)
      }else{
        m <-
          leaflet() %>%
          addProviderTiles(providers$Esri.OceanBasemap) %>%
          setView(lng = mean(as.numeric(position_colony_chosen$lon)),
                  lat = mean(as.numeric(position_colony_chosen$lat)),
                  zoom = 3) %>%
          addMarkers(lng = as.numeric(position_colony_chosen$lon),
                     lat = as.numeric(position_colony_chosen$lat),
                     icon = makeIcon("./www/home.png", iconWidth = 18, iconHeight = 18))
      }

      m
      
      #print("----------TRAJECTORIES -------------------------")
      #print(paste("len_all_traj_metadata ",length(metadata_chosen$archipelago)))
      i = 1
      for(t in 1:length(metadata_chosen$archipelago)){ # all the trips are in all_traj_metadata[[1]] -> [[2]] and + are metadata

        #print(paste("t ",t))
        #print(metadata_chosen$file_formatted_by_Marie[t])

        data <- read.csv(paste("./data",as.character(metadata_chosen$file_formatted_by_Marie[t]),sep="/"),sep=',',dec='.')
        #print(data$lon[1])
        
        # print("factor color")
        # print(factor_color)

        #print(data$lon[1:length(data$lon)])
        #print(data$lat[1:length(data$lon)])
        m <- addPolylines(m,
                          lng=data$lon[1:length(data$lon)],
                          lat=data$lat[1:length(data$lat)],
                          color=col_final[t],
                          popup = metadata_chosen$unique_name[t])
        
        cat(i, " out of ", nrow(metadata_chosen), "\n")
        updateProgressBar(
          session = session,
          id = "map_progress",
          value = i,
          total = length(metadata_chosen$archipelago)
        )
        i = i+1

      }
      closeSweetAlert(session = session)
      m
      
    })
    
  })
  
  
}

### ----- IV. SHINY APP ----------------

shinyApp(ui = ui, server = server)