## Wiegetisch Preprocess Data
# shiny app to preprocess data following this steps:
#  - aggregate data (1000 Hz to defined timebreaks)
#  - inspect data and set maximum and minimum values
#  - clean data using running means
#  - interpolate missings (due to data cleaning)
#  - visual inspection and final export

#### DEBUG ONLY
#setwd("C:/Users/Juli/Desktop/Auswertung MA/wiegetisch_preprocess_data")
library(shiny)
####

# install and load packages
list.of.packages <- c("shinydashboard", "shinycssloaders", "shinybusy", "haven", "R.matlab", "stringr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library("shinydashboard")
library("shinycssloaders")
library("shinybusy")
library("haven") 
library("R.matlab")
library("stringr")


# load helper functions
source("helper_functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "Wiegetisch Preprocess data"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("README", tabName = "home"),
      menuItem("Load data", tabName = "load_data"),
      menuItem("Process data", tabName = "process_data"),
      menuItem("Export data", tabName = "export_data")
    )

    ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),

    tabItems(
##### load data
      tabItem(tabName = "load_data",
       fluidRow(
          column(12,
            ### Load Data
            box(title = "Load data", collapsible = TRUE, width = NULL,

              p("This step combines the dataset into one R-object and stores it in one binary file. This binary file can be loaded inside the 'Process data' tab to perform the data cleaning process. Additionaly some initial cleaning parameters, which can be changed later, are calculated and stored with the prefix 'paras_'."),
              p ("Put the raw '.txt' files in the ./raw_data directory. The files must contain the following columns: vp, time and columns with the names of the snack. The columns must be white-space separated, decimal character is a . and a header must be given."),
              p("!!! Please note that the loading of the data can take some time !!!"),
              textInput("snack_names", "Set column names of snacks: (comma separated)", 
                         value = "CCC, KSU, PME, BKS, BES, LBS"),
              textInput("name_dat_raw", "Set name of binary file:", 
                        value = paste0("dat_raw_", format(Sys.time(), "%Y-%m-%d_%H%M"), ".rds")),
              actionButton("start_loading", "Load '.txt' files"),
              br(), br(),
              withSpinner(verbatimTextOutput("inspect_loaded", placeholder = FALSE), type = 7, size = .5, proxy.height = 80)
            )

      ))),

##### process data
      tabItem(tabName = "process_data",
        fluidRow(
            box(title = "Choose data", collapsible = TRUE, width = 12,
              uiOutput("load_data"),
              verbatimTextOutput("inspect_loaded_rds"))
            
        ),
        fluidRow(
          
            ### Set Range
            box(title = "Process data", collapsible = TRUE, width = 12,

              # previous, next button and current vp and snack
              splitLayout(cellWidths = c("80px", "70px", "120px"),
                conditionalPanel(
                condition = "output.previous_show_button == true",
                actionButton("previous_range", "Previous")),
              conditionalPanel(
                condition = "output.next_show_button == true",
                actionButton("next_range", "Next")), 
              textOutput("track_status")),


            # manual control sliders (parameters)
              splitLayout(cellWidths = c("80%", "20%"), 
                            cellArgs = list(style = "padding: 6px"),
                 sliderInput("man_range", "Set meaningful range:",
    min = 0, max = 3300, value = c(100, 3000), step = .1),

                sliderInput("man_delete_first", "Remove first # points:",
    min = 0, max = 10, value = 0, step = 1)
                
                ),
              splitLayout(cellWidths = c("17%", "17%", "17%", "17%", "10%", "20%"), 
                            cellArgs = list(style = "padding: 6px"),
                sliderInput("man_first_lag", "First running mean (RM) lag:",
    min = 0, max = 30, value = 20, step = 1),
                sliderInput("man_first_error", "Error for first RM:",
    min = 0, max = 30, value = 20, step = 1),
                sliderInput("man_second_lag", "Second RM lag:",
    min = 0, max = 30, value = 10, step = 1),
                sliderInput("man_second_error", "Error for second RM:",
    min = 0, max = 30, value = 10, step = 1),
                # add point
                verbatimTextOutput("show_mouse"),
                column( width = 12, 
                conditionalPanel(
                 condition = "output.add_point_show_button == true",
                 actionButton("add_point", "Add point manually")
                ),
                actionButton("reset_points", "Reset all manual points")
                )
                ),

# plot data 2 x 2
              withSpinner(plotOutput("range_plot", click = "plot_click"), , type = 7, size = .5)


              )
 
           


        )
      ),

# export data
tabItem(tabName = "export_data",
       fluidRow(
          column(12,
            ### Load Data
            box(title = "Export data", collapsible = FALSE, width = NULL,
              p("If you have visually checked and, if necessary, adjusted all the cleaning parameters in the 'Process data' tab, you can choose the respective  data. Then click export and take a look inside the 'exported_data' folder. Have fun with your data analysis :D"),
              uiOutput("load_data_export"),
              textInput("add_info_name", "Add specific tags to exported .txt files (e.g. study name)"),
              actionButton("export_data_btn", "Export data")



              )

      ))

))))


