# this app is a minimal example of uploading a layout file using the layout uploads module
library(tidyverse)
library(shiny)
library(shinyalert) # needed to upload layout files
library(shinycssloaders) # needed for the spinner on the layout plots
library(varhandle) 
source("upload_layout_module.R")
source("layout_color_plot_module.R")

ui <- fluidPage(useShinyalert(),
                sidebarLayout(
                    sidebarPanel(width = 4,
                                 uploadLayoutUI("data")[[1]] # upload panel,

                    ),
                    mainPanel(
                        p("color plotting module"),
                        selectLayoutColorUI("color_var")[[1]], # select a color_by variable
                        selectLayoutColorUI("color_var")[[2]], # plate plot
                        p("Layout, accessed OUTSIDE the module pair (just the head)") %>% strong(),
                        tableOutput("table_external"),
                        p("Layout, rendered INSIDE the module pair") %>% strong(),
                        uploadLayoutUI("data")[[2]] # upload panel,
                    )
                )
)

server <- function(input, output, session) {
    #### upload layout module ####
    layout_raw <- uploadLayoutServer("data") # upload the data
    layout <- reactive(layout_raw()) # access the layout outside of the module
    output$table_external <- renderTable(head(layout()))
    
    
    ### color layout plot module ###
    layout_plot <- selectLayoutColorServer("color_var",
                                           data = layout_raw)
    
    
    
}


shinyApp(ui = ui, server = server)
