#' example UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_example_ui <- function(id){
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = ns("plot_type"), 
        label = "Choose plot type", 
        choices = list(
          "Histogram" = 1, 
          "Scatterplot" = 2
        ), 
        selected = 1
      )
    ), 
    mainPanel(
      
    )
  )
}
    
#' example Server Functions
#'
#' @noRd 
mod_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_example_ui("example_ui_1")
    
## To be copied in the server
# mod_example_server("example_ui_1")
