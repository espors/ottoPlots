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
      ),
      conditionalPanel(
        condition = "input.plot_type == 1", 
        selectInput(
          inputId = ns("hist_variable"), 
          label = "Select variable", 
          choices = NULL, 
          selected = NULL, 
          multiple = FALSE
        ), 
        ns = ns
      ), 
      conditionalPanel(
        condition = "input.plot_type ==2", 
        selectInput(
          inputId = ns("scatter_var1"), 
          label = "Select x-axis variable", 
          choices = NULL, 
          selected = NULL, 
          multiple = FALSE
        ),
        selectInput(
          inputId = ns("scatter_var2"), 
          label = "Select y-axis variable", 
          choices = NULL, 
          selected = NULL, 
          multiple = FALSE
        ),
        ns = ns
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
    
    observe({
      updateSelectInput(
        session = session, 
        inputId = "hist_variable", 
        choices = colnames(iris)[1:4], 
        selected = colnames(iris)[1]
      )
      updateSelectInput(
        session = session, 
        inputId = "scatter_var1", 
        choices = colnames(iris)[1:4], 
        selected = colnames(iris)[1]
      )
      updateSelectInput(
        session = session, 
        inputId = "scatter_var2", 
        choices = colnames(iris)[1:4], 
        selected = colnames(iris)[2]
      )
    })
    
 
  })
}
    
## To be copied in the UI
# mod_example_ui("example_ui_1")
    
## To be copied in the server
# mod_example_server("example_ui_1")
