#' download_figure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_figure_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' download_figure Server Functions
#'
#' @noRd 
mod_download_figure_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_download_figure_ui("download_figure_ui_1")
    
## To be copied in the server
# mod_download_figure_server("download_figure_ui_1")
