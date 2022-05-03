#' download_figure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_figure_ui <- function(id, label = "Download Plot"){
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("download_popup"), 
      label = label
    )
  )
}
    
#' download_figure Server Functions
#'
#' @noRd 
mod_download_figure_server <- function(
    id, 
    filename, 
    figure, 
    width = 8, 
    height = 6, 
    ggplot = TRUE
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    min_size <- 2 # min for width or height
    max_size <- 30 # max for width or height
    
    #Check width
    figure_width <- reactive({
      if (is.numeric(input$width)) {
        # cutoff the input value to a range as shiny does not enforce
        return(max(min_size, min(max_size, input$width, na.rm = TRUE)))
      } else {
        # use default value when entered text
        return(width)
      }
    })
    
    #Check height
    figure_height <- reactive({
      if (is.numeric(input$width)) {
        # cutoff the input value to a range as shiny does not enforce
        return(max(min_size, min(max_size, input$height, na.rm = TRUE)))
      } else {
        # use default value when entered text
        return(height)
      }
    })
    
   
    observeEvent(
      input$download_popup,
      {
        showModal(
          modalDialog(
            numericInput(               #Figure width
              inputId = ns("width"),
              label = "Width (in)",
              value = width,
              min = min_size,
              max = max_size
            ),
            numericInput(               #Figure Height
              inputId = ns("height"),
              label = "Height (in)",
              value = height,
              min = min_size,
              max = max_size
            ),
            h5("The plot will be rendered differently depending on size. 
            When the dimensions are too small, error or blank plot
               will be generated."
            ),
            downloadButton(             #buttons
              outputId = ns("dl_pdf"),
              label = "PDF"
            ),
            downloadButton(
              outputId = ns("dl_png"),
              label = "PNG"
            ),
            downloadButton(
              outputId = ns("dl_svg"),
              label = "SVG"
            ),
            size = "s" 
          )
        )
      }
    )
    
    # Download PDF
    output$dl_pdf <- downloadHandler(
      filename = paste0(filename, ".pdf"),
      content = function(file) {
        
        on.exit(removeModal())
        pdf(
          file,
          width = figure_width(),
          height = figure_height()
        )
        if (ggplot == TRUE){
          print(figure())
        } else {
          figure()
        }
        dev.off()
      }
    )
    
    # Download PNG
    output$dl_png <- downloadHandler(
      filename = paste0(filename, ".png"),
      content = function(file) {
        
        on.exit(removeModal())
        png(
          file,
          res = 360,
          width = figure_width(),
          height = figure_height(),
          units = "in"
        )
        if (ggplot == TRUE){
          print(figure())
        } else {
          figure()
        }
        dev.off()
      }
    )
    
    #download SVG
    output$dl_svg <- downloadHandler(
      filename = paste0(filename, ".svg"),
      content = function(file) {
        
        on.exit(removeModal())
        svg(
          file,
          width = figure_width(),
          height = figure_height()
        )
        if (ggplot == TRUE){
          print(figure())
        } else {
          figure()
        }
        dev.off()
      }
    )
  })
}
    
## To be copied in the UI
# mod_download_figure_ui("download_figure_ui_1")
    
## To be copied in the server
# mod_download_figure_server("download_figure_ui_1")
