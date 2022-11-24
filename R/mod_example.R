#' example UI Function
#'
#' @description A small {shiny} app that demonstrates the code and usage for the 
#' functions in ottoPlots. 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_example_ui <- function(id){
  ns <- shiny::NS(id)
  
  ## sidebar panel--------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("hist_variable"), 
        label = "Select variable", 
        choices = NULL, 
        selected = NULL, 
        multiple = FALSE
      ), 
      sliderInput(
        inputId = ns("num_bins"), 
        label = "Select number of bins", 
        min = 10, 
        max = 100,
        value = 30
      )
    ), 
    
    ## main panel --------------------------------------------------------------
    mainPanel(
      fluidRow(
        column(
          width = 6, 
          plotOutput(ns("hist_gg")), 
          mod_download_figure_ui(
            ns("dl_hist_gg") 
          )
        ), 
        column(
          width = 6, 
          plotOutput(ns("hist_r")),
          mod_download_figure_ui(
            ns("dl_hist_r")
          )
        )
      )
    )
  )
}
    
#' example Server Functions
#'
#' @noRd 
#' @importFrom graphics hist
mod_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    iris <- datasets::iris
    
    ## update select input -----------------------------------------------------
    observe({
      updateSelectInput(
        session = session, 
        inputId = "hist_variable", 
        choices = colnames(iris)[1:4], 
        selected = colnames(iris)[1]
      )
      
      ## histograms ------------------------------------------------------------
      
      #### ggplot histogram ------
      hist_gg <- reactive({
        req(input$hist_variable) 
        ggplot2::ggplot(
          data = iris, 
          ggplot2::aes_string(x = input$hist_variable)
        ) + 
          ggplot2::geom_histogram(bins = input$num_bins + 1) + 
          ggplot2::labs(title = "ggplot_histogram") + 
          ggplot2::theme_linedraw()
      })
      
      output$hist_gg <- renderPlot({ print(hist_gg()) })
      
      dl_hist_gg <- mod_download_figure_server(
        id = "dl_hist_gg", 
        filename = "histogram_ggplot", 
        figure = reactive({ hist_gg() })
      )
      
      #### base r histogram ------
      hist_r <- reactive({
        req(input$hist_variable)
        hist(
          iris[, c(input$hist_variable)], 
          xlab = input$hist_variable, 
          main = "Base R histogram", 
          breaks = input$num_bins
        )
        p <- recordPlot()
        return(p)
      })
      
      output$hist_r <- renderPlot({ print(hist_r()) })
      
      dl_hist_r <- mod_download_figure_server(
        id = "dl_hist_r", 
        filename = "histogram_r", 
        figure = reactive({hist_r()})
      )
    })
  })
}
    