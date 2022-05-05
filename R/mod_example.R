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
        sliderInput(
          inputId = ns("num_bins"), 
          label = "Select number of bins", 
          min = 10, 
          max = 100,
          value = 30
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
      
      conditionalPanel(
        condition = "input.plot_type == 1",
        fluidRow(
          column(
            width = 6, 
            plotOutput(ns("hist_gg")), 
            mod_download_figure_ui(
              ns("dl_hist_gg"), 
              label = "Download ggplot"
            )
          ), 
          column(
            width = 6, 
            plotOutput(ns("hist_r")),
            mod_download_figure_ui(
              ns("dl_hist_r"), 
              label = "Download base R plot"
            )
          )
        ),
        ns = ns 
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 2", 
        fluidRow(
          column(
            width = 6, 
            plotOutput(ns("scatter_gg")), 
            mod_download_figure_ui(
              ns("dl_scatter_gg"), 
              label = "Download ggplot"
            )
          ), 
          column(
            width = 6, 
            plotOutput(ns("scatter_r")), 
            mod_download_figure_ui(
              ns("dl_scatter_r"), 
              label = "Download base R plot"
            )
          )
        ), 
        ns = ns
      )
      
    )
  )
}
    
#' example Server Functions
#'
#' @noRd 
mod_example_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    #--- update select input --- 
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
      
      #--- histograms ---- 
      hist_gg <- reactive({
        req(input$hist_variable) 
        ggplot2::ggplot(
          data = iris, 
          ggplot2::aes_string(x = input$hist_variable)
        ) + 
          ggplot2::geom_histogram(bins = input$num_bins + 1) + 
          ggplot2::labs(title = "ggplot_histogram")
      })
      output$hist_gg <- renderPlot({
        print(hist_gg())
      })
      dl_hist_gg <- mod_download_figure_server(
        "dl_hist_gg", 
        filename = "histogram_ggplot", 
        figure = reactive({ hist_gg() })
      )
      
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
      output$hist_r <- renderPlot({
        print(hist_r())
      })
      dl_hist_r <- mod_download_figure_server(
        "dl_hist_r", 
        filename = "histogram_r", 
        figure = reactive({hist_r()})
      )
      
      #--- scatterplots -----
      scatter_gg <- reactive({
        req(input$scatter_var1, input$scatter_var2)
        ggplot2::ggplot(
          data = iris, 
          ggplot2::aes_string(
            x = input$scatter_var1, 
            y = input$scatter_var2
          )
        ) + 
          ggplot2::geom_point() + 
          ggplot2::labs(title = "ggPlot scatterplot")
      })
      output$scatter_gg <- renderPlot({
        print(scatter_gg())
      })
      dl_scatter_gg <- mod_download_figure_server(
        "dl_scatter_gg", 
        filename = "scatter_ggplot", 
        figure = reactive({ scatter_gg() })
      )
      
      scatter_r <- reactive({
        req(input$scatter_var1, input$scatter_var2)
        plot(
          x = iris[ ,c(input$scatter_var1)],
          y = iris[ ,c(input$scatter_var2)], 
          xlab = input$scatter_var1, 
          ylab = input$scatter_var2, 
          main = "Base R scatterplot"
        )
        p <- recordPlot()
        return(p)
      })
      output$scatter_r <- renderPlot({
        print(scatter_r())
      })
      dl_scatter_r <- mod_download_figure_server(
        "dl_scatter_r", 
        filename = "scatter_rplot", 
        figure = reactive({ scatter_r() })
      )
      
    })
  })
}
    
## To be copied in the UI
# mod_example_ui("example_ui_1")
    
## To be copied in the server
# mod_example_server("example_ui_1")
