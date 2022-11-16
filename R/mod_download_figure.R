#' download_figure UI Function
#'
#' @description The UI function of the {shiny} module that works with
#' \code{\link{mod_download_figure_server}()} to automatically download figures
#'  from within a {shiny} application. The UI function provides a "Download"
#'  button that results in a pop-up where users can enter plot height and width
#'  and options to download a plot as PDF, PNG, or SVG. This work for both
#'  {ggplot2} graphics and base R plots. Base R plots must be saved in an object
#'  with \code{\link{recordPlot}()}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param label Label for download button. Default is "Download Plot".
#' @return UI elements for download button.
#'
#' @export
#'
#' @importFrom shiny NS tagList
mod_download_figure_ui <- function(id, label = "Download Plot") {
  ns <- NS(id)
  tagList(
    actionButton(
      inputId = ns("download_popup"),
      label = label
    ),
    tippy::tippy_this(
      ns("download_popup"),
      "Click to download plot in preferred format and size."
    )
  )
}

#' download_figure Server Functions
#'
#' @description The server function of the {shiny} module that works with
#'  \code{\link{mod_download_figure_ui}()} to automatically download figures from
#'  within a {shiny} application. The server function handles the UI for the
#'  pop-up and code for downloading the plot as PDF, PNG, or SVG.  This works
#'  for both ggplot2 graphics and base R plots. Base R plots must be save in an
#'  object with \code{\link{recordPlot}()}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param filename String designating he name of file that plot should be saved
#'  as
#' @param figure Object containg the plot to be downloaded. Should be wrapped in
#'  a reactive function
#' @param width Width (inches) the plot should be saved as.
#'  Default is 8 in and must be a value between 2 and 30.
#' @param height Height (inches) the plot should be saved as.
#'  Default is 6 in and must be a value between 2 and 30.
#'
#' @export
#'
mod_download_figure_server <- function(id,
                                       filename,
                                       figure,
                                       width = 8,
                                       height = 6) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # set max and min sizes
    min_size <- 2
    max_size <- 30

    # Check width
    figure_width <- reactive({
      if (is.numeric(input$width)) {
        # cutoff the input value to a range as shiny does not enforce
        return(max(min_size, min(max_size, input$width, na.rm = TRUE)))
      } else {
        # use default value when entered text
        return(width)
      }
    })

    # Check height
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
            numericInput(
              inputId = ns("width"),
              label = "Width (in)",
              value = width,
              min = min_size,
              max = max_size
            ),
            numericInput(
              inputId = ns("height"),
              label = "Height (in)",
              value = height,
              min = min_size,
              max = max_size
            ),
            h5("The plot will be rendered differently depending on size.
            When the dimensions are too small, error or blank plot
               will be generated."),
            downloadButton(
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

    # Download PDF ----
    output$dl_pdf <- downloadHandler(
      filename = paste0(filename, ".pdf"),
      content = function(file) {
        on.exit(removeModal())
        pdf(
          file,
          width = figure_width(),
          height = figure_height()
        )
        print(figure())
        dev.off()
      }
    )

    # Download PNG ----
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
        print(figure())
        dev.off()
      }
    )

    # Download SVG ---
    output$dl_svg <- downloadHandler(
      filename = paste0(filename, ".svg"),
      content = function(file) {
        on.exit(removeModal())
        svg(
          file,
          width = figure_width(),
          height = figure_height()
        )
        print(figure())
        dev.off()
      }
    )
  })
}
