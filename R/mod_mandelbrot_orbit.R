#' mandelbrot_orbit UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mandelbrot_orbit_ui <- function(id){
  # QUESTION - what is noRd above
  ns <- NS(id)
  # QUESTION why is this tagList here?
  tagList(
    fluidRow(
      column(
        width = 3,
        h1("z(n+1) = z(n)^2 + c"),
        br(),
        # QUESTION - how would you do this neater?
        sliderInput(inputId=ns('x_value'), label='Real part of c', value = ifelse(is.null(golem::get_golem_options('x_start')),0.35,golem::get_golem_options('x_start')), min=-1, max=1, step=0.01, animate=TRUE, width = '100%'),
        sliderInput(inputId=ns('y_value'), label='Imaginary part of c', value = ifelse(is.null(golem::get_golem_options('x_start')),0.35,golem::get_golem_options('x_start')), min=-1, max=1, step=0.01, animate=TRUE, width = '100%'),
        sliderInput(inputId=ns('num_iterations'), label='Number of iterations', value = 50, min=5, max=100, step=5, animate = TRUE, width = '100%'),
        br(),
        actionButton(inputId = ns('c_random'), label = 'Random value of c', width = '100%'),
        br(),
        br(),
        actionButton(inputId = ns('c1'), label = 'c = -0.12 + 0.75i - Period 3', width = '100%'),
        actionButton(inputId = ns('c2'), label = 'c = +0.28 + 0.54i - Period 4', width = '100%'),
        actionButton(inputId = ns('c3'), label = 'c = -0.50 + 0.56i - Period 5', width = '100%'),
        actionButton(inputId = ns('c4'), label = 'c = +0.39 + 0.22i - Period 6', width = '100%'),
        actionButton(inputId = ns('c5'), label = 'c = -0.62 + 0.43i - Period 7', width = '100%'),
        actionButton(inputId = ns('c6'), label = 'c = -0.36 + 0.62i - Period 8', width = '100%'),
        actionButton(inputId = ns('c7'), label = 'c = -0.67 + 0.34i - Period 9', width = '100%'),
      ),
      column(
        width = 9,
        br(),
        plotly::plotlyOutput(ns('plot'))
      )
    )
  )
}

#' mandelbrot_orbit Server Functions
#'
#' @noRd
mod_mandelbrot_orbit_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns # QUESTION WHY IS THIS NEEDED
    plot_values <- reactive({
      orbit_values(
        input$x_value,
        input$y_value,
        input$num_iterations
      )
    })

    output$plot <- plotly::renderPlotly({
      p <- plotly::plot_ly(plot_values(), x=~Real, y=~Imaginary, type='scatter', mode='markers', width = 800, height = 800) |>
        plotly::add_trace(plot_values(), x=~Real, y=~Imaginary, type='scatter', mode='lines') |>
        plotly::layout(xaxis = list(range=c(-1,1))) |>
        plotly::layout(yaxis = list(range=c(-1,1))) |>
        plotly::layout(showlegend = FALSE)
      p
      })
    observeEvent(input$c_random,{
      updateSliderInput(inputId = 'x_value', value = sample(-100:100,1)/100)
      updateSliderInput(inputId = 'y_value', value = sample(-100:100,1)/100)
    })
    observeEvent(input$c1,{
      updateSliderInput(inputId = 'x_value', value = -0.12)
      updateSliderInput(inputId = 'y_value', value = +0.75)
    })
    observeEvent(input$c2,{
      updateSliderInput(inputId = 'x_value', value = +0.28)
      updateSliderInput(inputId = 'y_value', value = +0.54)
    })
    observeEvent(input$c3,{
      updateSliderInput(inputId = 'x_value', value = -0.50)
      updateSliderInput(inputId = 'y_value', value = +0.56)
    })
    observeEvent(input$c4,{
      updateSliderInput(inputId = 'x_value', value = +0.39)
      updateSliderInput(inputId = 'y_value', value = +0.22)
    })
    observeEvent(input$c5,{
      updateSliderInput(inputId = 'x_value', value = -0.62)
      updateSliderInput(inputId = 'y_value', value = +0.43)
    })
    observeEvent(input$c5,{
      updateSliderInput(inputId = 'x_value', value = -0.62)
      updateSliderInput(inputId = 'y_value', value = +0.43)
    })
    observeEvent(input$c6,{
      updateSliderInput(inputId = 'x_value', value = -0.36)
      updateSliderInput(inputId = 'y_value', value = +0.62)
    })
    observeEvent(input$c7,{
      updateSliderInput(inputId = 'x_value', value = -0.67)
      updateSliderInput(inputId = 'y_value', value = +0.34)
    })
  })
}

# QUESTION - I have a function here that's not shiny related - where should I put it?
# QUESTION - can I make the Roxygen documentation appear automatically for a function?
orbit_values <- function(x_start,y_start,num_iterations){
  if(all(!is.null(x_start),!is.null(y_start),!is.null(num_iterations))){
    mtx <- matrix(data=NA,nrow=num_iterations,ncol=2)
    mtx[1,1] <- x_start
    mtx[1,2] <- y_start
    for(i in 2:num_iterations){
      mtx[i,1] <- mtx[i-1,1]^2 - mtx[i-1,2]^2 + x_start
      mtx[i,2] <- 2 * mtx[i-1,1] * mtx[i-1,2] + y_start
      if((mtx[i,1]^2+mtx[i,2]^2)>10) break
    }
    mtx <- data.table::as.data.table(mtx)
    data.table::setnames(mtx, c('Real','Imaginary'))
  }
}
## To be copied in the UI
# mod_mandelbrot_orbit_ui("mandelbrot_orbit_1")

## To be copied in the server
# mod_mandelbrot_orbit_server("mandelbrot_orbit_1")
