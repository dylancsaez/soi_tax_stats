library(shiny)
library(shinyMatrix)

ui <- fluidPage(
  titlePanel("shinyMatrix: Simple App"),
  sidebarPanel(
    width = 6,
    numericInput("rows", "Number of rows:", "", min = 2, max = 100),
    numericInput("columns", "Number of columns:", "", min = 2, max = 100),
  ),
  mainPanel(
    matrixInput("matrix1",
                value = matrix(NA, 2,2),
                rows = list(
                  names = TRUE,
                  editableNames = TRUE),
                cols = list(
                  names = TRUE,
                  editableNames = TRUE
                ),
                class = "numeric"),
    width = 6,
  )
)

server <- function(input, output, session) {
  observe({
    m <- matrix(0, nrow = input$rows, ncol = input$columns)
    updateMatrixInput(session, "matrix1", m)
  })
}
