
library(shiny)
library(tidyverse)
library(tidymodels)
library(horus)

df <- read_rds("house_locations.rds")

ui <- fluidPage(
  titlePanel("Decision Boundary Visualizer"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(
        width = 12,
        br(),
        p("In 1973, Eldfell, a volcano on the island of Heimaey in Iceland erupted. 312 of the 1097 houses on the island were destroyed.
                                        This app shows the decision boundary drawn by a Polynomial support vector machine algorithm between the houses destroyed by the lava and those that were not destroyed.
                                        You can adjust the degree of the polynomial to better understand how SVMs work."),
        br()
      ),
      column(
        width = 12,
        p("The app makes use of"),
        a(href = "https://github.com/EmilHvitfeldt/horus", "Emil Hvitfeldt's {horus} package", target = "_blank"),
        br())
    ),
    sliderInput("degree",
      "Degree of polynomial:",
      min = 1,
      max = 9,
      value = 3
    )
  ),
  mainPanel(
    plotOutput("distPlot")
  )
)
)

server <- function(input, output) {

  output$distPlot <- renderPlot({
    workflow() %>%
      add_formula(destroyed ~ latitude + longitude) %>%
      add_model(svm_poly() %>%
        set_mode("classification") %>%
        set_args(degree = input$degree)) %>%
      fit(df) %>%
      viz_decision_boundary(., df) +
      coord_flip() +
      labs(
        y = "Longitude",
        x = "Latitude",
        fill = "Predicted class"
      )
  })
}

shinyApp(ui = ui, server = server)
