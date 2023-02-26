#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(maps)
shp <- read_sf("data/county_data.shp") %>%
  mutate(state = str_to_title(state)) %>%
  mutate(white = as.numeric(white)) %>%
  mutate(black = as.numeric(black)) %>%
  mutate(hispanic = as.numeric(hispanic)) %>%
  mutate(asian = as.numeric(asian))
state_list <- unique(shp$state)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("County Demographic Map By State"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          helpText("Create demographic maps with 
        information from the 2010 US Census."),
        selectInput("state", 
                    label = "Choose a state to display",
                    choices = state_list,
                    selected = "Illinois"),
        selectInput("var", 
                    label = "Choose a variable to display",
                    choices = c("Percent White", "Percent Black",
                                "Percent Hispanic", "Percent Asian"),
                    selected = "Percent White")
        ),
        mainPanel(
           plotOutput("map")
        )
      )
      )
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$map <- renderPlot({
      new_shp <- shp %>% 
        filter(state == input$state)
        state_list <- unique(shp$state)
        state <- switch(input$state, state_list)
        var <- switch(input$var,
                      "Percent White" = new_shp$white,
                      "Percent Black" = new_shp$black,
                      "Percent Hispanic" = new_shp$hispanic,
                      "Percent Asian" = new_shp$asian)
        color <- switch(input$var, 
                        "Percent White" = "darkgreen",
                        "Percent Black" = "black",
                        "Percent Hispanic" = "darkorange",
                        "Percent Asian" = "darkviolet")
        legend <- switch(input$var, 
                          "Percent White" = "Percent White",
                          "Percent Black" = "Percent Black",
                          "Percent Hispanic" = "Percent Hispanic",
                          "Percent Asian" = "Percent Asian")
        # draw the histogram with the specified number of bins
        percent_map <- ggplot(new_shp) +
          geom_sf(aes(fill = var)) +
          labs(title=input$state, x="", y="") +
          theme(axis.text=element_blank(),
                axis.ticks=element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank() 
          ) + scale_fill_gradientn(legend,
                                   colors = c("white", color),
                                   limits = c(0, 100)
          )
         percent_map
    })
}
# Run the application 
shinyApp(ui = ui, server = server)

runGitHub("2010-County-Demographics", "jdunbar0803")
