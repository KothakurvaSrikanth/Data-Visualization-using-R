# Load required packages
# shiny package is needed for Shiny Apps
library(shiny)
library(dplyr)
library(ggplot2)

deliveries.df <- read.csv("deliveries.csv", stringsAsFactors = TRUE)

matches.df <- read.csv("matches.csv", stringsAsFactors = TRUE)


# User Interface

ui <- fluidPage(
  # Title Panel
  titlePanel("Wins by Venue"),
  
  wellPanel(
    # Create Radio Buttons to select region
    selectInput(
      inputId = "Team",
      label = "Select Team:",
      choices = c(as.character(unique(matches.df$venue))
      )
    ),
    
    # Create Radio Buttons to select whether to show line or bar graph

  ),
  
  # Display the visualization
  plotOutput("visualization")
)

server <- function(input, output){
  output$visualization <- renderPlot({
    # Determine the region and filter data for years 2005-2015 for that region
    Team <- input$Team
    output.plot <- matches.df %>% 
      select(id, season,venue, winner) %>% 
      filter(venue==Team) %>% 
      group_by(venue) %>% 
      arrange(venue) %>% 
      count(winner) %>% 
      ggplot()+
      geom_col(
        mapping = aes(x=winner, y= n)
      )+
      ggtitle("Wins by a Team based on Location")+
      xlab("Team")+
      ylab("Number of Wins")+
      theme(axis.text.x = element_text(size = 13, angle = 60, hjust = 1))

    
    output.plot
  })
}

# Call to shinyApp function
shinyApp(ui = ui, server = server)

