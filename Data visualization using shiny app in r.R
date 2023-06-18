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
  titlePanel("Boundaries"),
  
  wellPanel(
    # Create Radio Buttons to select region
    radioButtons(
      inputId = "Team",
      label = "Select Team:",
      choices = c(
        "Sunrisers Hyderabad", 
        "Mumbai Indians",
        "Royal Challengers Bangalore",
        "Chennai Super Kings",
        "Kolkata Knight Riders",
        "Kings XI Punjab",
        "Delhi Capitals",
        "Rajasthan Royals"
      ),
      inline = TRUE
    ),
    
    # Create Radio Buttons to select whether to show line or bar graph
    radioButtons(
      inputId = "Boundary",
      label = "Select Boundary:",
      choices = c(
        "Four", 
        "Six"
      ),
      inline = TRUE
    )
  ),
  
  # Display the visualization
  plotOutput("visualization")
)

server <- function(input, output){
  output$visualization <- renderPlot({
    # Determine the region and filter data for years 2005-2015 for that region
    Team <- input$Team
    names(deliveries.df)[1] <- "id"
    team.fours <- deliveries.df %>% 
      select(id,batsman,batting_team,batsman_runs) %>% 
      group_by(id) %>% 
      filter(batting_team==Team)
    id.match <- matches.df %>% 
      select(id, season)
    id.match.season <- inner_join(team.fours, id.match, by="id")

    
    # Determine which type of graph to display
    if (input$Boundary == "Four"){
      
      output.plot <- id.match.season %>% 
        filter(batsman_runs==4) %>% 
        count(season, batting_team) %>% 
        ggplot()+
        geom_col(
          mapping = aes(x=as.factor(season), y=n)
        )
    }else{
      output.plot <- id.match.season %>% 
        filter(batsman_runs==6) %>% 
        count(season, batting_team) %>% 
        ggplot()+
        geom_col(
          mapping = aes(x=as.factor(season), y=n)
        )
    }
    
    output.plot
  })
}

# Call to shinyApp function
shinyApp(ui = ui, server = server)

