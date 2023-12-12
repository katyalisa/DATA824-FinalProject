
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)

# Load your data here
election_data <- read.csv("election2019.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Election Data Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectMetric", "Select Metric", 
                  choices = c("Votes" = "VOTES", "Seats" = "SEATS"),
                  selected = "Votes"),
      selectInput("selectParty", "Select Party", 
                  choices = unique(election_data$CANDIDATES.2019), 
                  selected = unique(election_data$CANDIDATES.2019)[1]),
      selectInput("selectYear", "Select Year", 
                  choices = c("2016", "2019"),
                  selected = "2019")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bar Chart", plotOutput("barChart")),
        tabPanel("Pie Chart", plotOutput("pieChart")),
        tabPanel("Line Graph", plotOutput("lineGraph")),
        tabPanel("Descriptive Statistics", verbatimTextOutput("statsOutput")),
        tabPanel("Heat Map", plotOutput("heatMap"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$barChart <- renderPlot({
    # Convert SEATS.2016 to integer
    election_data$SEATS.2016 <- as.integer(election_data$SEATS.2016)
    
    # Reshape the data to compare seats in 2016 and 2019
    long_data <- election_data %>%
      select(CANDIDATES.2019, SEATS.2016, SEATS.2019) %>%
      pivot_longer(cols = c("SEATS.2016", "SEATS.2019"), 
                   names_to = "Year", 
                   values_to = "Seats") %>%
      mutate(Year = sub("SEATS.", "", Year))
    
    # Create a bar chart
    ggplot(long_data, aes(x=CANDIDATES.2019, y=Seats, fill=Year)) +
      geom_bar(stat="identity", position=position_dodge()) +
      theme_minimal() +
      labs(title="Seats Won by Party in 2016 vs 2019", x="Party", y="Seats", fill="Year")
  })
  
  output$pieChart <- renderPlot({
    # Filter data based on selected year
    year_col <- paste("VOTES.PERCENT.", input$selectYear, sep="")
    data_filtered <- election_data[, c("CANDIDATES.2019", year_col)]
    
    # Create a pie chart
    ggplot(data_filtered, aes(x="", y=!!as.name(year_col), fill=CANDIDATES.2019)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start=0) +
      theme_void() +
      labs(fill="Party", title=paste("Vote Percentage Distribution in", input$selectYear))
  })
  
  output$lineGraph <- renderPlot({
    # Create a line graph comparing seats in 2016 and 2019
    ggplot(election_data, aes(x=CANDIDATES.2019)) +
      geom_line(aes(y=SEATS.2016, group=1, colour="2016")) +
      geom_line(aes(y=SEATS.2019, group=1, colour="2019")) +
      theme_minimal() +
      labs(title="Seats Comparison Over Years", x="Party", y="Seats", color="Year")
  })
  output$statsOutput <- renderText({
    # Select the year based on user input
    year_suffix <- ifelse(input$selectYear == "2016", "2016", "2019")
    seats_col <- paste("SEATS.", year_suffix, sep="")
    votes_col <- paste("VOTES.", year_suffix, sep="")
    
    # Calculate descriptive statistics for each party
    stats <- election_data %>%
      group_by(CANDIDATES.2019) %>%
      summarise(
        Seats_Mean = mean(!!as.name(seats_col), na.rm = TRUE),
        Seats_Median = median(!!as.name(seats_col), na.rm = TRUE),
        Seats_SD = sd(!!as.name(seats_col), na.rm = TRUE),
        Seats_Min = min(!!as.name(seats_col), na.rm = TRUE),
        Seats_Max = max(!!as.name(seats_col), na.rm = TRUE),
        Votes_Mean = mean(!!as.name(votes_col), na.rm = TRUE),
        Votes_Median = median(!!as.name(votes_col), na.rm = TRUE),
        Votes_SD = sd(!!as.name(votes_col), na.rm = TRUE),
        Votes_Min = min(!!as.name(votes_col), na.rm = TRUE),
        Votes_Max = max(!!as.name(votes_col), na.rm = TRUE)
      )
    
    # Convert the stats to a readable format
    stats_text <- paste("Descriptive Statistics for the Year ", input$selectYear, ":\n\n", sep="")
    for(i in 1:nrow(stats)) {
      stats_text <- paste(stats_text, stats$CANDIDATES.2019[i], ":\n",
                          "Seats - Mean: ", stats$Seats_Mean[i], 
                          ", Median: ", stats$Seats_Median[i], 
                          ", SD: ", stats$Seats_SD[i], 
                          ", Min: ", stats$Seats_Min[i], 
                          ", Max: ", stats$Seats_Max[i], "\n",
                          "Votes - Mean: ", stats$Votes_Mean[i], 
                          ", Median: ", stats$Votes_Median[i], 
                          ", SD: ", stats$Votes_SD[i], 
                          ", Min: ", stats$Votes_Min[i], 
                          ", Max: ", stats$Votes_Max[i], "\n\n")
    }
    stats_text
  })
  output$heatMap <- renderPlot({
    # Convert the seats and votes columns to integers
    election_data$SEATS.2016 <- as.integer(election_data$SEATS.2016)
    election_data$SEATS.2019 <- as.integer(election_data$SEATS.2019)
    election_data$VOTES.2016 <- as.integer(election_data$VOTES.2016)
    election_data$VOTES.2019 <- as.integer(election_data$VOTES.2019)
    
    # Prepare the data for the heat map
    metric_2016_col <- paste(input$selectMetric, ".2016", sep="")
    metric_2019_col <- paste(input$selectMetric, ".2019", sep="")
    heat_data <- election_data %>%
      select(CANDIDATES.2019, !!as.name(metric_2016_col), !!as.name(metric_2019_col)) %>%
      pivot_longer(cols = c(!!as.name(metric_2016_col), !!as.name(metric_2019_col)), 
                   names_to = "Year", 
                   values_to = "Value") %>%
      mutate(Year = sub("VOTES.", "", Year),
             Year = sub("SEATS.", "", Year))
    
    # Create a heat map
    ggplot(heat_data, aes(x=CANDIDATES.2019, y=Year, fill=Value)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "white", high = "darkred") +
      labs(title = paste(input$selectMetric, "Heat Map"), 
           x = "Party", y = "Year", fill = input$selectMetric) +
      theme_minimal() +
      theme(axis.title.y = element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
