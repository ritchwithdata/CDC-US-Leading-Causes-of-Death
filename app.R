library(shiny)  # Load the shiny package
library(bslib)  # Load the bslib package for Bootstrap themes
library(readr)  # Load the readr package for reading CSV files
library(ggplot2)  # Load the ggplot2 package for plotting
library(dplyr)  # Load the dplyr package for data manipulation

# # Load the dataset
lead_death_us <- read_csv("NCHS_Leading_Causes_of_Death__United_States.csv")
lead_death_us <- lead_death_us[-2]
lead_death_us <- lead_death_us %>% filter(Year >= 2008)

# Define UI for NCHS Leading Cause of Death Data----
ui <- page_sidebar(
  
  # App title ----
  title = "Leading Cause of Death US by State",  # Set the title of the app
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Selector for Year ----
    selectInput(
      "year",  # Input ID
      "Year:",  # Label for the input
      sort(unique(lead_death_us$Year), decreasing = TRUE)  # Names of values in Year Variable sorted alphabetically
    ),
    
    selectInput(
      "state",  # Input ID
      "State:",  # Label for the input
      sort(unique(lead_death_us$State))  # Names of values in Cause Name Variable sorted alphabetically
    ),
    
    # Input: Selector for variable to plot against mpg ----
    selectInput(
      "Cause Name",  # Input ID
      "Cause Name:",  # Label for the input
      sort(unique(lead_death_us$`Cause Name`))  # Names of values in Cause Name Variable sorted alphabetically
    )
    
  ),
  
  # Output: Formatted text for caption ----
  h4("Cause of Death and Death Rate by State"),  # New subheading added here
  
  # Output: Plots side by side ----
  fluidRow(  # Use fluidRow to arrange outputs side by side
    column(6, plotOutput("barchart")), # Display bar chart in the second column
    column(6, plotOutput("trendline"))  # Display trend line in the first column
  ),
  
  fluidRow(  # Use fluidRow to arrange outputs side by side
    column(6, h4("State Deaths by Cause")), 
    column(6, h4("State Deaths Over Time")) 
  ),
  
  # Output: Table of the summary statistics ----
  fluidRow(  # Use fluidRow to arrange outputs side by side
    column(6, tableOutput("leadcauseTable")), # Display bar chart in the second column
    column(6, tableOutput("stateTable"))  # Display trend line in the first column
  ),
)

  # Define server logic ----
server <- function(input, output) {
  # Filtered data based on Cause Name and Year
  filtered_data1 <- reactive({
    lead_death_us %>%
      filter(State == input$state, Year == input$year)  # Use input$year
  })
  
  # Filtered data based on Cause Name 
  filtered_data2 <- reactive({
    lead_death_us %>%
      filter(`Cause Name` == input$`Cause Name`, State == input$state)
  })
  
  # Box plot of Deaths by Cause Name
  output$barchart <- renderPlot({
    filtered_data1() %>%  # Use the filtered data
      group_by(`Cause Name`) %>%
      filter(`Cause Name` != "All causes") %>%  # Exclude "All Causes"
      summarise(Total_Deaths = sum(Deaths, na.rm = TRUE)) %>%  # Summarize total deaths
      arrange(desc(Total_Deaths)) %>%  # Order by total deaths descending
      ggplot(aes(x = reorder(`Cause Name`, Total_Deaths), y = Total_Deaths, fill = `Cause Name`)) +  # Add fill aesthetic
      geom_bar(stat = "identity") +  # Use stat = "identity" for pre-summarized data      ggplot(aes(x = reorder(`Cause Name`, Total_Deaths), y = Total_Deaths, fill = `Cause Name`)) +  # Add fill aesthetic
      guides(fill = "none") +  # Remove legend for fill aesthetic
      geom_text(aes(label = Total_Deaths), hjust = -0.2) +  # Add labels above points
      theme_minimal() +
      labs(title = "Deaths by Cause", x = "Cause Name", y = "Total Deaths") +
      coord_flip()  # Optional: flip coordinates for better readability
  })
  
  # Trend line of Age-adjusted Death over Year
  output$trendline <- renderPlot({
    data <- filtered_data2() %>%
      filter(`Cause Name` == input$`Cause Name`) %>%
      group_by(Year) %>%
      summarise(Aggregated_Rate = mean(`Age-adjusted Death Rate`, na.rm = TRUE))
    
    # Calculate limits
    y_min <- min(data$Aggregated_Rate) * 0.90
    y_max <- max(data$Aggregated_Rate) * 1.10
    
    ggplot(data, aes(x = Year, y = Aggregated_Rate)) +  # Change color based on Cause Name
      geom_line() +  # Continuous line
      geom_point() +  # Points at each year
      geom_text(aes(label = round(Aggregated_Rate, 2)), vjust = -0.5) +  # Add labels above points
      ylim(y_min, y_max) +  # Set y-axis limits
      theme_minimal() +
      labs(title = "Age-adjusted Death Rate over Year (per 10,000)", x = "Year", y = "Age-adjusted Death Rate (per 10,000)")
  })

  # Table of aggregated deaths by Cause Name and Year
  output$leadcauseTable <- renderTable({
    filtered_data1() %>%
      arrange(desc(Deaths)) 
  })
  
  # Table of aggregated deaths by Cause Name and Year
  output$stateTable <- renderTable({
    filtered_data2() %>%
      arrange(desc(Year))
  })
}

# Create Shiny app ----
shinyApp(ui, server)
