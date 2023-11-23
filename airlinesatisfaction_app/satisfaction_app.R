#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(plotly)
library(MetBrewer)
library(colorspace)

# UI
ui <- fluidPage(
    theme = shinytheme("cerulean"),  # Set the theme here
    
    titlePanel("Airline Satisfaction"),
    sidebarLayout(
        sidebarPanel(
            radioButtons("gender_selector", "Gender:",
                         choices = c("All", "Female", "Male"),
                         selected = "All"),
            radioButtons("customer_type_selector", "Customer Type:",
                         choices = c("All", "First-time", "Returning"),
                         selected = "All"),
            checkboxGroupInput("age_category_selector", "Age Categories:",
                               choices = levels(df$age_category))
        ),
        mainPanel(
            plotlyOutput("satisfaction_pie_chart")
        )
    )
)

# Server
server <- function(input, output) {
    # Filter data based on selected gender and age categories
    filtered_data <- reactive({
        df_selected <- df
        
        if (input$gender_selector != "All") {
            df_selected <- subset(df_selected, Gender == input$gender_selector)
        }
        
        if (input$customer_type_selector != "All") {
            df_selected <- subset(df_selected, Customer.Type == input$customer_type_selector)
        }
        
        if (!is.null(input$age_category_selector) && length(input$age_category_selector) > 0) {
            df_selected <- subset(df_selected, age_category %in% input$age_category_selector)
        }
        
        return(df_selected)
    })
    
    # Generate pie chart
    output$satisfaction_pie_chart <- renderPlotly({
        pie_chart_data <- table(filtered_data()$Satisfaction)
        pie_chart_labels <- c("Neutral or Unsatisfied", "Satisfied")
        
#        c1 <- met.brewer("Paquin")
        cust_colors <- c("#e8e8e8", "088bbd") # custom blue / grey values to match theme
        
        plot_ly(labels = ~pie_chart_labels, 
                values = ~pie_chart_data, 
                type = "pie",
        marker = list(colors = cust_colors)
        ) %>%
            layout(
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
                   )
    })
}

# Run the app
shinyApp(ui = ui, server = server)
