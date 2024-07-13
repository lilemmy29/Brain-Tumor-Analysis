## Load the libraries 
library(shiny)
library(plotly)
library(DT)
library(dplyr)

## Create the UI file 
ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("analysis", "Select Insights:",
                    choices = c("Age Distribution",
                                "Gender Distribution",
                                "Tumor Location Distribution",
                                "Gender-Based Distribution of Tumor Location",
                                "Tumor Treatment Distribution by Type",
                                "Tumor Treatment Distribution by Outcome",
                                "Average Survival Month by Treatment"
                                )
        
      ),
      actionButton("update", "UPDATE VIEW")
    ),
    mainPanel(
      plotlyOutput("plot"),
      DTOutput("table")
    )
    )
  )
)

server <- shinyServer(
  function(input, output) {
    data <- read.csv("C:\\Users\\user\\Desktop\\MouseWithoutBorders\\BrainTumor.csv") # Read the analysis from the specified file path
    
    observeEvent(input$update, { # Update the view when the button is clicked
      if(input$analysis == "Age Distribution") {
        # Create a histogram for age distribution
        
        age_distro <- plot_ly(data, x = ~Age, type = "histogram", width = 500, height = 400)
        age_distro <- age_distro %>% 
          layout(
            title = "Age Distribution",
            xaxis = list(title = "Age"),
            yaxis = list(title = "Frequency")
          )
        output$plot <- renderPlotly({ age_distro }) # Render the histogram
        output$table <- renderDT({ datatable(summary(data$Age)) }) # Render the summary of age data
      } else if(input$analysis == "Gender Distribution") {
        # Create a bar chart for gender distribution
        gender_count <- table(data$Gender)
        gender_count <- as.data.frame(gender_count)
        colnames(gender_count) <- c("Gender", "Count")
        gender_distro <- plot_ly(gender_count, x = ~Gender, y = ~Count, type = 'bar', width = 500, height = 200) %>%
          layout(
            title = "Gender Distribution",
            xaxis = list(title = "Gender"),
            yaxis = list(title = "Count")
          )
        output$plot <- renderPlotly({ gender_distro }) # Render the bar chart
        output$table <- renderDT({ datatable(gender_count) }) # Render the gender count data
      } else if(input$analysis == "Tumor Location Distribution") {
        # Create a bar chart for tumor location distribution
        location_count <- table(data$Tumor.Location)
        location_df <- as.data.frame(location_count)
        colnames(location_df) <- c("Location", "Count")
        location_fig <- plot_ly(location_df, x = ~Location, y = ~Count, type = "bar", width = 500, height = 300) %>% 
          layout(
            title = "Tumor Location Distribution",
            xaxis = list(title = "Tumor Location"),
            yaxis = list(title = "Count")
          )
        output$plot <- renderPlotly({ location_fig }) # Render the bar chart
        output$table <- renderDT({ datatable(location_df) }) # Render the location count data
      } else if(input$analysis == "Gender-Based Distribution of Tumor Location") {
        # Create a bar chart for tumor location distribution by gender
        location_gender_count <- table(data$Tumor.Location, data$Gender)
        location_gender_df <- as.data.frame(location_gender_count)
        colnames(location_gender_df) <- c("Location", "Gender", "Count")
        location_gender_fig <- plot_ly(location_gender_df, x = ~Location, y = ~Count, color = ~Gender, type = "bar", width = 500, height = 300) %>% 
          layout(
            title = "Tumor Location Distribution by Gender",
            xaxis = list(title = "Tumor Location"),
            yaxis = list(title = "Count")
          )
        output$plot <- renderPlotly({ location_gender_fig }) # Render the bar chart
        output$table <- renderDT({ datatable(location_gender_df) }) # Render the location by gender count data
      } else if(input$analysis == "Tumor Treatment Distribution by Type") {
        # Create a bar chart for tumor treatment distribution by type
        treatment_type_count <- table(data$Tumor.Type, data$Treatment)
        treatment_type_df <- as.data.frame(treatment_type_count)
        colnames(treatment_type_df) <- c("Type", "Treatment", "Count")
        treatment_type_fig <- plot_ly(treatment_type_df, x = ~Treatment, y = ~Count, color = ~Type, type = "bar", width = 700, height = 300) %>% 
          layout(
            title = "Tumor Treatment Distribution by Type",
            xaxis = list(title = "Tumor Treatment"),
            yaxis = list(title = "Count")
          )
        output$plot <- renderPlotly({ treatment_type_fig }) # Render the bar chart
        output$table <- renderDT({ datatable(treatment_type_df) }) # Render the treatment type count data
      } else if(input$analysis == "Tumor Treatment Distribution by Outcome") {
        # Create a bar chart for tumor treatment distribution by outcome
        treatment_outcome_count <- table(data$Treatment.Outcome, data$Treatment)
        treatment_outcome_df <- as.data.frame(treatment_outcome_count)
        colnames(treatment_outcome_df) <- c("Outcome", "Treatment", "Count")
        treatment_outcome_fig <- plot_ly(treatment_outcome_df, x = ~Treatment, y = ~Count, color = ~Outcome, type = "bar", width = 700, height = 300) %>% 
          layout(
            title = "Tumor Treatment Distribution by Outcome",
            xaxis = list(title = "Tumor Treatment"),
            yaxis = list(title = "Count")
          )
        output$plot <- renderPlotly({ treatment_outcome_fig }) # Render the bar chart
        output$table <- renderDT({ datatable(treatment_outcome_df) }) # Render the treatment outcome count data
      } else if(input$analysis == "Average Survival Month by Treatment") {
        # Create a bar chart for average survival months by treatment
        average_survival_treatment <- data %>% 
          group_by(Treatment) %>% 
          summarize(average_survival = round(mean(data$Survival.Time..months., na.rm = TRUE)))
        survival_average_df <- as.data.frame(average_survival_treatment)
        colnames(survival_average_df) <- c("Treatment", "Average Survival Month")
        survival_average_fig <- plot_ly(survival_average_df, x = ~Treatment, y = ~`Average Survival Month`, type = "bar", width = 700, height = 300) %>% 
          layout(
            title = "Average Survival Months by Treatment",
            xaxis = list(title = "Tumor Treatment"),
            yaxis = list(title = "Average Survival Months")
          )
        output$plot <- renderPlotly({ survival_average_fig }) # Render the bar chart
        output$table <- renderDT({ datatable(survival_average_df) }) # Render the average survival months data
      }
    })
  }
  
  
  
)


shinyApp(ui = ui, server = server)
  

    
    
    