
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(haven)
library(shinydashboard)
library(plotly)


#Load datasets
demo_data <- read_xpt("DEMO_J.XPT")
hdl_data <- read_xpt("HDL_J.XPT")
trigly_data <- read_xpt("TRIGLY_J.XPT")
bmx_data <- read_xpt("BMX_J.XPT")
bpx_data <- read_xpt("BPX_J.XPT") # Blood Pressure data

# Merge datasets
merged_data <- demo_data %>%
    left_join(hdl_data, by = "SEQN") %>%
    left_join(trigly_data, by = "SEQN") %>%
    left_join(bmx_data, by = "SEQN") %>%
    left_join(bpx_data, by = "SEQN") # Add blood pressure data


# Filter and rename columns
final_data <- merged_data %>%
    select(SEQN, RIAGENDR, RIDAGEYR, LBDHDD, LBDLDL, LBXTR, BMXBMI, BPXSY1, BPXDI1) %>%
    rename(
        id = SEQN,
        gender = RIAGENDR,
        age = RIDAGEYR,
        hdl = LBDHDD,
        ldl = LBDLDL,
        triglycerides = LBXTR,
        bmi = BMXBMI,
        sbp = BPXSY1, # Systolic Blood Pressure
        dbp = BPXDI1  # Diastolic Blood Pressure
    )



# Update gender values
final_data$gender <- factor(final_data$gender, levels = c(1, 2), labels = c("Male", "Female"))




##User Interface
ui <- dashboardPage(
    dashboardHeader(title = "NHANES Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "dataTab", icon = icon("table")),
            menuItem("Visualization", tabName = "vizTab", icon = icon("chart-bar"))
        ),
        selectInput(
            "variable",
            "Select a variable:",
            choices = c(
                "HDL Cholesterol" = "hdl",
                "LDL Cholesterol" = "ldl",
                "Triglycerides" = "triglycerides",
                "BMI" = "bmi",
                "Systolic Blood Pressure" = "sbp",
                "Diastolic Blood Pressure" = "dbp"
            ),
            selected = "bmi"
        ),
        selectInput(
            "gender",
            "Select gender:",
            choices = c("Both" = "both", "Male" = "Male", "Female" = "Female"),
            selected = "both"
        ),
        sliderInput("ageRange", "Age range:", 0, 100, value = c(0, 100), step = 1)
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "dataTab",
                DT::dataTableOutput("dataTable")
            ),
            tabItem(
                tabName = "vizTab",
                plotly::plotlyOutput("interactivePlot"),
                plotly::plotlyOutput("histogramPlot")
                
            )
        )
    )
)



##Server
server <- function(input, output, session) {
    reactive_data <- reactive({
        gender_filtered_data <- if (input$gender == "both") {
            final_data
        } else {
            final_data %>% filter(gender == input$gender)
        }
        gender_filtered_data %>%
            filter(age >= input$ageRange[1], age <= input$ageRange[2])
    })
    
    observe({
        updateSliderInput(session, "ageRange", min = min(final_data$age), max = max(final_data$age), value = c(min(final_data$age), max(final_data$age)))
    })
    
    output$dataTable <- DT::renderDataTable({
        DT::datatable(
            reactive_data(),
            options = list(pageLength = 10),
            rownames = FALSE
        )
    })
    
    output$interactivePlot <- plotly::renderPlotly({
        p <- ggplot(reactive_data(), aes_string(x = "age", y = input$variable, color = "gender")) +
            geom_point(alpha = 0.5) +
            labs(title = paste0("Scatterplot of ", input$variable, " vs. Age"), x = "Age", y = input$variable) +
            theme_minimal()
        ggplotly(p)
    })
    
    
    output$histogramPlot <- plotly::renderPlotly({
        p <- ggplot(reactive_data(), aes_string(x = input$variable)) +
            geom_histogram(binwidth = 1, color = "black", fill = "lightblue") +
            labs(title = paste0("Histogram of ", input$variable), x = input$variable, y = "Frequency") +
            theme_minimal()
        ggplotly(p)
    })
    
}



## Run the application 
shinyApp(ui = ui, server = server)















