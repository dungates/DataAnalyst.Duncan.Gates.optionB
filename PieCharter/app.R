#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(here)
library(scales) # For graphing scales
library(shiny)
library(shinyWidgets)
library(DT)
library(grDevices)
teaching_df <- read_rds(here("Data/final_df.rds")) # Read in the data
cols <- c("professional_training_session", 
          "select_your_site_district_parish_or_network", 
          "select_the_grade_band_s_you_focused_on", 
          "learning_session_satisfaction",
          "todays_topic_was_relevant_for_my_role",
          "designed_to_help_me_learn",
          "likely_to_apply_46_weeks",
          "select_the_name_of_your_first_facilitator",
          "s_he_facilitated_the_content_clearly_12",
          "did_you_have_a_second_facilitator",
          "select_the_name_of_your_second_facilitator",
          "s_he_facilitated_the_content_clearly_16",
          "s_he_effectively_built_a_community_of_learners_17") 
col = grDevices::colorRampPalette(c("#040404", "#04ABEB"))

sidebarPanel2 <- function (..., out = NULL, width = 4) 
{
    div(class = paste0("col-sm-", width), 
        tags$form(class = "well", ...),
        out
    )
}


# Define UI for application that draws 
ui <- fluidPage(

  
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #dec4de;
        }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
  )),
  
    setBackgroundColor(
      color = c("#F7FBFF", "#2171B5"),
      gradient = "linear",
      direction = "bottom"
    ),
    
  
    # Application title
    titlePanel("Pie Chart Maker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel2(
            out = h5("Quick (very ugly) Shiny App I made to visualize all discrete variables with pie charts, there are obviously better visualization tools
                     for pie charts, but I wanted to demonstrate Shiny competency. The variable names are still in their 'dirty' form but should
                     at least be decipherable."),
            selectInput("data", cols, width = '600px',
                        label = h3("Select Variable:")), # Basic Shiny Input Take
            textOutput("textData")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           wellPanel(plotOutput("piePlot", width = "100%")),
           DTOutput("tableData"),
           HTML('<style type="text/css">
        .span8 .well { background-color: #00FFFF; }
        </style>')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    mydata <- reactive({
        teaching_df %>%
            dplyr::group_by(get(input$data)) %>% # Group by input variable
            dplyr::summarise(n = n()) %>% # Get count of variable
            ungroup() %>% # Ungroup
            dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) # Make a percent column
    })
    
    output$piePlot <- renderPlot({
        ggplot2::ggplot(mydata(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + #Pie chart input, ordered by n
            labs(fill = "Type", x = NULL, y = NULL,
                 title = paste("Percentage of", as.character(input$data)),
                 subtitle = "Variables with greater than 5% occurrence") +
            geom_bar(stat = "identity", color = "gray", width = 0.5) + # Using bar columns put in polar coordinates later
            geom_text(aes(label = ifelse(percent > 5,
                                         paste0(percent, "%\n", `get(input$data)`),
                                         paste("")),
                          x = 0.4),
                      position = position_stack(vjust = 0.5), #position stack normally stacks bars, but here it keeps the text in the right place once put in polar
                      size = 3,
                      fontface = "bold") + # Add bold text with percentage and variable label
            scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
            coord_polar(theta = "y", direction = -1) + # Make bars polar
            scale_fill_manual(values = c(col(nrow(mydata()))), na.value = "white") + # custom colors
            theme_void() + # Remove all panel elements
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5),
                  plot.subtitle = element_text(hjust = 0.5),
                  plot.background = element_rect(fill = "skyblue"))
    })

    output$tableData <- DT::renderDT({
        DT::datatable(mydata(), colnames = c(paste(str_to_title(input$data), "Group"), "Number", "Percent"),
                      options = list(order = list(list(2, 'desc'))))
    })
    
    output$textData <- renderText({ 
        paste("You have selected", str_to_title(input$data))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
