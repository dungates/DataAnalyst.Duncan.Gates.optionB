#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://github.com/Appsilon/semantic.dashboard
# https://appsilon.com/semantic-dashboard-new-open-source-r-shiny-package/

library(tidyverse)
library(here)
library(scales) # For graphing scales
library(shiny)
library(shinyWidgets)
library(DT)
library(grDevices)
library(shinyjs)
library(shinydashboard)
extrafont::loadfonts()

teaching_df <- read_rds(here("Data/original_df.rds")) # Read in the data
# Relevant columns
oldcols <- c("Professional training session", 
  "Select your site (district, parish, or network).", 
  "Select the best description for your role.", 
  "Select the grade-band(s) you focused on.", 
  "I am satisfied with the overall quality of today's professional learning session.", 
  "Today's topic was relevant for my role.", 
  "The activities of today's session were well-designed to help me learn.", 
  "How likely are you to apply this learning to your practice in the next 4-6 weeks?", 
  "Select the name of your first facilitator.", 
  "S/he facilitated the content clearly....12", 
  "S/he effectively built a community of learners....13", 
  "Did you have a second facilitator?", 
  "Select the name of your second facilitator.", 
  "S/he facilitated the content clearly....16", 
  "S/he effectively built a community of learners....17",
  "How likely are you to recommend this professional learning to a colleague or friend?"
) # Original column names
newcols <- str_to_title(c("Professional training session", 
             "District, parish, or network", 
             "What is the best description for your role?", 
             "What grade band(s) do you focus on?", 
             "% satisfied with the overall quality of today's professional learning session", 
             "% Who say today's topic was relevant for my role", 
             "% Who say activities of today's session were well-designed to help me learn", 
             "How likely are you to apply this learning to your practice in the next 4-6 weeks?", 
             "Name of your first facilitator", 
             "S/he facilitated the content clearly (first facilitator)", 
             "S/he effectively built a community of learners (first facilitator)", 
             "Did you have a second facilitator?", 
             "Name of your second facilitator.", 
             "S/he facilitated the content clearly (second facilitator)", 
             "S/he effectively built a community of learners (second facilitator)",
             "How likely are you to recommend this professional learning to a colleague or friend?"
)) # New column names
# Small data clean
teaching_df <- teaching_df %>% 
  mutate_if(is.character, funs(replace_na(., "No Response"))) %>%
  mutate_if(is.numeric, funs(replace_na(., "No Response"))) %>%
  rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols)

col = grDevices::colorRampPalette(c("#040404", "#04ABEB", "#FFFFFF")) # Color ramp, includes white because ugly otherwise
teaching_colors <- c("#04ABEB", "#040404", "#346475", "#324D56") # Teaching Lab Color Palette - derived from logo


# sidebarPanel2 <- function (..., out = NULL, width = 4) 
# {
#     div(class = paste0("col-sm-", width), 
#         tags$form(class = "well", ...),
#         out
#     )
# }

# header <- dashboardHeader(
#   title = "Teaching Lab Data"
# ) # Figure out how to set header


# Define UI for application that draws 
ui <- dashboardPage(
  tags$head(tags$style(
    HTML('
         #sidebar {
            background-color: #355c7d;
        }

        body, label, input, button, select, h2 { 
          font-family: "Oswald";
        }')
  )),
  
  # Background color
    setBackgroundColor(
      color = c("#F7FBFF", "#2171B5"),
      gradient = "linear",
      direction = "bottom"
    ),
    
  
    # Application title
    dashboardHeader(title = "Interactive Data Visualization",
                    titleWidth = 400),

    # Sidebar with a series of selections for variables
    dashboardSidebar(
        sidebarMenu(
          menuItem("Inputs", icon = icon("bar-chart-o"),
            selectInput("data", 
                        newcols, 
                        width = '600px', 
            selected = NULL,
                        label = h3("Select Variable:"))#, # Basic Shiny Input Take
            # textOutput("textData"),
            # width = 2
        )
      )
    ),

        # Show a plot of the generated distribution
    dashboardBody(
      fluidRow(splitLayout(cellWidths = 750,
                           plotOutput("piePlotNA", width = "100%"),
                           plotOutput("piePlot", width = "100%")
                           )),
       fluidRow(splitLayout(cellWidths = 750,
                            DTOutput("tableData2"),
                            DTOutput("tableData")
                            ))
      )
    #)
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
    
    mydata2 <- reactive({
      teaching_df %>%
        dplyr::group_by(get(input$data)) %>% # Group by input variable
        dplyr::summarise(n = n()) %>% # Get count of variable
        ungroup() %>% # Ungroup
        dplyr::filter(`get(input$data)` != "No Response") %>%# Filter out non-responses
        dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) # Make a percent column without non-responses
    })
    
    output$piePlot <- renderPlot({
        g <- ggplot2::ggplot(mydata(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + #Pie chart input, ordered by n
            labs(fill = "Type", x = NULL, y = NULL,
                 title = paste(str_to_title(as.character(input$data))),
                 subtitle = "Variables with greater than 5% occurrence are labelled.") +
            geom_bar(stat = "identity", width = 0.5) + # Using bar columns put in polar coordinates later
            geom_text(aes(label = ifelse(percent > 5,
                                         paste0(percent, "%\n", ifelse(
                                           str_length(`get(input$data)`) > 10,
                                           gsub('(.{9,}?)\\s', '\\1\n\\2', `get(input$data)`),
                                           `get(input$data)`)
                                         ),
                                         paste("")),
                          x = 0.4, # Outwards distance from pie chart
                          color = reorder(`get(input$data)`, n)),
                      position = position_stack(vjust = 0.5), #position stack normally stacks bars, but here it keeps the text in the right place once put in polar
                      size = 3,
                      fontface = "bold",
                      family = "Oswald") + # Add bold text with percentage and variable label
            scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
            coord_polar(theta = "y", direction = -1) + # Make bars polar
            scale_fill_manual(values = c(rev(col(nrow(mydata())))), na.value = "white") + # custom colors
            scale_color_manual(values = c(rev(col(nrow(mydata()))))) +
            theme_void() + # Remove all panel elements
            ggpubr::theme_transparent() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, family = "Oswald"),
                  plot.subtitle = element_text(hjust = 0.5, family = "Oswald", 
                                               margin = margin(t = 5, unit = "pt"))#,
                  # plot.background = element_rect(fill = "#355c7d", color = "#355c7d")
                  )
        cowplot::ggdraw(g) #+
          # theme(plot.background = element_rect(fill = "#355c7d", color = NA))
    }, height = 400, width = 750, res = 75)
    
    output$piePlotNA <- renderPlot({
      g2 <- ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + #Pie chart input, ordered by n
        labs(fill = "Type", x = NULL, y = NULL,
             title = paste(str_to_title(as.character(input$data))),
             subtitle = "Variables with greater than 5% occurrence are labelled, and non-responses removed.") +
        geom_bar(stat = "identity", width = 0.5) + # Using bar columns put in polar coordinates later
        geom_text(aes(label = ifelse(percent > 5,
                                     paste0(percent, "%\n", ifelse(
                                       str_length(`get(input$data)`) > 10,
                                       gsub('(.{9,}?)\\s', '\\1\n\\2', `get(input$data)`),
                                       `get(input$data)`)
                                       ),
                                     paste("")),
                      x = 0.4, # Distance outwards from pie chart
                      color = reorder(`get(input$data)`, n)),
                  position = position_stack(vjust = 0.5), #position stack normally stacks bars, but here it keeps the text in the right place once put in polar
                  size = 3,
                  fontface = "bold",
                  family = "Oswald") + # Add bold text with percentage and variable label
        scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
        coord_polar(theta = "y", direction = -1) + # Make bars polar
        scale_fill_manual(values = c(rev(col(nrow(mydata2())))), na.value = "white") + # custom colors
        scale_color_manual(values = c(rev(col(nrow(mydata2()))))) +
        theme_void() + # Remove all panel elements
        ggpubr::theme_transparent() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, family = "Oswald"),
              plot.subtitle = element_text(hjust = 0.5, family = "Oswald", 
                                           margin = margin(t = 5, unit = "pt"))#,
              # plot.background = element_rect(fill = "#355c7d", color = "#355c7d")
              )
      cowplot::ggdraw(g2) #+
        # theme(plot.background = element_rect(fill = "#355c7d", color = NA))
    }, height = 533.3333, width = 1000, res = 100)

    output$tableData <- DT::renderDT({
        DT::datatable(mydata(), 
                      colnames = c(paste(str_to_title(input$data)), "Number", "Percent"), # Column names
                      extensions = "Buttons", # Add buttons
                      options = list(order = list(list(2, 'desc')), # Sort by second column which is number
                                     dom = 'Bfrtip', # Buttons source
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), # Buttons spec
                      rownames = F, # No rownames
                      class = 'cell-border stripe hover') # Table prettyifying, cell borders that are striped and highlight on hover
    })
    
    output$tableData2 <- DT::renderDT({
      DT::datatable(mydata2(), 
                    colnames = c(paste(str_to_title(input$data)), "Number", "Percent"), # Column names
                    extensions = "Buttons", # Add buttons
                    options = list(order = list(list(2, 'desc')), # Sort by second column which is number
                                   dom = 'Bfrtip', # Buttons source
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), # Buttons spec
                    rownames = F, # No rownames
                    class = 'cell-border stripe hover') # Table prettyifying, cell borders that are striped and highlight on hover
    })
    
    output$textData <- renderText({ 
        paste0("You Have Selected ", input$data)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
