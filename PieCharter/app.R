#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# MAKE DATE SLIDER REACTIVE, TRY PUTTING IN ONE OBSERVE EVENT

# MAKE DATA TABLE WIDTHS EXPAND

# MAKE ALL FONT OSWALD

library(tidyverse)
library(lubridate)
library(here)
library(scales) # For graphing scales
library(shiny)
library(shinyWidgets)
library(DT)
library(grDevices)
library(shinyjs)
library(semantic.dashboard)
# library(shiny.semantic)
# library(shinyalert)
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
  rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols) %>%
  mutate(`Date for the session` = lubridate::ymd(`Date for the session`))

col = grDevices::colorRampPalette(c("#040404", "#04ABEB", "#00FFFF")) # Color ramp, includes white because ugly otherwise
teaching_colors <- c("#04ABEB", "#040404", "#346475", "#324D56") # Teaching Lab Color Palette - derived from logo


# sidebarPanel2 <- function (..., out = NULL, width = 4) 
# {
#     div(class = paste0("col-sm-", width), 
#         tags$form(class = "well", ...),
#         out
#     )
# }


# Define UI for application that draws 
ui <- dashboard_page(
  
  # tags$head(tags$style(HTML('* {font-family: "Oswald"};'))),
  
  theme = "darkly",
  
  # No longer needed with theme
  # Background color
    # setBackgroundColor(
    #   color = c("#F7FBFF", "#2171B5"),
    #   gradient = "linear",
    #   direction = "bottom"
    # ),
    
  
    # Application title
    dashboardHeader(title = "Interactive Data Visualization", 
                    titleWidth = "wide",
                    color = "black", 
                    inverted = T),
    # Sidebar with a series of selections for variables
    dashboardSidebar(
      side = "left", size = "wide",
      sidebar_menu(
        menu_item("", icon = icon("home"),
          selectInput("data", 
                      newcols,
                      selected = NULL,
                      label = h3("Select Variable:", style = "font-family:'Oswald';"),
                      width = 400), # Basic Shiny Input Take
          # textOutput("textData"),
          dateRangeInput("date",
                         label = h3("Select Date Range:", style = "font-family:'Oswald';"),
                      start = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      end = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      format = "yyyy-mm-dd"),
          shiny::sliderInput("date2",
                      label = h3("Select Date Range:", style = "font-family:'Oswald';"),
                      value = c(min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                                max(as.Date(teaching_df$`Date for the session`), na.rm = T)),
                      min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      max = max(as.Date(teaching_df$`Date for the session`), na.rm = T))
        )
      )
    ),

    # Show a plot the pie charts in one row, tables in another
    dashboardBody(
      # # Custom css for Oswald font
      # tags$head(tags$style(HTML('
      # .main-header .logo {
      #   font-family: "Oswald";
      #   font-weight: bold;
      #   font-size = 24px;
      # }
      # .skin-blue .main-header .logo {
      #     background-color: #3c8dbc;
      #   }
      #   .skin-blue .main-header .logo:hover {
      #     background-color: #3c8dbc;
      #   }'))),
      fluidRow(split_layout(cell_widths = 750,
                           plotOutput("piePlotNA", width = "100%"),
                           plotOutput("piePlot", width = "100%")
                           )),
       fluidRow(split_layout(cell_widths = 750,
                            cell_args = "padding: 6px;",
                            style = "border: 1px solid silver;",
                            DTOutput("tableData2"),
                            DTOutput("tableData")
                            ))
      )
    #)
)

# Define server logic 
server <- function(input, output, session) {
  
  # Update the dateRangeInput if start date changes
  observeEvent(input$date[1], {
    req(input$date)
    end_date = input$date[2]
    # If end date is earlier than start date, update the end date to be the same as the new start date
    if (input$date[2] < input$date[1]) {
      end_date = input$date[1]
    }
    updateDateRangeInput(session,"date", start=input$date[1], end=end_date, min=input$date[1] )
  })
  
  # Make dates the same
  # observeEvent(input$date, {
  #   req(input$date)
  #   # if (input$date2[1] != input$date[1] | input$date2[2] != input$date[2]) {
  #     updateDateRangeInput(session,
  #                          "date",
  #                          start = input$date2[1],
  #                          end = input$date2[2])
  #   # }
  # })
  
  # observeEvent(input$date2, {
  #   # if (input$date[1] != input$date2[1] | input$date2[2] != input$date[2]) {
  #     updateSliderInput(session,
  #                       "date2",
  #                       value = c(input$date[1], input$date[2]))
  #   # }
  # })
  
    mydata <- reactive({
        teaching_df %>%
            dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
            dplyr::group_by(get(input$data)) %>% # Group by input variable
            dplyr::summarise(n = n()) %>% # Get count of variable
            ungroup() %>% # Ungroup
            dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) # Make a percent column
    })
    
    mydata2 <- reactive({
      teaching_df %>%
        dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
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
    }, height = 400, width = 700, res = 80)
    
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
        scale_fill_manual(values = c(rev(col(nrow(mydata2()))))) + # custom colors
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
    }, height = 400, width = 700, res = 80)

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




