#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# IDEA: MAKE PIE CHART HIGHLIGHT SECTION WHEN SELECTED IN DATA TABLE

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
library(shiny.semantic)
library(Cairo)
library(showtext)
font_add_google(name = "Oswald", family = "Oswald") # Adds font for ggplot
showtext_auto()
showtext_opts(dpi = 150) # Changes ggplot font size
# library(shinyalert)
# extrafont::font_import(pattern = "Oswald", prompt = F)
# extrafont::loadfonts()
# dir.create('~/.fonts')
# file.copy("/Users/dunk/DataAnalyst.Duncan.Gates.optionB/PieCharter/www/Oswald-VariableFont_wght.ttf",
#           "~/.fonts")
# system('fc-cache -f ~/.fonts')

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

col = grDevices::colorRampPalette(c("#040404", "#04ABEB", "#9FDBF5")) # Color ramp, includes white because ugly otherwise
teaching_colors <- c("#04ABEB", "#040404", "#346475", "#324D56") # Teaching Lab Color Palette - derived from logo

# colordates <- unique(c(na.omit(teaching_df$`Date for the session`))) # Need to figure out how to highlight available dates


# sidebarPanel2 <- function (..., out = NULL, width = 4) 
# {
#     div(class = paste0("col-sm-", width), 
#         tags$form(class = "well", ...),
#         out
#     )
# }


# Define UI for application that draws 
ui <- dashboard_page(
  
  title = "Teaching Lab Data Visualization",
  
  # tags$head(includeCSS("www/styles.css")), # Messes with shiny.semantic
  
  # tags$head(tags$style(HTML('* {font-family: "Oswald"};'))), # Messes with shiny.semantic
  
  theme = "solar", # Best theme so far, need to see if Oswald can be integrated throughout app
  
  # No longer needed with theme
  # Background color
    # setBackgroundColor(
    #   color = c("#F7FBFF", "#2171B5"),
    #   gradient = "linear",
    #   direction = "bottom"
    # ),
    
  
    # Application title
    dashboardHeader(title = h2("Interactive Data Visualization", style = "font-family:'Oswald'"), 
                    titleWidth = "wide",
                    color = "black", 
                    inverted = T),
    # Sidebar with a series of selections for variables
    dashboardSidebar(
      side = "left", size = "wide", closable = T,
      sidebar_menu(
        # tags$head(tags$style("input.date {font-family: Oswald;}")), # Trying to change widget font
        tags$head(includeCSS("www/styles.css")), # Oswald CSS
        menu_item("", icon = shiny::icon("chart-pie"), 
          selectInput("data", 
                      newcols,
                      selected = NULL,
                      label = h3("Select Variable of Interest:", style = "font-family:'Oswald';"),
                      width = 400), # Basic Shiny Input Take
        ),
        menu_item("", icon = shiny::icon("calendar"),
          # textOutput("textData"),
          dateRangeInput("date",
                         label = h3("Calendar Select:", style = "font-family:'Oswald';"),
                      start = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      end = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      format = "yyyy-mm-dd",
                      width = "100%",
                      tags$head(includeCSS("www/styles.css"))),
          br(),
          shiny::sliderInput("date2",
                      label = h3("Date Slider:", style = "font-family:'Oswald';margin-bottom:6px;"),
                      value = c(min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                                max(as.Date(teaching_df$`Date for the session`), na.rm = T)),
                      min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
                      timeFormat = "%b %d, %Y"),
          br()
        ),
        menu_item("", icon = shiny::icon("globe"),
                  tags$a(href="https://dungates.shinyapps.io/PieCharter", "Link to Site!", target="_blank")
                  )
      )
    ),

    # Show a plot the pie charts in one row, tables in another
    dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")), # Favicon add
      tags$head(includeCSS("www/styles.css"),
                tags$style("@import url('//fonts.googleapis.com/css?family=Oswald');")),
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
      # tags$style(HTML('body {font-family:"Oswald";')),
      fluidRow(column(8,
        # cell_widths = 750,
                         plotOutput("piePlotNA", width = "100%")),
               column(8, plotOutput("piePlot", width = "100%"))),
       fluidRow(column(8,
         # cell_widths = 750,
         #                    cell_args = "padding: 6px;",
         #                    style = "border: 1px solid silver;",
                          DTOutput("tableData2")),
                column(8, DTOutput("tableData")))
      )
    #)
)

# Define server logic 
server <- function(input, output, session) {
  
  # Make dates stay the same
  
  ## Avoid chain reaction
  reactdelay <- 1
  change_slider <- reactiveVal(Sys.time())
  change_daterange <- reactiveVal(Sys.time())
  
  # Check date input 
  observeEvent(input$date2, {
    # req(input$date)
    if (difftime(Sys.time(), change_slider()) > reactdelay) {
      change_daterange(Sys.time())
      updateDateRangeInput(session,
                           "date",
                           start = input$date2[1],
                           end = input$date2[2])
    }
  })
  
  observeEvent(input$date, {
    if (difftime(Sys.time(), change_daterange()) > reactdelay) {
      change_slider(Sys.time())
      shiny::updateSliderInput(session,
                        "date2",
                        value = c(input$date[1], 
                                  input$date[2]),
                        timeFormat = "%b, %d, %Y") # Needs fixing
    }
  })
  # Data for second pie chart and table (rightmost)
    mydata <- reactive({
        teaching_df %>%
            dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
            dplyr::group_by(get(input$data)) %>% # Group by input variable
            dplyr::summarise(n = n()) %>% # Get count of variable
            ungroup() %>% # Ungroup
            dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>%# Make a percent column
            dplyr::relocate(percent, .before = n) %>%
            dplyr::filter(if (input$data == "What Is The Best Description For Your Role?") n > 5 else n > 0)
    })
    # Data for first pie chart and table (leftmost)
    mydata2 <- reactive({
      teaching_df %>%
        dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
        dplyr::group_by(get(input$data)) %>% # Group by input variable
        dplyr::summarise(n = n()) %>% # Get count of variable
        ungroup() %>% # Ungroup
        dplyr::filter(`get(input$data)` != "No Response") %>%# Filter out non-responses
        dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>%# Make a percent column without non-responses
        dplyr::relocate(percent, .before = n) %>%
        dplyr::filter(if (input$data == "What Is The Best Description For Your Role?") n > 5 else n > 0) # Filter out many responses with few observations for just one column selection
    })
    
    options(shiny.usecairo = T) # I don't think this does anything, need to read about it
    
    output$piePlot <- renderPlot({
        g <- ggplot2::ggplot(mydata(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + #Pie chart input, ordered by n
            labs(fill = "Type", x = NULL, y = NULL,
                 title = paste(str_to_title(as.character(input$data))),
                 subtitle = "Variables with greater than 5% occurrence are labelled.") +
            geom_bar(stat = "identity", width = 0.5, color = "gray10", size = ifelse(nrow(mydata()) > 10,
                                                                                     0.1,
                                                                                     0.6)) + # Using bar columns put in polar coordinates later
            geom_text(aes(label = ifelse(percent > 5,
                                         paste0(percent, "%\n", ifelse(
                                           str_length(`get(input$data)`) > 10,
                                           gsub('(.{9,}?)\\s', '\\1\n\\2', `get(input$data)`), # If character length greater than 10 add a new line at 9th character
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
            # ggpubr::theme_transparent() +
            theme(legend.position = "none",
                  plot.title = element_text(hjust = 0.5, family = "Oswald"),
                  plot.subtitle = element_text(hjust = 0.5, family = "Oswald", 
                                               margin = margin(t = 5, unit = "pt"))
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
        geom_bar(stat = "identity", width = 0.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
                                                                                 0.1,
                                                                                 0.6)) + # Using bar columns put in polar coordinates later
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
        theme_void() +
        # ggpubr::theme_transparent() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, family = "Oswald"),
              plot.subtitle = element_text(hjust = 0.5, family = "Oswald", 
                                           margin = margin(t = 5, unit = "pt"))
              # plot.background = element_rect(fill = "#355c7d", color = "#355c7d")
              )
      cowplot::ggdraw(g2) #+
      # ggsave("Images/g2.png", dpi = 300, type = "cairo")
        # theme(plot.background = element_rect(fill = "#355c7d", color = NA))
    }, height = 400, width = 700, res = 80)
    
    # Might need to output as images for resolution issues?
    # output$piePlotImage <-
    #   renderImage({
    #     outfile <- here("Images/g2.png")
    #     list(src = outfile,
    #          contentType = 'image/png',
    #          width = 700,
    #          height = 400)
    #   }, deleteFile = F)
    
    # Right most table with NAs
    output$tableData <- DT::renderDT({
        DT::datatable(mydata(), 
                      colnames = c(paste(str_to_title(input$data)), "Percent", "Number"), # Column names
                      extensions = "Buttons", # Add buttons
                      options = list(order = list(list(2, 'desc')), # Sort by second column which is number
                                     dom = 'Bfrtip', # Buttons source
                                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                     initComplete = DT::JS(
                                       "function(settings, json) {",
                                       "$('body').css({'font-family': 'Oswald'});
                                       $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
                                       "}")), # Buttons spec
                      rownames = F, # No rownames
                      class = 'cell-border stripe hover') %>%# Table prettyifying, cell borders that are striped and highlight on hover
        formatString("percent", suffix = "%") %>%
        formatStyle('n',
                    background = styleColorBar(c(0, mydata2()$n), "#04ABEB"),
                    backgroundSize = '95% 50%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'right') # Add percent bars
    })
    
    # Left most table without NAs
    output$tableData2 <- DT::renderDT({
      DT::datatable(mydata2(), 
                    colnames = c(paste(str_to_title(input$data)), "Percent", "Number"), # Column names
                    extensions = "Buttons", # Add buttons
                    options = list(order = list(list(2, 'desc')), # Sort by second column which is number
                                   dom = 'Bfrtip', # Buttons source
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   initComplete = DT::JS(
                                     "function(settings, json) {",
                                     "$('body').css({'font-family': 'Oswald'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
                                     "}"
                                   )), # Buttons spec
                    rownames = F, # No rownames
                    class = 'cell-border stripe hover') %>%# Table prettyifying, cell borders that are striped and highlight on hover
        formatString("percent", suffix = "%") %>%
        formatStyle('n',
                    background = styleColorBar(c(0, mydata2()$n), "#04ABEB"),
                    backgroundSize = '95% 50%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'right') # Add percent bars
    })
    
    # output$textData <- renderText({ 
    #     paste0("You Have Selected ", input$data)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)




