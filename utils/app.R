# Shiny app to display palettes from the paletteer package
# By Sharon Machlis
# Associated InfoWorld story and video:
# https://www.infoworld.com/article/3615230/r-colors-and-palettes-tips-tools.html

# CHANGE THE FILE EXTENSION FROM .txt TO .R

# Make sure all these apps are installed on your system:
library(shiny)
library(paletteer)
library(scales)
library(dplyr)
library(shinythemes)
library(stringr)

## Functions & data ----
all_packages_df <- dplyr::bind_rows(palettes_d_names, palettes_c_names, palettes_dynamic_names)

get_package_name_choices <- function(input_palette_category, input_palette_type, mydf_d = palettes_d_names, mydf_c = palettes_c_names, mydf_dynamic =  palettes_dynamic_names) {
    if(input_palette_category == "discreet") {
        mydf <- mydf_d
    } else if(input_palette_category == "continuous") {
        mydf <- mydf_c
    } else if(input_palette_category == "dynamic") {
        mydf <- mydf_dynamic
    }
    my_results <- mydf %>%
        dplyr::filter(type == input_palette_type) %>%
        pull(package)
    my_results <- sort(unique(my_results))
    return(my_results)
}



get_palette_name_choices <- function(input_palette_package, input_palette_category, input_palette_type, mydf_d = palettes_d_names, mydf_c = palettes_c_names, mydf_dynamic =  palettes_dynamic_names) {
    if(input_palette_category == "discreet") {
        mydf <- mydf_d
    } else if(input_palette_category == "continuous") {
        mydf <- mydf_c
    } else if(input_palette_category == "dynamic") {
        mydf <- mydf_dynamic
    }
    my_results <- mydf %>%
        dplyr::filter(package == input_palette_package & type == input_palette_type) %>%
        pull(palette)
    my_results <- sort(unique(my_results))
    return(my_results)
}


display_paletteer_palette <- function(my_palette, my_package, my_palette_category, input_palette_number = NULL) {
    pkg_palette <- paste0(my_package, "::", my_palette)
    if(my_palette_category == "discreet") {
        scales::show_col(as.character(paletteer_d(pkg_palette)))
    } else if(my_palette_category == "continuous") {
        scales::show_col(as.character(paletteer_c(pkg_palette, n = input_palette_number)))
    } else if(my_palette_category == "dynamic") {
        scales::show_col(as.character(paletteer_dynamic(pkg_palette, n = input_palette_number)))
        
    }
}

search_palettes <- function(input_palette_category2, input_palette_search2, mydf_d = palettes_d_names, mydf_c = palettes_c_names, mydf_dynamic =  palettes_dynamic_names) {
    if(input_palette_category2 == "discreet") {
        mydf <- mydf_d
    } else if(input_palette_category2 == "continuous") {
        mydf <- mydf_c
    } else if(input_palette_category2 == "dynamic") {
        mydf <- mydf_dynamic
    }
    my_results <- dplyr::filter(mydf, stringr::str_detect(tolower(palette), tolower(input_palette_search2)) | str_detect(tolower(package), tolower(input_palette_search2)))
    return(my_results)
    
}

# UI ----
ui <- navbarPage(
    title = "Paletteer Color Palettes",
    theme = shinytheme("cosmo"),
## tab 1 Display ----
tabPanel("Display Palettes",
    sidebarLayout(
        sidebarPanel(
             radioButtons("palette_category", "Select Palette Category: ", choices = c("Continuous" = "continuous", "Discreet" = "discreet", "Dyanmic" = "dynamic"), selected = "discreet"),
             radioButtons("palette_type", "Select Palette Type: ", choices = c("Diverging" = "diverging", "Qualitative" = "qualitative", "Sequential" = "sequential"), selected = "sequential"),
             uiOutput("palette_packages_placeholder"),
             uiOutput("palette_names_placeholder"),
             uiOutput("palette_number_placeholder")
        ),

        # Display colors
        mainPanel(
           uiOutput("headline"),
           plotOutput("display_palette"),
           uiOutput("code")
        )
    )
),   # end tab 1
## tab2 search ----
  tabPanel("Search Palettes by Name",
           sidebarLayout(
               sidebarPanel(
                   radioButtons("palette_category2", "Select Palette Category: ", choices = c("Continuous" = "continuous", "Discreet" = "discreet", "Dyanmic" = "dynamic"), selected = "discreet"),
                   textInput("palette_search2", "Search Palette and Package Names: ", value = "blue")
           ),
           mainPanel(width = 8,
               DT::DTOutput("palette_table2")
           )

           ) # end sidebar layout 2
) # end tab 2


) # end navbar page UI

# Define server logic required to display colors
server <- function(input, output) {

   output$headline <- renderUI({
     req(input$palette_name, input$palette_package, input$palette_category)
     h3(stringr::str_glue("{input$palette_package}::{input$palette_name}"), align = "center")
   })
   
   code_4_palette <- reactive({
     req(input$palette_name, input$palette_package, input$palette_category)
     needed_function <- dplyr::case_when(
       input$palette_category == "discreet" ~ stringr::str_glue('paletteer_d("{input$palette_package}::{input$palette_name}")'),
       input$palette_category == "continuous" ~ stringr::str_glue('paletteer_c("{input$palette_package}::{input$palette_name}", n = n)'), 
       input$palette_category == "dynamic" ~ stringr::str_glue('paletteer_dynamic("{input$palette_package}::{input$palette_name}", n = n)')
     )
     
   })
   
   code_4_ggplot_fill <- reactive({
     req(input$palette_name, input$palette_package, input$palette_category)
     needed_function <- dplyr::case_when(
       input$palette_category == "discreet" ~ stringr::str_glue('scale_fill_paletteer_d("{input$palette_package}::{input$palette_name}")'),
       input$palette_category == "continuous" ~ stringr::str_glue('scale_fill_paletteer_c("{input$palette_package}::{input$palette_name}", n = n)'),
       input$palette_category == "dynamic" ~ stringr::str_glue('scale_fill_dynamic("{input$palette_package}::{input$palette_name}", n = n)')
     )
   })
   
   code_4_ggplot_color <- reactive({
     req(input$palette_name, input$palette_package, input$palette_category)
     needed_function <- dplyr::case_when(
       input$palette_category == "discreet" ~ stringr::str_glue('scale_color_paletteer_d("{input$palette_package}::{input$palette_name}")'),
       input$palette_category == "continuous" ~ stringr::str_glue('scale_color_paletteer_c("{input$palette_package}::{input$palette_name}", n = n)'),
       input$palette_category == "dynamic" ~ stringr::str_glue('scale_color_dynamic("{input$palette_package}::{input$palette_name}", n = n)')
     )
   })
   
   output$code <- renderUI({
     req(code_4_palette(), code_4_ggplot_fill(), code_4_ggplot_color())
     HTML(stringr::str_glue("<div style = 'margin-left: 80px;'><p><strong>palette: </strong><span style='font-family:Courier;'>{code_4_palette()}</span></p><p><strong>ggplot2: </strong>&nbsp;<span style='font-family:Courier;'>{code_4_ggplot_fill()}</span> &nbsp;or <br /><span style='font-family:Courier;'>{code_4_ggplot_color()}</span></p></div>"))
   })
  
    output$palette_packages_placeholder <- renderUI({
        req(input$palette_category, input$palette_type)
        selectInput("palette_package", label = h3("Select Source Package: "), 
                           choices = get_package_name_choices(input$palette_category, input$palette_type), selected = "colorBlindness")
    })
    
    
    output$palette_names_placeholder <- renderUI({
        req(input$palette_category, input$palette_type, input$palette_package)
        radioButtons("palette_name", label = h3("Select Palette: "),
                    choices = get_palette_name_choices(input$palette_package, input$palette_category, input$palette_type), selected = "Blue2Orange12Steps"               )
    })
    
    output$palette_number_placeholder <- renderUI({
        req(input$palette_category)
        if(input$palette_category == "discreet") {
            NULL
        } else {
            sliderInput("palette_number", label = h3("How many colors? "),
                        value = 9, min = 3, max = 30, step = 1
                        )
        }
        
    })
    
   output$display_palette <- renderPlot({
       req(input$palette_name, input$palette_package, input$palette_category)
       display_paletteer_palette(input$palette_name, input$palette_package, input$palette_category, input$palette_number)
   })
   
   
# Tab 2 logic ----
   
table_data2 <- reactive({
    req(input$palette_category2, input$palette_search2)
    if(input$palette_search2 != ""){
        search_palettes(input$palette_category2, input$palette_search2)   
    } else
        if(input$palette_category2 == "discreet") {
            palettes_d_names
        } else if(input$palette_category2 == "continuous") {
            palettes_c_names
        } else if(input$palette_category2 == "dynamic") {
            palettes_dynamic_names
        }  

})
       
       
   
  output$palette_table2 <- DT::renderDT({
      req(table_data2())
      DT::datatable(table_data2(), filter = 'top', options = list(
                    pageLength = 20,
                    lengthMenu = c(20, 50, 100)
                    )
                    )
      
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
