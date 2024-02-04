# Ternary Phase Plot

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(ggtern)
library(DT)
library(plotly)

# HEADER
header <- dashboardHeader(disable = TRUE)

# SIDEBAR
sidebar <- dashboardSidebar(disable = TRUE)

# BODY
body <- dashboardBody(
  
  fluidRow(
    box(width = 2, title = "User Input", status = "primary", solidHeader = TRUE, collapsible = TRUE,
        fileInput("file","Upload  '.csv' file:"),
        uiOutput("vx"), # vx is coming from renderUI in server
        uiOutput("vy"), # vy is coming from renderUI in server
        uiOutput("vz"), # vy is coming from renderUI in server
        uiOutput("vv"), # vy is coming from renderUI in server
        sliderInput("dot",
                    "Point size:",
                    min = 0,
                    max = 10,
                    value = 5,
                    step = 1),
        sliderInput("label",
                    "Label size:",
                    min = 0,
                    max = 10,
                    value = 0,
                    step = 0.5),
        materialSwitch(inputId = "theme", 
                       label = tags$b("Choose a theme: White/Gray"), value = FALSE, 
                       status = "primary")
    ),
    box(width = 10, title = "Ternary Plot", footer=HTML(""), # add foot note here if required
        status = "primary",solidHeader = TRUE, collapsible = TRUE,
        plotOutput("plot",  height ="570px")
    )
  ),
  
  fluidRow(
    box(width = 12, title = "Data Table", status = "primary",solidHeader = TRUE, collapsible = TRUE,
        DT::dataTableOutput("table")
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# Define server logic
server <- function(input, output) {
  
  # get the names of the variables from the data file and used them in the selectInput part of the renderUI function
  vars <- reactive({
    req(input$file)
    names(read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ","))
  })
  
  output$vx <- renderUI({
    selectInput("vx", "Select the 1st (Left) component:", choices = vars())
  })
  
  output$vy <- renderUI({
    selectInput("vy", "Select the 2nd (Top) component:", choices = vars())
  })
  
  output$vz <- renderUI({
    selectInput("vz", "Select the 3rd (Right) component:", choices = vars())
  })
  
  output$vv <- renderUI({
    req(input$file)       # read again the data file to add option 'None' in the selection box !
    vars2 <- names(read.csv(input$file$datapath,
                           header = TRUE,
                           sep = ","))
    vars2$None <- 0
    
    selectInput("vv", "Select variable to be mapped to colour:", choices =vars2)
  })
  
  # render plot
  output$plot <- renderPlot({
    # read the data file and adds id column to the data frame
    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ",")
    
    df$id <- 1:nrow(df)

    if (input$theme) {
      print(ggtern(df, aes_string(x=input$vx, y=input$vy, z=input$vz)) + 
              geom_point(aes_string(fill=input$vv),color="black",shape=21,size=input$dot) +
              geom_density_tern(aes(color=..level..),bins=5) + #
              geom_text(aes(label=id),size=input$label) +
              scale_fill_gradient(low="yellow",high="red") +
              theme(legend.position=c(0,1),legend.justification=c(0,1)) +
              theme_rgbg() +
              labs(fill="Value"))
      
    } else {
      print(ggtern(df, aes_string(x=input$vx, y=input$vy, z=input$vz)) + 
              geom_point(aes_string(fill=input$vv),color="black",shape=21,size=input$dot) +
              geom_density_tern(aes(color=..level..),bins=5) + #
              geom_text(aes(label=id),size=input$label) +
              scale_fill_gradient(low="yellow",high="red") +
              theme(legend.position=c(0,1),legend.justification=c(0,1)) +
              theme_rgbw()+
              labs(fill="Value"))
      
    }   
    
  })
  
  # print dataframe in table to confirm it is read properly
  output$table <- DT::renderDataTable({
    req(input$file)
    df <- read.csv(input$file$datapath,
                   header = TRUE,
                   sep = ",")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)

