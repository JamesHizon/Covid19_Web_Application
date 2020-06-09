### COVID-19 Case Analysis By Country Web Application


### Prepare Dataset:
library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Country Data API Requests:
us <- fromJSON("https://api.covid19api.com/total/dayone/country/united-states")
mex <- fromJSON("https://api.covid19api.com/total/dayone/country/mexico")
ru <- fromJSON("https://api.covid19api.com/total/dayone/country/russia")
pi <- fromJSON("https://api.covid19api.com/total/dayone/country/philippines")
spain <- fromJSON("https://api.covid19api.com/total/dayone/country/spain")
india <- fromJSON("https://api.covid19api.com/total/dayone/country/india")
uk <- fromJSON("https://api.covid19api.com/total/dayone/country/united-kingdom")
peru <- fromJSON("https://api.covid19api.com/total/dayone/country/peru")
germany <- fromJSON("https://api.covid19api.com/total/dayone/country/germany")
iran <- fromJSON("https://api.covid19api.com/total/dayone/country/iran")
italy <- fromJSON("https://api.covid19api.com/total/dayone/country/italy")

# List of countries:
list <- list(us, mex, ru, pi, spain, india, uk, peru, germany, iran, italy)

# Join All Countries:
all_countries <- join_all(list, type='full')

# Drop unnecessary columns:
all_countries <- all_countries[-c(2:7)]

# Strip element from Date column:
all_countries$Date <- gsub('T00:00:00Z', '',
                           all_countries$Date)

all_countries$Date <- as.Date(all_countries$Date)

# Separate Data into multiple columns:
# all_countries <- all_countries %>%
# separate(Date, c("Year", "Month", "Day"), sep = "-")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Country Cases Web Application"),
    
    # Sidebar layout w/ input + output definitions:
    sidebarLayout(
      sidebarPanel(
        # Checkbox
        checkboxInput("Confirmed", "Confirmed", value = T),
        checkboxInput("Deaths", "Deaths", value = F),
        checkboxInput("Recovered", "Recovered", value = F),
        checkboxInput("Active", "Active", value = F),
        # Dataset Input
        selectInput(inputId = "dataset",
                    label = "Choose a country:",
                    choices = c("United States of America",
                                "Philippines",              
                                "Russian Federation",       
                                "Spain",                    
                                "United Kingdom",           
                                "India",                    
                                "Peru",                     
                                "Germany",                  
                                "Iran, Islamic Republic of",
                                "Mexico",                   
                                "Italy"),
                    selected = "Philippines")),
      
      mainPanel(
        plotOutput("ts_plot"), # Name of plot
        verbatimTextOutput("summary")
      )
    )
)

# Define server logic required to create plot
server <- shinyServer( 
  function(input, output) {
    
  # Output reactive upon dataset:
  datasetInput <- reactive({
    # Filter by input selection (country):
    all_countries %>% 
      filter(Country == input$dataset)
  })
    

  set.seed(123)
  pt1 <- reactive({
    if (!input$Confirmed) return(NULL)
    dataset <- datasetInput()
    ggplot(dataset, aes(x = Date, y = Confirmed)) +
      geom_point(color = 'blue')
  })
  pt2 <- reactive({
    if (!input$Deaths) return(NULL)
    # qplot(rnorm(500),fill=I("blue"),binwidth=0.2,main="plotgraph2")
    dataset <- datasetInput()
    ggplot(dataset, aes(x = Date, y = Deaths)) +
      geom_point(color = 'darkred')
  })
  pt3 <- reactive({
    if (!input$Recovered) return(NULL)
    # qplot(rnorm(500),fill=I("green"),binwidth=0.2,main="plotgraph3")
    dataset <- datasetInput()
    ggplot(dataset, aes(x = Date, y = Recovered)) +
      geom_point(color = 'darkgreen')
  })
  pt4 <- reactive({
    if (!input$Active) return(NULL)
    # qplot(rnorm(500),fill=I("yellow"),binwidth=0.2,main="plotgraph4")
    dataset <- datasetInput()
    ggplot(dataset, aes(x = Date, y = Active)) +
      geom_point(color = 'yellow')
  })
  output$ts_plot <- renderPlot({
    dataset <- datasetInput()
    pt_list <- list(pt1(), pt2(), pt3(), pt4())
    # remove the null plots from ptlist and wtlist
    to_delete <- !sapply(pt_list,is.null)
    pt_list <- pt_list[to_delete] 
    if (length(pt_list)==0) return(NULL)
    grid.arrange(grobs=pt_list)
  })
  
  
})


# Run the application 
shiny::shinyApp(ui = ui, server = server)


### Deploy Web Application:

# ### Use rsconnect package:
# library(rsconnect)
# 
# ### Authorize Account:
# rsconnect::setAccountInfo(name='jameshizon',
#                           token='BEE70D33810CA92363673EF1EDF556EC',
#                           secret='<SECRET>')
# 
# ### Deploy App:
# 
# rsconnect::deployApp('/home/james/STA 141B/Final Project/')