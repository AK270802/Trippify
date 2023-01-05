library(shiny)
library(shinydashboard)
library(bslib)


setwd("C:\\Akshay\\Projects\\FLASK\\TravelBot\\TravelRecommender")
# Read in the data
data.cities <- read.csv("data_cities.csv")
data.nature <- read.csv("data_nature.csv")
#data.flights <- read.csv("MainData.csv")

# Drop irrelevant columns - "Fun...Games" and "Nature...Parks"
data.cities <- within(data.cities, rm("Fun...Games", "Nature...Parks"))

colnames(data.cities) <- c("label", "Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                           "Concerts.Shows", "Museums", "Outdoor.Activities", "Food.Drink")

ui <- dashboardPage(
  dashboardHeader(title = "TRIPPIFY"),
  dashboardSidebar(menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                   menuItem("Destinations", tabName = "Destinations", icon = icon("th"))),

  dashboardBody(
    
     # CSS
    tags$head(
        # tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
fixedRow(
        column(4,
            tags$div(class="explainer",
                     img(src="https://raw.githubusercontent.com/AK270802/Image/main/logo.png", width="200px"),
                     h2("TRIPPIFY"),
                     p("Trippify is a travel recommender system that helps used by the tourist and travelers to fulfil their needs 
                     which makes user to take decisions easy like deciding the travel destinations, finding nearby Point of Interest 
                     (POI),restaurants, shortest distance to travel, accommodation"),
                     h4("Click on Dashboard to get started")
            )

        ),
        column(7, offset=1,
               plotOutput("showPlot1") 
        )
    ),
     hr(),
     
    tabItems(
      tabItem(tabName = "Dashboard",
              fluidRow(
                box(
                  # Application title
                  titlePanel("Welcome to the TRIPPIFY"), background = "black",
                  mainPanel(
                    selectInput("select","Select:", c("Choose One:", "Nature", "Metropolitan"))
                  ),
                  mainPanel(
                    textOutput("text1"), br(),
                    sliderInput("shop", "Shopping", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("night", "Nightlife", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("gtours", "Guided Tours", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("spa.well", "Spas/Wellness", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("sl", "Sights/Landmarks", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("cshows", "Concerts/Shows", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("muse", "Museums", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("out", "Outdoor Activities", min = 1, max = 10, value = 5, step = 0.01),
                    sliderInput("fand", "Food and Drink", min = 1, max = 10, value = 5, step = 0.01)
                  )),
                mainPanel(
                  fluidRow(column(12, align = 'center', actionButton("go", "Go"))),
                  hr(),
                  fluidRow(column(12, align = 'center', textOutput("result.text")))
                )
              )
              
      ),
      
      #Tab Cintent for the Destinations Page
      tabItem(tabName = "Destinations",
              h2("These are some of our destinations."),
              fluidPage(
                box(
                  mainPanel(
                    HTML('<img src="https://www.sandeepdavada.com/wp-content/uploads/2018/02/NYCSkyline-14.jpg?189db0&189db0", height = 100, width = 100'),
                    h3("New York"),
                    p("New York City is the most populous city in the United States. With an estimated 2019 population of 8,336,817 distributed over a land area of about 302.6 square miles (784 km2), New York City is also the most densely populated major city in the United States."),
                  )
                ),
                box(
                  mainPanel(
                    HTML('<img src="https://www.goinsurance.com.au/wp-content/uploads/2018/02/San-Francisco-Top-10-Must-See-Attractions-Activities-1.jpeg", height = 100, width = 100'),
                    h3("San Francisco"),
                    p("San Francisco, officially the City and County of San Francisco, is the cultural, commercial, and financial center of Northern California. San Francisco is the 16th most populous city in the United States, and the fourth most populous in California, with 881,549 residents as of 2019.")
                  )
                ),
                box(
                  mainPanel(
                    HTML('<img src="https://api.time.com/wp-content/uploads/2014/06/washington-dc-capitol-building.jpg", height = 100, width = 100'),
                    h3("Washington D.C."),
                    p("Washington, D.C., formally the District of Columbia and commonly referred to as Washington, D.C., D.C., or simply D.C., is the capital of the United States. Founded after the American Revolution as the seat of government of the newly independent country, Washington was named after George Washington, first President of the United States and Founding Father.")
                  )
                ),
                box(
                  mainPanel(
                    HTML('<img src="http://cdn1.blairfieldrealty.com/wp-content/uploads/2019/05/19154443/iStock-1071629386-scaled.jpg", height = 100, width = 100'),
                    h3("Austin"),
                    p("Austin is the capital of the U.S. state of Texas and the seat of Travis County. It is the 11th-most populous city in the United States and the 4th-most populous city in Texas. It is also the fastest growing large city in the United States, the second most populous state capital after Phoenix, Arizona, and the southernmost state capital in the contiguous United States.")
                  )
                ),
                box(
                  mainPanel(
                    HTML('<img src="https://wallpapercave.com/wp/wp1825956.jpg", height = 100, width = 100'),
                    h3("Los Angeles"),
                    p("Los Angeles, officially the City of Los Angeles and often known by its initials L.A., is the most populous city in California and the second most populous city in the United States, after New York City. With an estimated population of four million, Los Angeles is the cultural, financial, and commercial center of Southern California.")
                  )
                ),
                box(
                  mainPanel(
                    HTML('<img src="https://www.sandeepdavada.com/wp-content/uploads/2018/02/Rome-14.jpg?189db0&189db0", height = 100, width = 100'),
                    h3("Portland"),
                    p("Portland is the largest city in the U.S. state of Oregon and the seat of Multnomah County. It is a major port in the Willamette Valley region of the Pacific Northwest, at the confluence of the Willamette and Columbia rivers.")
                  )
                ),
           
                
              )
      )
    )
  )
)

server <- function(input, output) { 
  output$text1 <- renderText(paste0("On a scale of 1-10, with 1 being the least and 10 being the most,
                                    how much would you like to do each of the following activity?"))
  
  output$text3 <- renderText(paste0("Please select whether you want to visit a metropolitan or explore the nature?"))
  
  # Create a vector by combining all the inputs when the user clicks on 'Go'
  new_vector <- eventReactive(input$go, {
    vec <- cbind(input$shop, input$night, input$gtours, input$spa.well, input$sl, input$cshows, input$muse, input$out, input$fand)
    return(vec)
  })
  
  
  # Function to calculate Euclidean distance of input vector with a row
  calculate.euclidean <- function(x){
    return(dist(rbind(x, new_vector())))
  }
  
  
  # Subset data, keep only those columns which will be used in calculating Euclidean distance
  new_df <- reactive({
    subset(data.cities, select = c("Shopping", "Nightlife", "Tours", "Spas.Wellness", "Sights.Landmarks",
                                   "Concerts.Shows", "Museums", "Outdoor.Activities", "Food.Drink"))
  })
  
  # new_df1 <- reactive({
  #   subset(data.flights, select = c("FP1", "FP2" ,"FP3", "FP4" , "FP5", "FP6", "FP7" ,"FP8", "FP9", "FP0"))
  # })
  
  result.destination <- reactive({
    # Apply calculate.euclidean() to each row of the subsetted data
    cf <- apply(new_df(), 1, calculate.euclidean)
    # Add computed distance as a column to the existing data frame
    data.cities$distance <- cf
    # Order new data by distance in ascending order
    df.cities.ordered <- data.cities[order(data.cities$distance),]
    # Return 'label' column from the first row
    # This is the destination which is most similar to the input vector
    df.cities.ordered[1, "label"]
  })
  
  output$result.text <- renderText({
    paste0("You should go to: ", result.destination())
  })
}

shinyApp(ui, server)