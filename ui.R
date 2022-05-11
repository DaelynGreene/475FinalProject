ui <- dashboardPage(
  dashboardHeader(title = "Daelyn 475 Final"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to Use", tabName = "Tab1"),
      menuItem("Now You Try - Graph Viewing", tabName = "Tab2"),
      menuItem("Interpretations - Graph Viewing", tabName = "Tab3"),
      menuItem("Now You Try - Forecasts", tabName = "Tab4"),
      menuItem("Interpretations - Forecasts", tabName = "Tab5")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Tab1",
        
        
        h3("Upon opening the app, the user needs to input their search term of interest into the text box."),
        
        
        br(),
        
        textInput("EXAMPLE1","What do you want to search for?",placeholder = "Lindsey Stirling"),
      
        h5("NOTE: The timeframe of the search will be for all data that Google Trends has."),
        
        br(),
        
        actionButton("EXAMPLE2","Go"),
        
        br(),
        
        h3("Once Go is clicked, code magic happens in the background. The data is gathered, cleaned, and stored, AND a graph of the data is displayed."),
        
        h3("As an example, I have run this process for the search term Lindsey Stirling."),
        
        plotlyOutput("TutorialGraph"),
        
        br(),
        
        h3("Once you've looked at the graph of the full data until you've gone crazy, you can then click on one of the following three Radio Buttons to display more graphs."),
        
        br(),
        
        prettyRadioButtons(
          inputId = "EXAMPLE3",
          label = "What graph do you want to see in addition to the full series graph?", 
          choices = c("Seasonality", "Autocorrelation", "Decomposition"),
          icon = icon("check"), 
          bigger = TRUE,
          inline = T,
          animation = "tada",
          selected = ""
        ),
        
        h3("This is the output for when Seasonality is clicked."),
        
        plotlyOutput("TutorialSeason"),
        
        br(),
        
        h3("This is the output for when Autocorrelation is clicked."),
        
        plotOutput("TutorialAuto"),
        
        br(),
        
        h3("This is the output for when Decomposition is clicked."),
        
        plotlyOutput("TutorialDecomp"),
        
        h3("Once you have gotten to this point, you are done with learning the first part of the app. You can now go over to the Interpretations - Graph Viewing tab to learn how to understand what you have seen, OR, if you want to be adventurous, jump right into the Now You Try - Graph Viewing tab."),
        
        br(),
        
        br(),
        
        h3("The second part of the app has to do with forecasting the future search interest. The user will select which model they want to use to model their data and create the forecast from."),
        
        br(),
        
        prettyRadioButtons(
          inputId = "EXAMPLE4",
          label = "What model do you want to use to forecast the future of your search term?", 
          choices = c("Naive", "Seasonal Naive", "Mean", "Drift", "Holts", "Holts/Winters", "ARIMA"),
          icon = icon("check"), 
          bigger = TRUE,
          inline = T,
          animation = "tada",
          selected = ""
        ),
        
        br(),
        
        h3("The graph will be displayed based on the user's selection. The model will be in blue, the original data will be in black. This is the graph of the Naive model."),
        
        plotOutput("TutOut"),
        
        br(),
        
        h3("Now that you have made it here, you are finished with the app. Head over to Interpretations - Forecast to see how to understand the forecast graph or go to Now You Try - Forecasts to try out your own search.")
        
        
      ),
      
      tabItem(
        tabName = "Tab2",
        
        textInput("SearchKeyword","What do you want to search for?",placeholder = ""),
        
        prettyRadioButtons(
          inputId = "GraphToDisplay",
          label = "What graph do you want to see in addition to the full series graph?", 
          choices = c("Seasonality", "Autocorrelation", "Decomposition"),
          icon = icon("check"), 
          bigger = TRUE,
          inline = T,
          animation = "tada",
          selected = ""
        ),
        
        actionButton("Go","Go"),
        
        br(),
        
        br(),
        
        plotlyOutput("UserMade"),
        
        br(),
        
        uiOutput("Selected")
        
      ),
      
      tabItem(
        tabName = "Tab3",
        
        h3("The full series and seasonality graphs are both fairly self-explanatory..."),
        h3("The full graph visualizes the data collected from Google Trends and the seasonal graph shows a graph of each year overlayed on each other to better show if there is any seasonality."),
        br(),
        br(),
        h3("The autocorrelation graph is where things start to get interesting. Look at the vertical bars on the autocorrelation graph. If any of them are above the dashed horizontal blue line, that is a highly correlated lag."),
        h3("A highly correlated lag, in simple terms, just means that, at whatever month the bar is located at, the value at this prior month is a good predictor of the value of the current month. If the high bar is at month 1, the previous month is a good predictor of this month. If the highest bar is at month 12, the value in this month last year is a good predictor for the value this month."),
        br(),
        br(),
        h3("The decomposition graph, while scary looking at first, really isn't that bad. The graph at the top is the initial full series. The next graph is a graph of the trend. The next is a graph of the seasonal component of the initial series. The last is a graph of the remainder (that is, the things that are just by random chance)."),
        h3("The way to tell which is the most impactful is to look at the scale of the y axis. The item with the largest scale is the most impactful. If two (or all three) have the same scale, you then need to look at the little rectangles. The rectangle that covers the largest amount is the most impactful.")
      ),
      
      
      tabItem(
        tabName = "Tab4",
        
        textInput("SearchKeyword2","What do you want to search for?",placeholder = ""),
        
        prettyRadioButtons(
          inputId = "ForecastChoice",
          label = "What model do you want to use to forecast the future of your search term?", 
          choices = c("Naive", "Seasonal Naive", "Mean", "Drift", "Holts", "Holts/Winters", "ARIMA"),
          icon = icon("check"), 
          bigger = TRUE,
          inline = T,
          animation = "tada",
          selected = ""
        ),
        
        actionButton("Go2","Go"),
        
        br(),
        
        plotlyOutput("UserMade2"),
        
        br(),
        
        plotOutput("ForecastOutput")
        
      ),
      
      
      tabItem(
        tabName = "Tab5",
        
        h3("The forecast graph is fairly self-explanatory. Depending on which option is selected, a model is generated. The forecast for the next year of trends is created and graphed along with the original data."),
        
        br(),
        
        h3("The shaded band around the forecast describes the level of confidence of the prediction. The 80% band means that we are 80% confident that the true value of our data falls in that range. The 95% band is the exact same, just with 95% confident instead of 80%.")
    
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ))
)
