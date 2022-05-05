ui <- dashboardPage(
  dashboardHeader(title = "Daelyn 475 Midterm"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("How to Use", tabName = "Tab1"),
      menuItem("Now You Try", tabName = "Tab2"),
      menuItem("Interpretations", tabName = "Tab3")
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
        
        h3("Once you have gotten to this point, you are done with learning the app. You can now go over to the Interpretations tab to learn how to understand what you are seeing, OR, if you want to be adventurous, jump right into the Now You Try tab.")
        
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
        
        h5("NOTE: There is an issue somewhere in my code that I cannot find for the life of me. If decomposition is selected you need to scroll down a good bit to see it. If autocorrelation is selected, you need to scroll down some to see it."),
        
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
        
        
        
        
        
        
        
        
      )
    ))
  
  
  
  
)
