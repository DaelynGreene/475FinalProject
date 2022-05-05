server <- function(input, output, session) {
  
  output$TutorialGraph <- renderPlotly(ggplot() + geom_line(data = g_trends,aes(x=Month,y=Interest)))
  output$TutorialSeason <- renderPlotly(gg_season(data = g_trends,y = Interest))
  output$TutorialAuto <- renderPlot(autoplot(ACF(g_trends,lag_max = 12)))
  output$TutorialDecomp <- renderPlotly(g_trends %>%
    model(STL(Interest ~ season(window=9), robust=TRUE)) %>%
    components() %>% autoplot())
  
  observeEvent(input$Go,{
    
    if (input$SearchKeyword == "" | is.null(input$GraphToDisplay)) {
      if (input$SearchKeyword == "" & is.null(input$GraphToDisplay)){
        showModal(modalDialog(
          title = "Important Message",
          "Would you kindly input a Search Keyword and a Graph."
        ))
      } else if (input$SearchKeyword == ""){
        showModal(modalDialog(
          title = "Important Message",
          "Would you kindly input a Search Keyword."
        ))
      } else {
        showModal(modalDialog(
          title = "Important Message",
          "Would you kindly input a Graph Type."
        ))
      }
    } else {
      
      UserInputted <- gtrends(keyword = input$SearchKeyword, time = "all")
      UserTrend <- as.data.frame(UserInputted$interest_over_time)
      UserTrend$keyword <- NULL
      UserTrend$geo <- NULL
      UserTrend$time <- NULL
      UserTrend$gprop <- NULL
      UserTrend$category <- NULL
      names(UserTrend) <- c("Month", "Interest")
      UserTrend$Month <- yearmonth(UserTrend$Month)
      UserTrend <- tsibble(UserTrend)
      UserTrend$Interest <- as.numeric(
        ifelse(UserTrend$Interest == "<1", 0, UserTrend$Interest)
      )
      
      output$UserMade <- renderPlotly(ggplot() + geom_line(data = UserTrend,aes(x=Month,y=Interest)))
      
      if (input$GraphToDisplay == "Seasonality"){
        output$Selected <- renderUI({renderPlotly(gg_season(data = UserTrend,y = Interest))})
        output$UserAuto <- NULL
        output$UserDecomp <- NULL
      } else if (input$GraphToDisplay == "Autocorrelation"){
        output$UserSeason <- NULL
        output$Selected <- renderUI({renderPlot(autoplot(ACF(UserTrend,lag_max = 12)))})
        output$UserDecomp <- NULL
      } else {
        output$UserSeason <- NULL
        output$UserAuto <- NULL
        output$Selected <- renderUI({renderPlotly(UserTrend %>%
                                          model(STL(Interest ~ season(window=9), robust=TRUE)) %>%
                                          components() %>% autoplot())})
      }
      
      
    }
    
    
    
    
    
  })
  
  
  
  
}