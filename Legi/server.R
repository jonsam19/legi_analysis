server <- function(input, output, session) {

    forecast_plot <- reactive(
            
            if(input$select_gender=="All"){
                ECDC_plot(
                forecast(
                    legi_tsibble %>%
                        summarise(cases=sum(nb)) %>% filter_index(.~"2016 W52") %>%
                        model(ARIMA(log(cases+1)~fourier(K=3)+1+pdq(1,1,1)+PDQ(0,0,0))),h=156) %>% 
                    
                    ## include 80% and 90% confidence intervals
                    hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
                    rename(
                        upper_95 = '95%_upper',
                        upper_80 = '80%_upper',
                        lower_95 = '95%_lower',
                        lower_80 = '80%_lower') %>% 
                    
                    ## join observed cases
                    left_join(legi_tsibble %>%
                                  summarise(cases=sum(nb)) %>% 
                                  filter_index("2017 W01"~.), 
                              by="week") %>% rename("observed"="cases.y"), 
                legi_tsibble %>% summarise(cases=sum(nb)))
                } else {
                    ECDC_plot(
            forecast(
                legi_tsibble %>% filter(Gender==input$select_gender) %>% 
                    summarise(cases=sum(nb)) %>% filter_index(.~"2016 W52") %>%
                    model(ARIMA(log(cases+1)~fourier(K=3)+1+pdq(1,1,1)+PDQ(0,0,0))),h=156) %>% 
                
                ## include 80% and 90% confidence intervals
                hilo(level=c(80,95)) %>% unpack_hilo("80%") %>% unpack_hilo("95%") %>% 
                rename(
                    upper_95 = '95%_upper',
                    upper_80 = '80%_upper',
                    lower_95 = '95%_lower',
                    lower_80 = '80%_lower') %>% 
                
                ## join observed cases
                left_join(legi_tsibble %>% filter(Gender==input$select_gender) %>%
                              summarise(cases=sum(nb)) %>% 
                              filter_index("2017 W01"~.), 
                          by="week") %>% rename("observed"="cases.y"), 
            legi_tsibble %>% filter(Gender==input$select_gender) %>% summarise(cases=sum(nb)))
                }
    )
    
## output
    output$forecast_plot <- renderPlotly(ggplotly(forecast_plot()))
}