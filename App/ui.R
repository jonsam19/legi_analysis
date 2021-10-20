library(plotly)

ui <- fluidPage(
    
    navbarPage("Legi analysis",
               
               tabPanel("Retrospective predictions",
                        
                                             sidebarPanel(h4("Prediction plot"),
                                                          radioButtons("prediction_gender", "Select gender",
                                                                       choices=list("All"="All",
                                                                                    "Female"="F",
                                                                                    "Male"="M"))),
                                             mainPanel(plotlyOutput("forecast_plot"))),
               
               
               
               tabPanel("Interrupted time series",
                        
                        sidebarPanel(h4("ITS plot"),
                                     radioButtons("its_gender", "Select gender",
                                                  choices=list("All"="All",
                                                               "Female"="F",
                                                               "Male"="M"))),
                        mainPanel(plotlyOutput("its_plot")))
                                   
               
    )
)