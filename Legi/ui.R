ui <- fluidPage(
    
    navbarPage("Legi analysis",
               
               tabPanel("Retrospective predictions",
                        
                                             sidebarPanel(h4("Prediction plot"),
                                                          radioButtons("select_gender", "Select gender",
                                                                       choices=list("All"="All",
                                                                                    "Female"="F",
                                                                                    "Male"="M"))),
                                             mainPanel(plotlyOutput("forecast_plot")))
                                   
               
    )
)