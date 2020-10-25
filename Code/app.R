#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
BodyFatPercentage <- c('2-4%','6-13%','14-17%','18-25%','25%+')
Classification <- c('Essential Fat','Athletes','Fitness','Accepetable','Obese')
CategoryTable <- as.data.frame(cbind(BodyFatPercentage,Classification))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Body Fat Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("age","Age(years):",20),
            numericInput("abdomen","Abdomen(cm):",90),
            numericInput("wrist","Wrist(cm):",18),
            helpText("Note: This calculator is for man only."),
            submitButton("CALCULATE")
        ),

        # Show a result
        mainPanel(
           tabsetPanel(
               tabPanel("Calculation",
                        h4("Result"),
                        textOutput("result"),
                        
                        h4("Body Fat Percentage Categories"),
                        tableOutput("category"),
                        
                        h4("Got Your Calculated Body Fat Percentage?"),  
                        textOutput("got"),
                        uiOutput("url")),
               tabPanel("Contact Info", 
                        textOutput("contact")
                        )
           )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$result <- renderText({
        validate(
            need(is.numeric(input$age), 'Age must be a number!'),
            need(is.numeric(input$abdomen), 'Abdomen must be a number!'),
            need(is.numeric(input$wrist), 'Wrist must be a number!')
        )
        bodyfat <- round(-9.04+0.07*input$age+0.71*input$abdomen-2.24*input$wrist,2)
        if (bodyfat>2 & bodyfat<50)
            paste("Your body fat percentage:",bodyfat,"%")
        else
            print("Oops, the estimated body fat cannot be real, check your input and try again!")
    },)
    output$category <- renderTable({
        test <- CategoryTable
        test
    },width='100%',align = 'l')
    output$got <- renderText({
        "Now you know your body fat percentage, but what does it mean? Get a better understanding of how your body burns fat and what exercises will help you reach your goal."
    })
    output$url <- renderUI({
        url1 <- a("15 Easy Ways to Speed Up Your Metabolism", href="https://www.active.com/fitness/articles/15-easy-ways-to-speed-up-your-metabolism")
        url2 <- a("10 Surprising Reasons You're Not Losing Weight", href="https://www.active.com/fitness/articles/10-surprising-reasons-you-re-not-losing-weight")
        url3 <- a("15-Minute Fat-Blasting Workout for Busy People", href="https://www.active.com/fitness/articles/15-minute-fat-blasting-workout-for-busy-people")
        tagList("URL link:", url1,";",url2,";",url3)
    })
    output$contact <- renderText({
        print("We would love to hear from you. Share your feedback at the developer.
        Email:li2268@wisc.edu \n
        Phone:(608)-9494582")
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
