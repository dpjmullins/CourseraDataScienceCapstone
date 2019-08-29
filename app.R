# Shiny App Code

## The 'app.R' file for each app should live in its own directory

# Libraries
library(shiny)
library(ggplot2)

## Set up dataset
dataset <- as.data.frame(Titanic)
total_passengers <- sum(dataset$Freq)
total_survivors <- sum(dataset[dataset$Survived == "Yes",]$Freq)

# Define UI (User Interface) function for the shiny app
## fluidPage() automaticallu adjusts to a user's window
ui <- fluidPage(

    ## Set the title of the app
    titlePanel("Coursera Data Science Specialisation - Capstone Project - Text Prediction App"),
 
    ## set CSS styling for formatting fluidRows
    tags$style(HTML("
        #box1 {
            border: 4px outset SandyBrown;
        }
        #box2 {
            border: 4px outset SkyBlue;
        }
    ")),

    ## Sidebar panel for controlling inputs
    fluidRow( id = "box1", column(width=10,
        
        ## Sentence input for next word prediction
        textInput(
            inputId = "predictor_words",
            label = h3("Input a partial sentence for next word prediction in the bow below:"),
            value = "welcome my dear"
        ),
        h4("Don't forget to press 'Predict!'"),
        ## Submit button
        submitButton(text = "Predict!"),
        br(), br(), br() ## breaks to separate input and output in space
  
    )),
    
    fluidRow( id = "box2", column(width=10,
        h3("The ", tags$b(" predicted "), " next word in the sentence:"),
        tags$em(p("The algorithm predicts off the final 3 words of the sentence.")),

        ## Output text
        h1(span(textOutput(outputId = "predicted_word"), style = "color:blue"))
    ))
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    output$predicted_word <- renderText({ 
        source('preprocessing_functions.R')
        source('03_prediction.R')
        input.processed <- sapply( text_transformer( Corpus( VectorSource( input$predictor_words ) ) ), identity )
        predicted_word <- predictor.func(input.processed)
        predicted_word
    })

}

## Call the ui and server functions
shinyApp(ui = ui, server = server)

## Code to run app
### 'display.mode = "showcase"' displays the code alongside the app
#runApp("ds_capstone_app", display.mode = "showcase")
