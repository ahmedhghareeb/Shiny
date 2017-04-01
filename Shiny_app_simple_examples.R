library(shiny)
library(ggplot2)
library(randomForest)


# Data Processing ---------------------------------------------------------

getdfnew <- function(df){
  # clean up data frame to set types appropriately
  df$Compressor.Temperature.F <- as.numeric(df$Compressor.Temperature.F)
  # Build the training/validate/test datasets.
  nobs <- nrow(df)
  ntr <- 0.2*nobs # assumes first 20% of data tunes good system
  set.seed(42)
  str(df)
  indices.train <- 1:ntr # 
  indices.apply <- (ntr+1):nobs # 
  
  input.variables <- c("Ambient.Air.Temperature.F",     
                       "Condenser.Coil.Temperature.F","Control.Temperature.Setpoint.F",
                       "Evaporator.Coil.Temperature.F","Return.Air.Temperature.F",      
                       "Supply.Air.Temperature.F")
  
  input.numbers<- c("Ambient.Air.Temperature.F",     
                    "Condenser.Coil.Temperature.F","Control.Temperature.Setpoint.F",
                    "Evaporator.Coil.Temperature.F","Return.Air.Temperature.F",      
                    "Supply.Air.Temperature.F")
  
  target.variable  <- "Compressor.Temperature.F"
  
  set.seed(42)
  result.rf <- randomForest::randomForest(Compressor.Temperature.F ~ .,
                                          data=df[indices.train ,c(input.variables, target.variable)], 
                                          ntree=500,
                                          mtry=3,
                                          importance=TRUE,
                                          na.action=randomForest::na.roughfix,
                                          replace=FALSE)
  
  # Get predicted and actual values
  predicted.training <- result.rf$predicted
  actual <- df$Compressor.Temperature.F
  
  # Apply model to new data -------------------------------------------------
  
  df.apply <- df[indices.apply, c(input.variables, target.variable)]
  
  # Lets say we had new data ---- data.new
  # It's essential that the new data have the same input columns and target(s)
  #  new.data <- newdataset[1:nrows(newdataset),c(crs$input, crs$target)]
  
  # predict for each of the validate indices the wear condition
  predicted.apply <- predict(result.rf, df.apply , type="response",
                             norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
  

  predicted.all <- vector(length=nobs)
  predicted.all[1:ntr] <- predicted.training
  predicted.all[(ntr+1):nobs] <- predicted.apply
  
  df$Predicted <- predicted.all
  colnames(df)[12] <- "Predicted"
  
  return(df)
}


# Server ------------------------------------------------------------------


server <- function(input, output, session) { 
  output$plot.compressor.temp <- renderPlot({
    # get our dataframe from the selected file 
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    df <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                   quote=input$quote)

    df.new <- getdfnew(df)   # sends data to processing function ...returnns data
     # with forecasted compressor temp. 
    
    
  })
} 




# UI ----------------------------------------------------------------------


#  Executable ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Forecast Compressor Temperature"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"'),
      tags$hr(),
      p('If you want a sample .csv or .tsv file to upload,',
        'you can first download the sample',
        a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
        a(href = 'pressure.tsv', 'pressure.tsv'),
        'files, and then try uploading them.'
      )
    ),
    mainPanel(
      
      plotOutput('plot.compressor.temp')
    )
  )
)
  


shinyApp(ui = ui, server = server) # this launches your app
