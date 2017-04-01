library(shiny)
library(randomForest, quietly=TRUE)
library(ggplot2)


# Forecasting Function ----------------------------------------------------

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


# UI ----------------------------------------------------------------------
ui <- (fluidPage(
  titlePanel("Forecasting Compressor Temperature for T"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
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
                   '"')
    ),
    mainPanel(
      tableOutput('contents'),
      textOutput('str'),
      plotOutput('plot')
    )
  )
))



# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  
  # output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    # inFile <- input$file1
    # 
    # if (is.null(inFile))
    #   return(NULL)
    # 
    # df <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #                quote=input$quote)
   
    # output$str <- renderText({
    #   nobs <- nrow(df)
    # })


    # output$str <- renderText({
    #   nobs <- getnobs(df)
    #   
    # })
    
    output$plot <- renderPlot({
      
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      df <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                     quote=input$quote)
      
      dfnew <-getdfnew(df)
      
      x <- dfnew$Compressor.Temperature.F
      y <- dfnew$Predicted
      # browser()
      p <- ggplot(dfnew, aes(DATE)) + 
        geom_line(aes(y = Compressor.Temperature.F, colour = "Actual")) + 
        geom_line(aes(y = Predicted, colour = "Predicted"))
      print(p)
    })
}


# Shiny App ---------------------------------------------------------------

shinyApp(ui, server)


