#Attaching libraries
libs <- c("shiny", "reshape2", "plotly", "ggplot2")

sapply(libs, library, character.only = T, logical.return = T, 
       quietly = T, warn.conflicts = F)

#Increasing upload size
options(shiny.maxRequestSize=30*1024^2)

#Loading data
#load("test_data.RData")

shinyServer(function(input, output){
    
    datafile <- reactive({
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        dt <- read.csv(inFile$datapath, header=input$header, sep=input$sep)
    })
    
    #dt <- datafile()
    
    output$contents <- renderDataTable(
        datafile(),
        options = list(
            lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
            pageLength = 15)
    )
    
    output$ColumnSelector <- renderUI({
        selectInput("SelectedColumn","Select a column", 
                    choices = names(datafile())[-c(1:6, 38:39)])
    })
    
    dataset <- reactive({
        index <- which(names(datafile()) == input$SelectedColumn)
        to.show <- datafile()[,index]
        to.show
    })
    
    output$bakeryHistogramm <- renderPlot({
        # Render a histograms standard
        #hist(dataset(),
        #     main = input$SelectedColumn,
        #     ylab = "Number of entries",
        #     xlab = "Value")
        # Render a histograms ggplot
        #p <- qplot(dataset(), geom="histogram")
        #print(p)
    })
    
    # Render a histograms plotly
    output$trendPlot <- renderPlotly({

        gg <- qplot(dataset(), 
                    geom="histogram",
                    main = input$SelectedColumn, 
                    xlab = "Value",
                    ylab = "Number of entries")
        # Convert the ggplot to a plotly
        p <- ggplotly(gg)
        p
    })
    
    output$info_test <- renderText({
        #print(input$SelectedColumn)
        if (is.null(datafile())){print("Choose file to upload")} else
        {print(paste("Uploaded", dim(datafile())[1], "rows and", 
                     dim(datafile())[2], "columns"))}
    })
    
    output$info1 <- renderText({
        paste("The file has", dim(datafile())[1], "rows and", dim(datafile())[2], "columns.")
    })
    
    output$info2 <- renderText({
        paste("Columns", paste(names(datafile())[c(3, 38, 39)], sep = " ", collapse = " / "), 
              "look like a date.")
    })
    
    output$info3 <- renderText({
        a <- sapply(datafile(), function(x) sum(is.na(x)))
        empty <- which(a != 0)
        if (length(empty) == 0) {print("There are no empty values in data set")} else 
        {print(paste("Empty columns are:", a[empty]))}
    })
    
    output$info4 <- renderText({
        x = apply(datafile(),2,function(x) all(x==0))
        if (length(x) == 0) {print("There are no zeroed coulmns in data set")} else 
        {print(paste("The following column(s) contain zeroes only", 
                     paste(names(datafile())[which(x)], sep = " ", collapse = " / ")))}
    })
    
    output$info5 <- renderPrint({
        x = apply(datafile(),2,function(x) (length(which(x == 0))/nrow(datafile())))
        round(x[which(x > 0.5)]*100)
    })
    
    output$corr <- renderPrint({
        M <- abs(cor(datafile()[,-c(1:6, 38:39)]))
        diag(M) <- 0
        M[lower.tri(M, diag = TRUE)] <- NA
        M <- subset(melt(M, na.rm = TRUE), value > .85)
        M[order(M$value, decreasing = T),]
    })
    
})#shinyServer

