
library(shiny)

ui <- fluidPage(
    titlePanel("Comparison of Binomial Groups"),

    sidebarLayout(
        sidebarPanel(
            HTML("<h4>Enter the information for your data</h4>"),
            numericInput("nclass", "Number of classes:",
                        value= 4, max=8, min=2, width = 150),
            fluidRow(
                column(5, textInput("x1", HTML("x<sub>1</sub>"), value = "", width = 50)),
                column(5, textInput("n1", HTML("n<sub>1</sub>"), value = "", width = 50))
            ),
            fluidRow(
                column(5, textInput("x2", HTML("x<sub>2</sub>"), value = "", width = 50)),
                column(5, textInput("n2", HTML("n<sub>2</sub>"), value = "", width = 50))
            ),
            conditionalPanel(condition = "input.nclass > 2",           
                fluidRow(
                 column(5, textInput("x3", HTML("x<sub>3</sub>"), value = "", width = 50)),
                 column(5, textInput("n3", HTML("n<sub>3</sub>"), value = "", width = 50))
                )
            ),
            conditionalPanel(condition = "input.nclass > 3",
                fluidRow(
                 column(5, textInput("x4", HTML("x<sub>4</sub>"), value = "", width = 50)),
                 column(5, textInput("n4", HTML("n<sub>4</sub>"), value = "", width = 50))
                )
            ),
            conditionalPanel(condition = "input.nclass > 4",
                fluidRow(
                 column(5, textInput("x5", HTML("x<sub>5</sub>"), value = "", width = 50)),
                 column(5, textInput("n5", HTML("n<sub>5</sub>"), value = "", width = 50))
                 )
            ),
            conditionalPanel(condition = "input.nclass > 5",
                fluidRow(
                 column(5, textInput("x6", HTML("x<sub>6</sub>"), value = "", width = 50)),
                 column(5, textInput("n6", HTML("n<sub>6</sub>"), value = "", width = 50))
                )
            ),
            conditionalPanel(condition = "input.nclass > 6",
                fluidRow(
                 column(5, textInput("x7", HTML("x<sub>7</sub>"), value = "", width = 50)),
                 column(5, textInput("n7", HTML("n<sub>7</sub>"), value = "", width = 50))
                )
            ),
            conditionalPanel(condition = "input.nclass > 7",
                 fluidRow(
                   column(5, textInput("x8", HTML("x<sub>8</sub>"), value = "", width = 50)),
                   column(5, textInput("n8", HTML("n<sub>8</sub>"), value = "", width = 50))
                 )
            )                 
            
            
            
        ),
        mainPanel(
           tableOutput("txt3"),
           HTML("<hr>"),
           uiOutput("txt1"),
           uiOutput("txt2")
        )
    )
)

server <- function(input, output) {

   check.expression <- shiny::reactive({
        inp.x <- c(input$x1, input$x2, input$x3, input$x4, 
                   input$x5, input$x6, input$x7, input$x8)[1:input$nclass]
        inp.n <- c(input$n1, input$n2, input$n3, input$n4, 
                   input$n5, input$n6, input$n7, input$n8)[1:input$nclass]                   
        test <- all(c(inp.x, inp.n)!="")
        shiny::validate(shiny::need(test, ""))
    })

    data <- reactive({
        check.expression()
        n1 <- c(input$n1, input$n2,input$n3, input$n4, 
               input$n5, input$n6, input$n7,input$n8)
        x <- c(input$x1, input$x2,input$x3, input$x4, 
               input$x5,input$x6, input$x7, input$x8)
        n <- as.numeric(n1[1:input$nclass])
        x <- as.numeric(x[1:input$nclass])        
        list(tbl=cbind(x, n-x), n=n1)
    })
    
    p.val <- reactive({
            check.expression()
        p <- round(chisq.test(data()[[1]])$p.value, 4)
        ifelse(p<0.001, "0.000", p)
    })        
    
    pairwise <- reactive({
            check.expression()
            x <- data()[[1]]
            method <- "holm"
            m <- dim(x)[1]
            n <- x[, 2]+x[, 1]
            if(is.null(rownames(x))) names(n) <- 1:m
            else names(n) <- rownames(x)
            x <- x[, 1]
            names(x) <- names(n)
            z <- pairwise.prop.test(x, n, 
                                    p.adjust.method=method)$p.value
            out <- rep(0, (m-1)^2)
            k <- 0
            for(i in 1:(m-1))
                for(j in 1:(m-1)) {
                    k <- k+1
                    names(out)[k] <- paste(rownames(z)[i], " - ",
                                           colnames(z)[j])
                    out[k] <- z[i, j]
                }
            out <- out[!is.na(out)]
            out <- out[order(out)]
            round(out, 4)
        
    })
    
    output$txt3 <- renderTable({
        check.expression()
        n <- data()[[2]]
        for(i in 2:8)
            if(input$nclass==i & n[i]=="") return("")
        x <- data()[[1]]
        x <- data.frame(
            Group=as.integer(1:input$nclass),
            Correct=as.integer(x[, 1]), 
            Wrong=as.integer(x[, 2]),
            Percentage=x[, 1]/(x[, 1]+x[, 2])*100)
        x[order(x[,4]), ]
    }, digits = 1)
    
    output$txt1 <- renderText({
            check.expression()
        n <- data()[[2]]
        for(i in 2:8)
          if(input$nclass==i & n[i]=="") return("Enter Data")
        paste("<h5>p value of overall test:", p.val(), "</h5>")
    })
    
    output$txt2 <- renderText({
            check.expression()
        m <- input$nclass
        n <- data()[[2]]
        pvals <- pairwise()
        for(i in 2:8)
            if(input$nclass==i & n[i]=="") return("")
        lns <- c("<hr><h5>Pairwise Comparisons (using Holm's Method)</h5>",
                 "<h5>Pairs declared statistically significantly different at the 5% level:</h5>")
        p1 <- pvals[pvals<0.05/m]
        for(i in 1:length(p1))
            lns <- c(lns, paste("<h5>",names(p1)[i]),"</h5>")
        lns <- c(lns, "<hr>", 
                 "<h5>Pairs NOT declared statistically significantly different at the 5% level:</h5>")
        p1 <- pvals[pvals>=0.05/m]
           for(i in 1:length(p1))
              lns <- c(lns, paste("<h5>",names(p1)[i]),"</h5>")
        lns
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
