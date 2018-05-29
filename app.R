require(stringi)
require(shiny)

ui <- fluidPage(h1("asd-graph in R"),
                p("This small utility uses graphviz dot to visualise the dependencies declared in a Common Lisp <system-name>.asd file."),
                fileInput(inputId="asd",
                          label="Upload your *.asd file here.",
                          accept=".asd"),
                textOutput(outputId="test",
                           container=div,
                           inline=false)
                )

server <- function(input, output) {
    output$test <- renderText({
        file <- input$asd
        rawText <- readLines(file$datapath)
        com <- paste0("sbcl --load asd-graph.lisp --eval \'(asd-graph \"~/quicklisp/local-projects/jeffrey/jeffrey.asd\" :output-dir \"~/quicklisp/local-projects/asd-graph/\")\'")
        system(com)
        })
}

shinyApp(ui=ui, server=server)
