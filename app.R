# require(stringi)
require(shiny)
require(DiagrammeR)

ui <- fluidPage(h1("asd-graph in R"),
                p("This small utility uses graphviz dot to visualise the dependencies declared in a Common Lisp <system-name>.asd file."),
                fileInput(inputId="asd",
                          label="Upload your *.asd file here.",
                          accept=".asd"),
#                textOutput(outputId="test", container=div),
                grVizOutput("graph", width = "100%", height = "100%")
                )

server <- function(input, output) {

    asdfile <- reactive({
        asd <- input$asd
        if (is.null(asd)) return(NULL)
        asd})
    
    output$graph <- renderDiagrammeR({
        if (is.null(asdfile())) return(NULL)

        evalexpression <- paste0("\'(asd->dot \"",
                                 asdfile()$datapath,
                                 "\" *standard-output*)\'")

        dottext <- system2('sbcl',
                           args=c("--noinform", "--load", "\"asd-graph.lisp\"",
                                  "--eval", evalexpression, "--eval", "'(quit)'"),
                           stdout = TRUE, wait=T)
        
        DiagrammeR::grViz(diagram = dottext)
    })
}
                  
    
    
    
shinyApp(ui=ui, server=server)
