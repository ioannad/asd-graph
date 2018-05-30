require(stringi)
require(shiny)
require(DiagrammeR)

ui <- fluidPage(h1("asd-graph in R"),
                p("This small utility uses graphviz dot to visualise the dependencies declared in a Common Lisp <system-name>.asd file."),
                fileInput(inputId="asd",
                          label="Upload your *.asd file here.",
                          accept=".asd"),
                textOutput(outputId="test", container=div),
                grVizOutput("graph")
                )

server <- function(input, output) {

    asd <- reactive(input$asd)
    output$graph <- renderDiagrammeR({
        evalexpression <- paste0("'(asd->dot \"",
                                 "~/quicklisp/local-projects/jeffrey/jeffrey.asd",
                                 "\" *standard-output*)'")
        dottext <- system2('sbcl',
                           args=c("--noinform", "--load", "\"asd-graph.lisp\"",
                                  "--eval", evalexpression, "--eval", "'(quit)'"),
                           stdout = TRUE)
        DiagrammeR::grViz(diagram = dottext)
    })
}
                  
    
    
    
shinyApp(ui=ui, server=server)
