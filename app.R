require(stringi)
require(shiny)
require(DiagrammeR)

ui <- fluidPage(h1("asd-graph in R"),
                p("This small utility uses graphviz dot to visualise the dependencies declared in a Common Lisp <system-name>.asd file."),
                fileInput(inputId="asd",
                          label="Upload your *.asd file here.",
                          accept=".dot"),
                textOutput(outputId="test",
                           container=div,
                           inline=false),
                grVizOutput("graph")
                )

server <- function(input, output) {
    output$graph <- renderDiagrammeR(
        DiagrammeR::grViz(diagram = "
digraph 0 {
splines=ortho;
rankdir = LR;
node [shape=box];\"read\" -> \"graph\";
\"read\" -> \"parse\";
\"read\" -> \"process-strings\";
\"predicates\" -> \"graph\";
\"predicates\" -> \"read\";
\"draw\" -> \"graph\";
\"draw\" -> \"read\";
\"draw\" -> \"predicates\";
\"labelmaker\" -> \"graph\";
\"labelmaker\" -> \"read\";
\"main\" -> \"graph\";
\"main\" -> \"read\";
\"main\" -> \"predicates\";
\"main\" -> \"draw\";
\"latex-in-html\" -> \"graph\";
\"web-graph\" -> \"main\";
\"website\" -> \"parse-web-input\";
\"website\" -> \"latex-in-html\";
\"website\" -> \"encode-filenames\";
\"example-form-groups\" -> \"main\";
\"test\" -> \"graph\";
\"test\" -> \"read\";
\"test\" -> \"predicates\";
}
"))
}
    
    
shinyApp(ui=ui, server=server)
