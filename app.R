#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(poweRlaw)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$head(tags$link(rel = 'stylesheet', type="text/css", href="stylesheet.css")),
  
  titlePanel(h1("To Zipf or not to Zipf", style = "margin-bottom:5px")),
  p(HTML("Your friendly  neighbourhood Zipf plotter"), style="font-size:14px; color: #808182;"),
  h3("A little introduction"),
  
p(withMathJax(HTML(paste0("Zipf's law was first formulated by George Zipf in 
1936 in his famous book ", 
a(href='https://books.google.co.uk/books?id=Mcj7AQAAQBAJ&lpg=PP1&ots=npXS9hdP6i&dq=G.%20Zipf.%20The%20Psycho-Biology%20Of%20Language.%201936&lr&hl=cs&pg=PR8#v=onepage&q&f=false'
,"The Psycho-Biology
    of Language"))), 
"where he states that in both natural and man-made languages, an interesting 
frequency distribution can be observed. This frequency distribution states that every
rth most frequent word in a language should appear with a frequency f(r) that scales according
to: ")),

withMathJax("$$f(r)=\\frac{1}{r^\\alpha}=r^{-\\alpha}$$"),

p(HTML(paste0("where the scaling factor alpha is said to be close to 1. That means that the second
  most frequent word in any language should appear approximately 1/2 as many times as the
  most frequent, the third most frequent word would appear approximately 1/3 as often 
  and so on. This pattern has been empirically observed in many languages, even")),
a(href="https://arxiv.org/pdf/0808.2904.pdf", "extinct ones"), "and has been shown to hold 
for the ", a(href="https://arxiv.org/pdf/1402.2965.pdf", "sizes of cities"), "as well."),
  
h3("Does it actually hold?"),
p("Scientists have been disputing whether or not Zipf's law holds in this exact form"),
h3("How to calculate it?"),

  sidebarLayout(
    position = "right",
    sidebarPanel(
      plotOutput("mainplot"), width=8
    ),
    mainPanel(
      p("Upload your text", style = "font-family: 'Helvetica'; 
                                    font-size: 16px"),
      fileInput("file", h3("File upload")),
      width=4
    )
  ),
  
  p("output", label = "endtext")
)

# Define server logic required to draw a histogram
server <- function(input, output){
  #output$endtext <- renderText({ 
    
 # })
  
  output$mainplot <- renderPlot({
    data <- scan(input$file$datapath)
    distr <- displ$new(data)
    est <- estimate_xmin(distr)
    est
    distr$setXmin(est)
    est <- estimate_pars(distr)
    distr$setPars(est)
    distr$getXmin()
    distr$getPars()
    plot(distr)
    lines(distr, col=2)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

