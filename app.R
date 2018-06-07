#
# This is a Shiny web application.
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# import libraries
library(shiny)
library(poweRlaw)
library(tm)
library(tau)
library(readr)
library(wordcloud)

getWordCount <- function(file, nameFlag = FALSE){
  # create corpus
  data <- tm::PlainTextDocument(readr::read_lines(file, skip=0, n_max=-1L))
  corpus <- Corpus(VectorSource(data))
  
  # clean up input
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)

  # create document term matrix, count word frequencies
  ctrl <- list(removePunctuation = TRUE, 
               tokenize = "scan", 
               dictionary = NULL, 
               stopwords = FALSE, 
               stemming = FALSE, 
               removeNumbers = TRUE,
               wordLengths=c(1, Inf))
  
  dtm <<- DocumentTermMatrix(corpus, control = ctrl)
  
  # this produces a named vector with words as names and frequencies as values
  freq <<- termFreq(data, control = ctrl)
  sort.freq <- sort(freq)
  strippedFreq <- as.numeric(sort.freq) # unname vector
 
   if(nameFlag==TRUE){
    return(sort.freq)
  }
  else {
    return(strippedFreq)
  }
}

performCalculation <- function(inputfile){
  data2 <- getWordCount(inputfile)
  distr <<- displ$new(data2)
  est <<- estimate_xmin(distr)
  # debugging  
  # est 
  distr$setXmin(est)
  est2 <<- estimate_pars(distr)
  distr$setPars(est2)
}

performPvalueCalc <- function(){
  presult <<- bootstrap_p(distr,no_of_sims=500, threads= 4)
}

evaluateP <- function(){
  if (presult$p >= 0.1) {
    catflag <<- TRUE
    return(cat("The average calculated p-value for this data set using 500 simulations was ", presult$p, ". ", 
               "This value is larger than or equal to 0.1 and therefore Zipf distribution is a likely fit for this dataset. ",
              "In other words, Zipf's law holds for this text."))
  }
  else {
    catflag <<- FALSE
    return(cat("The average calculated p-value for this data set using 500 simulations was ", presult$p, ". ",
               "The value is below 0.1 and therefore Zipf distribution is not a likely fit for this dataset. ",
               "In other words, Zipf's law does not hold for this text."))
  }
}

evaluateCat <- function(){
  if(catflag == TRUE)
    return(TRUE)
  else
    return(FALSE)
}

# Define UI for the application
ui <- fluidPage(
  
  tags$head(tags$link(rel = 'stylesheet', type="text/css", href="stylesheet.css")),
  tags$head(tags$title("Zipf plotter")),
  
  titlePanel(h1("To Zipf or not to Zipf", style = "margin-bottom:5px")),
  p(HTML("Your friendly  neighbourhood Zipf plotter"), id="topbottomtext"),
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
p(HTML(paste0("Academics have been disputing whether or not Zipf's law holds in this exact form for quite some time.
  While some claim that the law ")), 
  a(href="https://arxiv.org/pdf/cond-mat/0412004.pdf", "holds"), 
  ", others have been claiming ", 
  a(href="http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0009411","the opposite")
  ,"."),
h3("How to calculate it?"),

p(HTML(paste0("The most commonly used procedure to determine if a power-law is present is plotting a log-log graph of frequency and rank and estimating if the slope is close to −1 by using linear regression. 
  This would prove that the scaling parameter α is close to 1. This method is however deemed quite limited and not refined enough. 
  Therefore, Zipf plotter uses a different approach following the procedure by")),
  a(href="https://arxiv.org/abs/0706.1062", "Clauset, Shalizi and Newman"), 
  "which was implemented by Colin Gillespie in the ",
  a(href="https://github.com/csgillespie/poweRlaw", "poweRlaw package"),
  "First of all the lower bound of power-law behavior x-min has to be determined. 
  Then, the scaling parameter α is estimated. 
  Lastly, and most importantly, goodness-of-fit tests are performed to see, whether it is plausible to fit power-law behavior to our empirical data.
  Generally, the higher the p-value is, the more plausible fit the given distribution is. In most cases, if the p-value is above 0.1, we 
  can consider the proposed distribution a plausible fit."),

  sidebarLayout(
    position = "left",
    sidebarPanel(
      h3("Upload your text (.txt only)"),
      fileInput("file", h3("File upload"), accept = c('text/plain', '.txt')),
      width=4
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Wordcloud",
                           plotOutput("wordcloud", height="600px")
                           ),
                  tabPanel("Frequency/Rank Graph", 
                           plotOutput("frequencies", height="600px")
                  ),
                  tabPanel("Zipf CDF Graph",
                           plotOutput("mainplot", height="600px")
                           ),
                  tabPanel("Calculated parameters",
                           verbatimTextOutput("textOutput")),
                  tabPanel("p-value calculation",
                           plotOutput("bootstrap", height= "600px")),
                  tabPanel("Results",
                           textOutput("results"),
                           imageOutput("cat"))
                  
                 
        )
      , width=12
      )
  )
)

# Define server logic
server <- function(input, output){
  
  # control whether a file has been uploaded
  controlVar <- reactiveValues(fileReady=FALSE)
  dat <- NULL
  observeEvent(input$file, {
    controlVar$fileReady <- FALSE
    if(is.null(input$file))
      return()
    else
      inFile <<- input$file$datapath
      controlVar$fileReady <- TRUE
      # to reset global parameters
      getWordCount(inFile) 
      performCalculation(inFile)
      performPvalueCalc()
  })
  
  # reactive
  #getCurrentPars <- reactive({
  #  getWordCount(inFile, TRUE)
  #})
  
  # render word frequency plot
  output$frequencies <- renderPlot({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    
    wc <- getWordCount(inFile, TRUE)
    plot(y = wc,x = length(wc):1, ylab = "word frequency", ylim=c(1,max(wc)), 
         xlim=c(1, length(wc)),
         xlab = "frequency rank")
    
  })
  
  # render main Zipf distr plot
  output$mainplot <- renderPlot({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    performCalculation(inFile)
    plot(distr, ylab= "CDF")
    lines(distr, col=2)
    
  })
  
  # render 3rd tab
  output$textOutput <- renderPrint({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    
    print(est);
    print(est2)
  })
  
  # render 4th tab
  # takes a considerable amount of time
  output$bootstrap <- renderPlot({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    
    validate(
      need(presult, "Please wait")
    )
    plot(presult)
    
  })
  

  output$wordcloud <- renderPlot({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    validate(
      need(presult, "Please wait")
    )
   
    wordcloud(names(freq), freq=freq, min.freq=1, max.words=200, random.order=FALSE, rot.per=0.3, 
              colors = brewer.pal(8, "Dark2"), scale = c(8, 1))
  })
  
  output$results <-renderPrint({
    validate(
      need(controlVar$fileReady, "Please upload a file and then wait for the calculation to finish")
    )
    
    evaluateP()
    
  })
  
  output$cat <- renderImage({
    if(evaluateCat()==TRUE)
      list(src='dependencies/catthumbsup.jpg')
  }, deleteFile=FALSE
  )

}

# Run the application 
shinyApp(ui = ui, server = server)

