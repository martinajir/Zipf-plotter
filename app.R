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

# define text mining functions
getWordCount <- function(file, nameFlag = FALSE){
  # create corpus
  data <- tm::PlainTextDocument(readr::read_lines(file, skip=0, n_max=-1L))
  corpus <- Corpus(VectorSource(data))
  
  # clean up input
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  # attempt to replace common punctuation/symbols with space instead
 # inputTransformer <- content_transformer(content_transformer(function(x, pattern) {return (gsub(pattern, " ", x))}))
#  corpus <- tm_map(corpus, inputTransformer, "-")
  #corpus <- tm_map(corpus, inputTransformer, ":")
  #corpus <- tm_map(corpus, inputTransformer, "/")
  #corpus <- tm_map(corpus, inputTransformer, ";")
  #corpus <- tm_map(corpus, inputTransformer, "'")
  #corpus <- tm_map(corpus, inputTransformer, "¨")
  
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
  
  dtm <- DocumentTermMatrix(corpus, control = ctrl)
  
  # this produces a named vector with words as names and frequencies as values
  freq <- termFreq(data, control = ctrl)
  sort.freq <- sort(freq)
  strippedFreq <- as.numeric(sort.freq) # unname vector
 
   if(nameFlag==TRUE){
    return(sort.freq)
  }
  else {
    return(strippedFreq)
  }
}


# Define UI for the application
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
  Lastly, and most importantly, goodness-of-fit tests are performed to see, whether it is plausible to fit power-law behavior to our empirical data."),

  sidebarLayout(
    position = "right",
    sidebarPanel(
      plotOutput("mainplot"), width=8
    ),
    mainPanel(
      h2("Upload your text (.txt only)"),
      fileInput("file", h3("File upload")),
      width=4
    )
  ),
  
  plotOutput("endtext")
)

# Define server logic
server <- function(input, output){
  
  # starting to implement word count
  output$endtext <- renderPlot({
    wc <- getWordCount(input$file$datapath, TRUE)
    plot(y = wc,x = 1:length(wc), ylab = "word frequency", ylim=c(1,max(wc)), xlab = "frequency rank")
    #xlab=names(wc)
  })
  
  # main Zipf distr plot
  output$mainplot <- renderPlot({
    # this used to work when provided with just frequencies
    # data <- scan(input$file$datapath)
    data <- getWordCount(input$file$datapath)
    distr <- displ$new(data)
    est <- estimate_xmin(distr)
    # est debugging
    distr$setXmin(est)
    est <- estimate_pars(distr)
    distr$setPars(est)
    distr$getXmin()
    distr$getPars()
    plot(distr, ylab= "CDF")
    lines(distr, col=2)
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

