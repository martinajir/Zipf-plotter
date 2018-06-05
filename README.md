# Zipf-plotter
A[shiny](https://shiny.rstudio.com/) app that enables the user to plot the word frequency distribution and see if it fits the Zipf distribution. Uses the [poweRlaw](https://github.com/csgillespie/poweRlaw) package. 

Currently the plotter accepts raw text (.txt) files and graphs the word frequencies on a frequency-rank graph. It analyzes the distribution of word frequencies of the given text and subsequently plots a CDF graph showing the best fit line. The estimated parameters of the distribution are shown and a p-value is calculated to find out whether the word frequency data obtained from the input file can be estimated using the Zipf distribution.
