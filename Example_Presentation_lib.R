
## These are helper functions for the Example_Presentation

library(readr)
library(ggplot2)
library(dplyr)
library(choroplethr)
library(choroplethrMaps)

## This function below is meant to provide many of the "basic"
## improvements to a plot which are nearly always needed.
theme_improve <- function(my_plot, title_color= 'brown',
                          title_size= 2, axis_title_size=1.5,
                          axis_text_size=1.3,
                          x_axis_text_angle=30) {
  my_plot = my_plot +
    theme(plot.title = element_text(size = rel(title_size),
                                    color = title_color),
          axis.title.y = element_text(size = rel(axis_title_size)),
          axis.title.x = element_text(size = rel(axis_title_size)),
          axis.text.x = element_text(size = rel(axis_text_size),
                                     angle = x_axis_text_angle),
          axis.text.y = element_text(size = rel(axis_text_size))
    )
  return(my_plot)
}


## This palette of colors is more closely aligned with our
## thinking of environmental being green, and non-environmental
## being brown.
environ_clrs <- rev(c(
  "#662506",
  "#800000", # full brown
  "#993404",
  "#BF812D",
  "#DFC27D",
  "#B8E186",
  "#7FBC41",
  "#4D9221" # full green
))

## The years needed to be captured as factors for the plots,
## and as factors, every single "level" would be plotted.
## Here, I could filter down to a subset that I'd like to see,
## to help reduce clutter along the axis.
years <- c(
  1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010
)

## European countries with mis-matched names across choroplethr
## map and our data set.
country_fixes1 <- c(
  "croatia",
  "macedonia",
  "slovakia"
)
country_fixes2 <- list(
  c("bosnia - herzegovina", "bosnia and herzegovina")
)


## All relevant European countries, so that we can zoom in on
## only these when making the choropleths.
euro_countries <- c(
  "albania",
  "austria",
  "belgium",
  "bosnia and herzegovina",
  "bulgaria",
  "croatia",
  "cyprus",
  "czech republic",
  "denmark",
  "estonia",
  "finland",
  "france",
  "germany",
  "greece",
  "hungary",
  "iceland",
  "ireland",
  "italy",
  "latvia",
  "lithuania",
  "luxembourg",
  "macedonia",
  "montenegro",
  "netherlands",
  "norway",
  "poland",
  "portugal",
  "romania",
  "slovakia",
  "slovenia",
  "spain",
  "sweden",
  "switzerland",
  "turkey",
  "united kingdom"
)


library(stringi)
subHtmlRender <- function(mdfile, htmlfile) {
  #replace <<insertHTML:htmlfile with actual html code
  #but without beginning white space
  lines <- readLines(mdfile)
  toSubcode <- paste0("<<insertHTML:[",htmlfile,"]")
  location <- which(stri_detect_fixed(lines, toSubcode) )
  htmllines <- stri_trim(readLines(htmlfile))
  
  #render html doc
  newRmdfile <- tempfile("temp", getwd(), ".Rmd")
  newlines <- c(lines[1:(location-1)],
                htmllines,
                lines[min(location+1, length(lines)):length(lines)])  #be careful when insertHTML being last line in .Rmd file
  write(newlines, newRmdfile)
  rmarkdown::render(newRmdfile, "html_document")
  system(gsub(".Rmd",".html",basename(newRmdfile),fixed=T))
} #end subHtmlRender







