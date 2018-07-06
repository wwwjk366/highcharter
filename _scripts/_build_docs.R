# build site --------------------------------------------------------------
pkgdown::build_site("../highcharter/.", path = "../highcharter-gh-pages/docs/",
                    examples = FALSE, mathjax = FALSE)

# adding style ------------------------------------------------------------
fls <- dir("docs/", full.names = TRUE, recursive = TRUE, pattern = ".html")

theme <- "lumen"

link <- "<link href=\"https://cdnjs.cloudflare.com/ajax/libs/bootswatch/4.1.1/lumen/bootstrap.css\" rel=\"stylesheet\" crossorigin=\"anonymous\">"
link <- sprintf(link, theme)

map(fls, function(f){
  message("Changing css '", f, "'")
  # f <- sample(fls, size = 1)
  txt <- readLines(f)
  
  plc <- which(grepl("<!-- Bootstrap -->", txt))
  
  txt <- c(txt[1:plc], link, txt[(1+plc):length(txt)])
  
  writeLines(txt, f)
  
})




