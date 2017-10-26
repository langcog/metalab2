serverAddress <- "http://52.24.141.166:3838/"

menuNavbar <- function(relativePath="", isIndex=FALSE) {
  fluidRow(
    column(id = "maxim-width",
      width = 12,
      tags$ul(id = "navb",
        tags$li(class = "navb-header", a(href = paste0(relativePath, "index.html"), "MetaLab")),
        if (!isIndex) {tags$li(a("Home", href = paste0(relativePath, "index.html")))},
        tags$li(a("Analyses", href = paste0(relativePath, "analyses.html"))),
        tags$li(a("Documentation", href = paste0(relativePath, "documentation.html"))),
        tags$li(a("Tutorials", href = paste0(relativePath, "tutorials.html"))),
        tags$li(a("Publications", href = paste0(relativePath, "publications.html"))),
        tags$li(a("About", href = paste0(relativePath, "about.html")))
        )
      )
    )
}

iconWrap <- function(x) {
  shiny::tags$i(class = paste0("fa fa-", x))
}

valueBoxes <- function(values, descriptions=c("Meta-analyses", "Papers", "Effect sizes", "Participants"), icons=c("cubes", "file-text-o", "list", "child")) {
  fluidRow(class = "value-box",
    map(1:4,
      ~ shinydashboard::valueBox(
        format(values[.], big.mark = ","),
        descriptions[.],
        width = 3,
        color = "red",
        icon = iconWrap(icons[.]))
      )
    )
  }


includeRmd <- function(path, shiny_data = NULL) {
  rmarkdown::render(path, quiet = TRUE)
  htmltools::includeHTML(gsub(".Rmd", ".html", path))
}

metricsCounter <- function(database) {
 c(nrow(database),
   sum(select(database, num_papers), na.rm = TRUE),
   sum(select(database, num_experiments), na.rm = TRUE),
   sum(select(database, num_subjects), na.rm = TRUE)
   ) %>% as.integer()
}

responsiveIFrame <- function() {
  iFrameAttribute <- "var navb_h = $('#navb').height() + 5
    $('.parent-cont').css('top', navb_h)"

  tagList(
    tags$script(paste0("$(document).ready(function() {",
                       iFrameAttribute,
                       "});")),
    tags$script(paste0("$(window).on('resize', function () {",
                       iFrameAttribute,
                       "});"))
    )
}

fullWidthPanelBox <- function(header, content) {
  fluidRow(
    column(width = 2),
    column(width = 8,
           fluidRow(class = "panel panel-primary",
                    div(class = "panel-heading", h3(class = "panel-title", header)),
                    div(class = "panel-body", content)
           )
    ),
    column(width = 2)
  )
}
