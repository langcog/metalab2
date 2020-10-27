# Developing for Metalab

Building MetaLab locally will allow you to make changes, including
text or data updates, adding new pages, or enhancing Shiny
applications.

## Software Requirements (one-time setup)

R >= 4.02
Hugo Extended >= v0.74.3

The easiest way to install Hugo is to use the blogdown package Hugo
installer. Open this project in R Studio, and run the following
commands:

```
renv::restore()
blogdown::install_hugo()
```

## Updating to latest data (optional)

If the spreadsheets have been updated and you want to try the latest
available data, you can run:

```
source(here::here("build", "update-metalab-data.R"))
```

If you do not run this command, you will be using the dataset that the
current MetaLab site uses.

## Building MetaLab (each time you want to make changes)

```

source(here::here("build", "build-metalab-site.R"))
blogdown::serve_site()
```
