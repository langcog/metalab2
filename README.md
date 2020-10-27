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

If you do not run this command, your build will use the dataset that
the current MetaLab site uses.

## Building MetaLab (each time you want to make changes)

You are now ready to build MetaLab. The commands below will serve the
site locally on your computer. The `blogdown::serve_site()` command
may take a few minutes to run. When completed, it will be serving a
local copy of the MetaLab site at http://localhost:4321


```
source(here::here("build", "build-metalab-site.R"))
blogdown::serve_site()
```

## Editing content

You can now try editing existing content in the `content`
directory. Your changes will automatically reload in your web browser.
