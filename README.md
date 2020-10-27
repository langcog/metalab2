# Developing for Metalab

Building MetaLab locally will allow you to make changes, including
text or data updates, adding new pages, or enhancing Shiny
applications.

## Software Requirements (one-time setup)

- [R](https://cloud.r-project.org/) >= 4.02
- [Hugo Extended](https://gohugo.io/getting-started/installing/) >= v0.74.3

One easy way to install Hugo Extended is to use the blogdown
package.

```
install.packages("blogdown") #only required if you do not already have it
blogdown::install_hugo()
```

## Install the required packages (one-time setup)

To install all the required R packages on your system, run the
following commad after opening this project in RStudio:

```
renv::restore()
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
site locally on your computer. The build script may take a few minutes
to run. When completed, it will be serving a local copy of the MetaLab
site at http://localhost:4321


```
source(here::here("build", "build-metalab-site.R"))
blogdown::serve_site()
```

## Editing content

You can now try editing existing content in the `content`
directory. Your changes will automatically reload in your web browser.
