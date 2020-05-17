# MetaLab

MetaLab is a suite of interactive tools for community-augmented
meta-analysis, power analysis, and experimental planning in cognitive
development research.

## Building MetaLab Locally

For development purposes, you can build MetaLab (datasets, web site,
and Shiny apps) locally. 

### Package Dependencies

If it is your first time running Metalab locally after cloning, or the
package dependencies have been updated, you can install all the
required R packages using renv.

```
## install MetaLab package dependencies
renv::restore()
```

### Developing the website and Shiny applications locally
```
## create the datasets needed by the website and the Shiny apps
source(here::here("scripts", "main_builder.R"))

## build and serve the MetaLab website on http://localhost:4321
metalab_serve(here::here("pages"))

## build and run, e.g., the visualization Shiny app
## (substitute "visualization" with any shiny app directory in ./shinyapps)
options(shiny.autoreload = TRUE)
shiny::runApp(here::here("shinyapps", "visualization")) 
```

### Deploying the Shiny applications to staging environment

To deploy the Shiny applications to the staging environment, create a
pull request to the master branch. This will build and deploy the
Shiny applications to the staging environment. There is not currently
a staging server for the website itself, only for the Shiny applications.

Note: The pull request cannot be initiated from a forked repostitory. 

### Deploying the website and Shiny applications to production environment

To deploy the website and Shiny applications to the production
environment, merge the pull request you created in the step
above. This will build and deploy both the website and the Shiny
applications to the production environment. 

## Project Structure

### Metadata 
The `metadata` directory contains many list-type data that is used
throughout the site. To change it, simply edit the corresponding file
in the `metadata` directory and wait for the site to render again. Use
these metadata files whenever you add or edit a dataset, a domain, a
shiny app, a tutorial tab, a documentation tab, a person in the about
page.

### Adding new or editing documentation/tutorial/report tabs

Each tab in these three pages is an Rmd file under the `documentation`,
`tutorials`, and `reports` directories, respectively. Simply add or
edit an Rmd there. If you're adding a new tab or want to change the
name of the tab, you need to edit the corresponding metadata file in
the `metadata` directory.

### Creating reports - no need to source dashboard/global.R

In the previous version of metalab, all Rmd reports had a line:
`source("../dashboard/global.R", chdir = TRUE)`. This is not needed,
because the build script that renders the Rmd reports already has the
code from that setup script, and its environment is accessible to the
reports.

### Shiny apps

To add a new shiny app or edit a shiny app, look in the `shinyapps`
directory. Don't forget to edit the corresponding metadata file.

All current shiny apps use a common R file `shinyapps/common/global.R`.

Variables that are currently accessible from this file:

    - project_directory - direct path to `metalab2` repository
    - fields  
    - fields_derived  
    - datasets_file  
    - datasets    
    - cached_data  
    - avg_month   
    - all_data  
    - studies   
    - subjects   
    - datasets  

### Editing the other pages

The code for the other pages is in the `pages/` directory.

### Images

Save images in the `pages/images` directory in the correct
subdirectory, and use that path in metadata files when referring to an
image.
