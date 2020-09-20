## Building Metalab locally on Windows:
#### After cloning this repo, you can install all required R packages using
```
renv::restore()
```
#### To build the website and Shiny applications locally
```
source(here::here("build", "build-metalab-site.R"))
```
#### Install hugo using blogdown
```
blogdown::install_hugo()
```
#### Install Go and make sure it runs using the steps in https://golang.org/doc/install
#### Serve MetaLab website on at http://localhost:4321
```
blogdown:::serve_site()
```
