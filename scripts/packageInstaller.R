suppressPackageStartupMessages({
  library(devtools)
  library(dplyr)
  library(purrr)
  library(magrittr)
  library(here)
})

installNeededPackage <- function(packageName, packageVersion, packageRepo, packageRepoName) {
 message("\nInstalling package: ", packageName, " ", packageVersion)
 if (packageRepo == "github") {
   if (!is.na(packageVersion)){
     devtools::install_github(packageRepoName,
                              ref = packageVersion)
   } else {
     devtools::install_github(packageRepoName)
   }

 } else {
   devtools::install_version(packageName,
                             version = packageVersion,
                             repos = "http://cran.us.r-project.org")
 }
}

logOnError <- function(expression) {
  tryCatch(expression, error = function(e) { message(e) })
}

message("==> Log for package installer.")

logOnError({
  neededPackages <- read.csv(here("metadata", "packageInfo.csv"), stringsAsFactors = FALSE)
})

actualPackages <- installed.packages()[, c("Package", "Version")] %>%
  as.data.frame(stringsAsFactors = FALSE, row.names = FALSE)

`%=%` <- function(x,y) {
  ifelse(is.na(y) | x != y, FALSE, TRUE)
}

logOnError({
  toInstall <- dplyr::left_join(neededPackages,
                                actualPackages,
                                by = "Package") %>%
    dplyr::filter(!Version.x %=% Version.y) %>%
    dplyr::select(-Version.y) %>%
    dplyr::rename(Version = Version.x)
})

if (nrow(toInstall) > 0) {
  1:nrow(toInstall) %>%
    map(
      ~ logOnError({
        installNeededPackage(toInstall[., 1], toInstall[., 2], toInstall[., 3], toInstall[., 4])
      })
    )
}

message("Execution of packageInstaller done!")
