
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Biodiveristy-stability relationships across taxa

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.13927807.svg)](https://doi.org/10.5281/zenodo.13927807)
<!-- badges: end -->

Here you find the code and data necessary to reproduce the analysis in
the manuscript:

**Rodrigues and Rissanen et al.** *Cross-taxa analysis of long-term data
reveals a positive biodiversity-stability relationship with
taxon-specific mechanistic underpinning*.

## How to use the repository

Download the repository or clone it using the git command:

`git clone https://github.com/avrodrigues/bbs_across_taxa.git`

### Setting the environment

We used the R version 4.2.2 and the [{renv}
package](https://rstudio.github.io/renv/index.html) v0.17.3 to create a
reproducible environment for the analysis.

After download the repository, you should run the code
`renv::restore()`. It will install all the packages needed with the same
version we used to produce the code and the analysis. Since it has
several packages to be installed, this should take some time to be
concluded.

## Repository structure

    #> .
    #> ├── bbs_across_taxa.Rproj
    #> ├── data
    #> ├── function
    #> ├── LICENSE
    #> ├── output
    #> ├── README.md
    #> ├── README.Rmd
    #> ├── renv
    #> └── script

### Main folders

In `data` you find all the data necessary to run the analysis and
compute the diversity and stability metrics.  
In `function` you find the functions created optimize the analysis
workflow.  
In `script` you find the code for the analysis and figures.  
In `output` you find the output from models, figures and table.
