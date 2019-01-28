[<img align="left" width="150" height="150" src="https://github.com/CLIMtools/CLIMGeno/blob/master/www/picture2.png">](https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/ "CLIMGeno")

# [CLIMGeno](https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/ "CLIMGeno")
[**GenoCLIM**](https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/) (https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/) is an SHINY component of [**Arabidopsis CLIMtools**](http://www.personal.psu.edu/sma3/CLIMtools.html) (http://www.personal.psu.edu/sma3/CLIMtools.html) originally adapted from the [**Zbrowse viewer**](http://www.baxterlab.org/untitled-cqi0) created by the [**Baxter laboratory**](http://www.baxterlab.org/) intended to explore the genetic variation associated with the environment.

Navigation through the graphs and tables in [CLIMGeno](https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/ "CLIMGeno") is done using the tabs at the top of the page, adjusting options on the sidebar panel, or clicking points on the plots. The user interface is designed to be intuitive and allow the user to quickly zoom into a point of interest anywhere on the genome.

To use this application choose the ExG association of interest from the left panel. The table on this tab provides a summary of the 25 variants with the strongest associations to the selected variable:

1. The Data Table tab provides a full description of the associated variants for the selected environmental variable. The user can easily manage the columns to be shown, rank based on score or q-value, or search and retrieve information for any associated variant.

2. The whole genome view tab provides an interactive Manhattan plot with the variants associated with the environmental variable of interest. The may manage the genetic variants on this plot based on its predicted effect (missense, synonymous variants, etc.). Scrolling over the variants in the plot retrieves the specific information on that variant. Clicking on it, automatically renders the plot shown in the next tab (chromosome view). Alternatively, the user can click and drag over a region of interest.

3. In the chromosome view tab, the user obtains an amplified view of the genetic region of interest. Clicking any variant in the top plot will automatically render in the bottom plot, a window size determined by the user on the left panel. This feature allows for the exploration of the region within linkage disequilibrium for the selected variant. The user can then explore nearby genes as well as other variants associated with the environmental variable of interest within the same genetic region.

4. The annotation table tab, provides an interactive table with the information in the genetic region selected in the previous tab.

This tool provides information on the q-values for all associated variants for the user to impose a particular FDR if desired. We recommend the exploration of the FDR parameters for these ExG association using our [FDRCLIM](https://rstudio.aws.science.psu.edu:3838/aaf11/FDRCLIM/ "FDRCLIM") tool.

We recommend the user of CLIMGeno to become familiar with the [limitations inherent to genome-wide association studies]((https://github.com/CLIMtools/AraCLIM/tree/master/www/myfile.pdf), for which a description is available in the left panel.

For a more detailed description of the logic behind the [Zbrowse viewer](http://www.baxterlab.org/untitled-cqi0) used in this tool visit the [user manual](http://docs.wixstatic.com/ugd/52737a_2a65d0deb3bd4da2b5c0190c0de343ca.pdf) by Greg Ziegler.

Please visit the [Assmann lab](http://www.personal.psu.edu/sma3/) to learn about other interesting research topics.


## [Data availability](https://github.com/CLIMtools/AraCLIM/tree/master/data)
The data from the GxE associations provided by this tool as is available in [data/ folder](https://github.com/CLIMtools/CLIMGeno/tree/master/www/config/data). 

## [Citation](https://www.nature.com/articles/s41559-018-0754-5)
**Ferrero-Serrano, Á & Assmann SM.** Phenotypic and genome-wide association with the local environment of Arabidopsis. Nature Ecology & Evolution. doi: 10.1038/s41559-018-0754-5 (2019)


[**Read-only link to our paper**](https://www.nature.com/articles/s41559-018-0754-5.epdf?shared_access_token=7G2rqgz5YIsUFUQDzOOwwtRgN0jAjWel9jnR3ZoTv0MbnmJteG8gNCxdeNQO1H4wDh1E_905NLgAEUDsgSxMeCUvfrdwzMerY68l_0xqLtN4hZkD3eeuiMuub_3wA-0ai22Mkp6nj-1R1gfz5uNZNn7voROoJdRKIabCXqxz3ko%3D)


[<img align="left" src="https://github.com/CLIMtools/CLIMGeno/blob/master/Screen Shot.png">](https://rstudio.aws.science.psu.edu:3838/aaf11/CLIMGeno/ "CLIMGeno")
