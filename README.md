

# Repository guide
Within this repository you will find the code materials for the commentary:

Nunez Y, Gibson AE, Gennings C, Tanner EM, Goldsmith JA, Coull AB, and Kioumourtzoglou M-A. Good Practices for Applied Statistical Learning in Epidemiology. Under review at the International Journal of Epidemiology

The commentary is currently under review at the International Journal of Epidemiology but available in https://arxiv.org/abs/2006.07305

The repository is organized into six folders. Four folders contained the coding materials for the four different methods discussed in the commentary: WQS, BKMR, lasso (lasso_glmnet), group lasso (grlasso_grpreg). All the materials for each respective method are within its folder. Within each method folder you will find an .Rmd file, or in the case of BKMR an r script, which contains the main code. For group lasso, you will find two .Rmd one contains the code for the cross validation over 100 different seeds and one the code for obtaining the variable coefficients over 100 different seeds. In the case of lasso, WQS, and BKMR all the code is in a single .Rmd. 

In addition to the methods folders you will find two other folders: 

1) Data: contains the data and the data dictionary.
2) Figures: contains the figures include in Nunez Y, et al paper.


**WARNING: This repo was created under R version 3.6.2.**