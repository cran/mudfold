mudfold v.1.1.1 (changes from version 1.1.0)

-   In this version of the package nonparametric ordinary bootstrap uncertairty estimates for the model fit and the diagnostic statistics have been added.

-   The R package "yesno" is not used anymore at the data checking step of the main mudfold() function.

-   Changes have been made at the generic functions prin.mdf() and summary.mdf() in order to incorporate bootstrap results.

-   an additional functionality has been added to the plot.mdf() function. Now by setting the argument plot.type= "persons" the distribution of the person parameters will be returned.

mudfold v.1.1.2 (changes from version 1.1.1)

-   New generic 'coef' function for 'mdf' class objects. 

-   The argument 'start' in mudfold() function has been renamed into 'start.scale'.

-   The main mudfold() function has been extended to handle datasets with missing values by using multiple imputation with chained equations implemented from the R package mice.

-   Bug in the data checking step of the mudfold() function that could potentially cause issues with simulated data from the mudfoldsim() function has been fixed.

-   In this version of the package an additional argument called "seed" has been added in the main mudfold() function in order to control reproducibility of bootstrap results.

-   The internal function CAM() that calculates the conditional adjacency matrix has been exported.

-   The internal function ISO() that calculates the ISO statistic has been changed and exported.

-   A new function MAX() that calculates the MAX statistic as described by Post(1992) is now available in the new version of the package. 

-   A new function diagnostics() has been added that calculates diagnostics for the MUDFOLD scale assumptions.

-   A warning has been added into the documentation of the function pick() (see ?pick) to alert the user about the practice of dichotomization. 






