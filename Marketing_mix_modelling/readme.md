R/Robyn implementation of a marketing mix model

Motivation for this project was to use Facebook's newly released open source software (Robyn) for marketing mix modelling with a standardised dataset from a consumer goods product.

The advantage of using Robyn as opposed to standard regression packages is that it provides options for flexible parameter optimisation for adstock with minimisation achieved through the use of integration into Nevergrad (gradient free optimization package).

A range of regression models based on the minimisation of rssd (decomposition root-sum-square-distance), nrmse (normalized root-mean-square error) and mape.lift (calibration mean average percentage error)

While marketing mix modelling will still require significant analyst input - packages like Robyn allow for a much more automated modelling process and have the potential to be designed in a low-code software package form.
