There seems to be a resurgence of interests for the nonparametric Bayes estimator of survival curve with censored data.
I got several emails asking about the R package NPBayes.
Therefore I updated the package I wrote 10 years ago, it is now working with more recent version of R (2.15.0, and 3.0.2).

References:

Susarla, V and Van Ryzin, J.  (1976).  Nonparametric Bayesian estimation of survival curves from incomplete observations. JASA 71 897-902.

Zhou, M. (2004). Nonparametric Bayes estimator of survival functions from doubly/interval censored data. Statistica Sinica 14, 533-546.

Zhou, M. and Luan, J. (2005) Nonparametric Bayes Estimator of Survival Function for Right-Censoring and Left-Truncation Data

http://www.ms.uky.edu/%7Emai/research/BayesEstimator.pdf

Note: The parameter for the Dirichlet process prior is a measure on R+, in the package NPBayes we took it to be  a measure generated
by the function  B exp( - theta t), i.e. for any interval [a, b) the measure is  B[ exp(-theta a) - exp(-theta b) ] ,
where B and theta are two scalar parameters.  This could be changed to any other positive measure on the R+, with some code modifications.

The prior is a Dirichlet process prior. Not mix of Dirichlet process priors.  
The package only computes the mean of the posterior, no credible regions..

