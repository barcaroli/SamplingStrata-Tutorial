# SamplingStrata-Tutorial

# Tutorial prepared for the uRos2018 conference

# Use of R package SamplingStrata for the Optimal Stratification of Sampling Frames for Multipurpose Sampling Surveys

The aim of this tutorial is to enable the participants to learn how to use the R package “SamplingStrata” in order to optimize 
the design of stratified samples. The package offers an approach for the determination of the best stratification of a sampling frame, 
the one that ensures the minimum sample cost under the condition to satisfy precision constraints in a multivariate and multi-domain case. 
This approach is based on the use of the genetic algorithm: each solution (i.e. a particular partition in strata of the sampling frame) 
is considered as an individual in a population; the fitness of all individuals is evaluated applying the Bethel algorithm to calculate 
the sampling size satisfying precision constraints on the target estimates. Functions in the package allows to: (a) prepare necessary 
inputs and check their validity; (b) perform the optimization step choosing the values of the most important parameters; (c) assign the 
optimized strata labels to the sampling frame; (d) select a sample from the new frame accordingly to the best allocation; (e) test the 
compliance of the design to precision constraints. The package also allows to consider the anticipate variance when the survey target 
variables are not available in the frame, but only proxy ones. A comparison to package “stratification” (valid for univariate designs) 
will be illustrated. 

Exercises will be proposed to participants, that are expected to be acquainted with basics of sampling theory. 
