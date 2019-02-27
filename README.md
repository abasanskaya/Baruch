# Anna Basanskaya's Repo

This is a sample of some of my school projects/contributions to school projects, including: Random Matrix Theory (RMT), PySpark, SVM, training vs. test, lasso, ridge


## Table of Contents 

> This repo contains the following:

- [Yelp Project Using PySpark](#yelp_project)
- [Random Matrix Theory Applied to the S&P 500 Correlation Matrix](#rmt)
- [Train vs. Test](#train_test)
- [Max Animal Longevity](#animal_longevity)



## yelp_project

- Used PySpark dataframes/ML to perform clustering on yelp businesses. 
- Businesses were filtered down to U.S. dining businesses, and the geographic cluster
with the highest open rate was selected.
- The food subcategory with the highest stars was then selected.
- Next, the reviews were aggregated with the businesses and featurized. A logistic regression 
  model is used to classify businesses, and the features with the highest weights for the 5 star category were selected.
- A word cloud was created with these features based on the weights.

## rmt
- Applied Random Matrix Theory (RMT) to de-noising the correlation matrix of stock returns. 

  -	Fit the Marchenko-Pastur distribution to the histogram of eigenvalues of a correlation matrix of S&P 500 stock returns.
  - Used the Tracy-Widom distribution to determine an upper limit below which “random” eigenvalues were discarded.
  - De-diagonalized the filtered matrix to get non-random correlation of stock returns.
  -	Compared the volatilities from the original and filtered matrices as well as the effect on the efficient portfolio.
 - Coding in R (Libaries: quantmod, zoo, tseries) 

## train_test
- Evaluated different train/test splits in the classification of the first-order theorem proving dataset with lasso and ridge regression, support vector machine (SVM), and random forest models.

  -	Examined distribution of the misclassification rate in lasso and ridge regression using loocv and 10-fold cross-validation methods as well as AIC and found optimal lasso and ridge penalty parameter
  -	Tuned SVM and Random Forest models 
  -	Determined variable importance from lasso, ridge, and random forest models
-	Coding in R


## animal_longevity
- Utilized principal component and principal factor analysis to gain insight into factors believed to be associated with maximal longevity in animals.

  -	Performed principal component and principal factors analysis to identify the most important factors related to maximal animal longevity
  - Found that the first factor represented log weight and log metabolic rate, with the second factor reflecting a difference between mitochondrial DNA A and G content, the third a difference between mitochondrial DNA T and C content, and the fourth a difference in the telomere length and temperature.
  
- Coding in SAS

