# GeneticExplanatoryModeling

The purpose of this project is to create a model from synthetic data based off the psychological study of Caspi et al. (2003), where they measured the risk of depression from the short and long alleles of the 5HTT transporter gene and from major life events. 

Significant variables are determined by looking at the summaries for the first and second order linear models. Then,  a Box-Cox transformation is used to determine the transformation needed for the Y variable. With the transformation, forward and backward model selection methods are used to find the candidate variables for our true model. By looking at those variables’ significance in the main models, the best model for the data is determined.
