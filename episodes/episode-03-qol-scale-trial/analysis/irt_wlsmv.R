
###############################################################
## ALFRED_STUNNER ##
###############################################################

###############################################################
## ORDINAL MODELS USING LIMITED INFO AND PROBIT LINK ##
###############################################################

###############################################################
## PACKAGES INSTALLATION BEGIN ## ###############################################################

#####  Check to see if packages are downloaded, install if not, then load  #####

if (!require("readxl")) install.packages("readxl")
library(readxl) # To import xls or xlsx data as table

if (!require("expss")) install.packages("expss")
library(expss) # To add variable and value labels, sort

if (!require("TeachingDemos")) install.packages("TeachingDemos")
library(TeachingDemos) # To create text output files

if (!require("psych")) install.packages("psych")
library(psych) # To add summary functions

if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2) # To make pictures

if (!require("lavaan")) install.packages("lavaan")
library(lavaan) # To fit SEMs

if (!require("semTools")) install.packages("semTools")
library(semTools) # lavaan add-ons (multiple imputation, moderation, omega)

if (!require("tidyr")) install.packages("tidyr")
library(tidyr) # For data frame management

if (!require("haven")) install.packages("haven")
library(haven) # For data import

if (!require("stargazer")) install.packages("stargazer")
library(stargazer) # For tables

if (!require("writexl")) install.packages("writexl")
library(writexl) # For writing Excel files

###############################################################
## PACKAGES INSTALLATION ENDS ## ###############################################################

###############################################################
## PREPROCESSING BEGINS ##
###############################################################

# Defining working directory
filesave = "C:/Users/eriak/OneDrive - University of Mississippi/Desktop/Alfred's thesis post proposal/Minichal"

# Setting the working directory to the folder specified above
setwd(dir=filesave)

# Save result to txt file within the folder above
 sink(file = paste0(filesave, "/R_Example4_IFA_Output.txt"))
 
# Set max.print to a high value
 options(max.print = 10000, width=1000, digits=8, scipen=9, show.signif.stars=FALSE)

# Importing and checking the PMN dataset. Imported using point and click
pmn_data_updated <- read_sas("pmn_data_updated.sas7bdat", 
                             NULL)

# Create a vector/columns for the items
Minichal <- c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64", "Q65", "Q66", "Q67", "Q68", "Q69")

# Create a data frame with only these 16 variables from the pmn_data_updated dataset
Minichaldata <- pmn_data_updated[Minichal]

###############################################################
## PREPROCESSING ENDS ##
###############################################################

###############################################################
## DESCRIPTIVES BEGIN ##
###############################################################

# Print the title for the descriptive analysis
print("Computing Descriptives")

# Insert a blank space in the output
cat("\n\n")

# Calculate and display descriptive statistics for the items
describe(x = Minichaldata)

# Insert a blank space in the output
cat("\n\n")

# Print the title for computing proportions
print("Computing proportions for each category of each item")

# Insert a blank space in the output
cat("\n\n")

# Function to calculate proportions for each item
calculate_proportions <- function(column) {
  prop.table(table(column))
}

# Apply the function to each column (item) in the dataset
proportions <- lapply(Minichaldata, calculate_proportions)

# Convert the list of proportions to a data frame for better readability
proportions_df <- do.call(cbind, proportions)

# Print the proportions
print(proportions_df)

# Insert a blank space in the output
cat("\n\n")


# Print the title for the correlation analysis
print("Computing Polychoric correlations")

# Insert a blank space in the output
cat("\n\n")

# Polychoric Correlations using lavCor from lavaan: Pairwise Correlation Matrix
lavCor(Minichaldata, ordered=Minichal, missing="pairwise", se="none", output="cor")

# Insert a blank space in the output
cat("\n\n")

# Polychoric Correlations using lavCor from lavaan: Saturated Model Results
lavCor(Minichaldata, ordered=Minichal, missing="pairwise", se="standard", output="est")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the correlation analysis
print("Computing Pearson correlations")

# Insert a blank space in the output
cat("\n\n")

# Regular Pearson Correlations using lavCor from lavaan: Pairwise Correlation Matrix
lavCor(Minichaldata, ordered=NULL, missing="pairwise", se="none", output="cor")

# Insert a blank space in the output
cat("\n\n")

# Regular Pearson Correlations using lavCor from lavaan: Saturated Model Results
lavCor(Minichaldata, ordered=NULL, missing="pairwise", se="standard", output="est")

# Insert a blank space in the output
cat("\n\n")

###############################################################
## DESCRIPTIVES ENDS ##
###############################################################

###############################################################
## SINGLE FACTOR MODELS BEGIN ##
###############################################################

# Print the title for the single factor model
print("SINGLE FACTOR MODEL")

# Insert a blank space in the output
cat("\n\n")

# Two-Parameter Model: Single Factor Using Fully Z-Scored Factor Scaling
# Factor Variance = 1, Factor Mean = 0, so all item loadings and thresholds estimated

# R Syntax for lavaan function: longer but more transparent version of model
Syntax2P = "

# ITEM LOADINGS

  # Define factor and request item factor loadings --> factor =~ item + item + item

    QOL =~ Q54  + Q55  + Q56 + Q57 + Q58 + Q59 + Q60 + Q61 + Q62 + Q63 + Q64 + Q65 + Q66 + Q67 + Q68 + Q69 


# ITEM THRESHOLDS/INTERCEPTS

  # Item thresholds/intercepts all estimated
  # Item thresholds (=intercept*-1) use the | operator and start at value 1 after t
  # There are 4 response categories and C-1 thresholds/intercepts

    Q54 | t1; Q54 | t2; Q54 | t3;

    Q55 | t1; Q55 | t2; Q55 | t3;

    Q56 | t1; Q56 | t2; Q56 | t3;

    Q57 | t1; Q57 | t2; Q57 | t3;

    Q58 | t1; Q58 | t2; Q58 | t3;

    Q59 | t1; Q59 | t2; Q59 | t3;

    Q60 | t1; Q60 | t2; Q60 | t3;

    Q61 | t1; Q61 | t2; Q61 | t3;

    Q62 | t1; Q62 | t2; Q62 | t3;

    Q63 | t1; Q63 | t2; Q63 | t3;

    Q64 | t1; Q64 | t2; Q64 | t3;

    Q65 | t1; Q65 | t2; Q65 | t3;

    Q66 | t1; Q66 | t2; Q66 | t3;

    Q67 | t1; Q67 | t2; Q67 | t3;

    Q68 | t1; Q68 | t2; Q68 | t3;

    Q69 | t1; Q69 | t2; Q69 | t3;


# ITEM ERROR/RESIDUAL VARIANCES

  # Item error variances fixed=1 for identification

    Q54  ~~ 1*Q54; 

    Q55  ~~ 1*Q55; 

    Q56  ~~ 1*Q56; 
    
    Q57  ~~ 1*Q57; 
    
    Q58  ~~ 1*Q58; 
    
    Q59  ~~ 1*Q59; 
    
    Q60  ~~ 1*Q60; 
    
    Q61  ~~ 1*Q61; 
    
    Q62  ~~ 1*Q62; 
    
    Q63  ~~ 1*Q63; 
    
    Q64  ~~ 1*Q64; 
    
    Q65  ~~ 1*Q65; 
    
    Q66  ~~ 1*Q66; 
    
    Q67  ~~ 1*Q67; 
    
    Q68  ~~ 1*Q68; 
    
    Q69  ~~ 1*Q69; 


# FACTOR MEAN

  # Factor mean fixed=0 (* means fixed in lavaan)

    QOL ~ 0;


# FACTOR VARIANCE

  # Factor variance fixed=1 (* means fixed in lavaan)

    QOL ~~ 1*QOL;

"

###############################################################
## GLOBAL FIT ESTIMATION ##
###############################################################

# Use WLSMV estimation like in Mplus, z-scored latent variables (mean=0, SD=1)
Model2P = lavaan(model=Syntax2P, data=Minichaldata, mimic="mplus", std.lv=TRUE,  
                 estimator="WLSMV", parameterization="theta", ordered=Minichal,
                 missing="pairwise")

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

# Print solution: get fit, get effect size, STDYX solution
summary(object=Model2P, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

###############################################################
## LOCAL FIT ESTIMATION ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for the test of local fit
print("TEST OF LOCAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for predicted means, variances and covariances
print("PREDICTED MEANS, VARIANCES, AND COVARIANCES")

# Insert a blank space in the output
cat("\n\n")

# Get model-implied means, variances, and covariances
fitted(object=Model2P)

###############################################################
## LOCAL FIT REIDUALS ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2P, type="raw");

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2P, type="cor");

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get normalized residuals (like z-scores for discrepancies)
resid(object=Model2P, type="normalized")

###############################################################
## LOCAL FIT MODIFICATION INDICES ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

# Get "cheat code" modification indices
modificationindices(object=Model2P, sort.=TRUE)

# Insert a blank space in the output
cat("\n\n")

###############################################################
## SINGLE FACTOR MODELS END ##
###############################################################

###############################################################
## TWO FACTOR MODELS BEGIN ##
###############################################################

# Print the title for the two factor model
print("TWO FACTOR MODEL")

# Insert a blank space in the output
cat("\n\n")

# Two-Parameter Model: Two Factor Using Fully Z-Scored Factor Scaling
# Factor Variance = 1, Factor Mean = 0, so all item loadings and thresholds estimated

# R Syntax for lavaan function: longer but more transparent version of model
Syntax2F_2P = "

# ITEM LOADINGS

  # Define factor and request item factor loadings --> factor =~ item + item + item

    # State of mind items
    StM =~ Q54  + Q55  + Q56 + Q57 + Q58 + Q59 + Q60 + Q61 + Q62 + Q63 
    
    # Somatic manifestation items
    SM =~ Q64 + Q65 + Q66 + Q67 + Q68 + Q69  

# ITEM THRESHOLDS/INTERCEPTS

  # Item thresholds/intercepts all estimated
  # Item thresholds (=intercept*-1) use the | operator and start at value 1 after t
  # There are 4 response categories and C-1 thresholds/intercepts

  # State of mind items

    Q54 | t1; Q54 | t2; Q54 | t3;

    Q55 | t1; Q55 | t2; Q55 | t3;

    Q56 | t1; Q56 | t2; Q56 | t3;

    Q57 | t1; Q57 | t2; Q57 | t3;

    Q58 | t1; Q58 | t2; Q58 | t3;

    Q59 | t1; Q59 | t2; Q59 | t3;

    Q60 | t1; Q60 | t2; Q60 | t3;

    Q61 | t1; Q61 | t2; Q61 | t3;

    Q62 | t1; Q62 | t2; Q62 | t3;

    Q63 | t1; Q63 | t2; Q63 | t3;

  # Somatic manifestation items

    Q64 | t1; Q64 | t2; Q64 | t3;

    Q65 | t1; Q65 | t2; Q65 | t3;

    Q66 | t1; Q66 | t2; Q66 | t3;

    Q67 | t1; Q67 | t2; Q67 | t3;

    Q68 | t1; Q68 | t2; Q68 | t3;

    Q69 | t1; Q69 | t2; Q69 | t3;


# ITEM ERROR/RESIDUAL VARIANCES

  # Item error variances fixed=1 for identification

  # State of mind items

    Q54  ~~ 1*Q54; 

    Q55  ~~ 1*Q55; 

    Q56  ~~ 1*Q56; 
    
    Q57  ~~ 1*Q57; 
    
    Q58  ~~ 1*Q58; 
    
    Q59  ~~ 1*Q59; 
    
    Q60  ~~ 1*Q60; 
    
    Q61  ~~ 1*Q61; 
    
    Q62  ~~ 1*Q62; 
    
    Q63  ~~ 1*Q63; 

  # Somatic manifestation items
    
    Q64  ~~ 1*Q64; 
    
    Q65  ~~ 1*Q65; 
    
    Q66  ~~ 1*Q66; 
    
    Q67  ~~ 1*Q67; 
    
    Q68  ~~ 1*Q68; 
    
    Q69  ~~ 1*Q69; 


# FACTOR MEAN

  # Factor mean fixed=0 (* means fixed in lavaan)

    # State of mind
      StM ~ 0;
      
    # Somatic manifestation
      SM ~ 0;


# FACTOR VARIANCE

  # Factor variance fixed=1 (* means fixed in lavaan)

    # State of mind
      StM ~~ 1*StM 
  
    # Somatic manifestation
      SM ~~ 1*SM

# FACTOR COVARIANCES

    # Covariance between the state of mind and somatic manifestation factors estimated

      StM ~~ SM

"

###############################################################
## GLOBAL FIT ESTIMATION ##
###############################################################

# Use WLSMV estimation like in Mplus, z-scored latent variables (mean=0, SD=1)
Model2F_2P = lavaan(model=Syntax2F_2P, data=Minichaldata, mimic="mplus", std.lv=TRUE,  
                    estimator="WLSMV", parameterization="theta", ordered=Minichal,
                    missing="pairwise")

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

# Print solution: get fit, get effect size, STDYX solution
summary(object=Model2F_2P, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

###############################################################
## LOCAL FIT ESTIMATION ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for the test of local fit
print("TEST OF LOCAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for predicted means, variances and covariances
print("PREDICTED MEANS, VARIANCES, AND COVARIANCES")

# Insert a blank space in the output
cat("\n\n")

# Get model-implied means, variances, and covariances
fitted(object=Model2F_2P)

###############################################################
## LOCAL FIT REIDUALS ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2F_2P, type="raw"); 

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2F_2P, type="cor");

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get normalized residuals (like z-scores for discrepancies)
resid(object=Model2F_2P, type="normalized")

###############################################################
## LOCAL FIT MODIFICATION INDICES ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

# Get "cheat code" modification indices
modificationindices(object=Model2F_2P, sort.=TRUE)

# Insert a blank space in the output
cat("\n\n")

###############################################################
## TWO FACTOR MODELS END ##
###############################################################



###############################################################
## TWO FACTOR MODELS WITH MODIFICATIONS BEGIN ##
###############################################################

# Print the title for the two factor model
print("TWO FACTOR MODEL WITH MODIFICATION")

# Insert a blank space in the output
cat("\n\n")

# Two-Parameter Model: Two Factor Using Fully Z-Scored Factor Scaling
# Factor Variance = 1, Factor Mean = 0, so all item loadings and thresholds estimated

# R Syntax for lavaan function: longer but more transparent version of model
Syntax2F_2P_M = "

# ITEM LOADINGS

  # Define factor and request item factor loadings. 
  # Add a cross-loading of Q62 on the somatic manifestation factor.
  # Add a cross-loading of Q63 on the somatic manifestation factor.
  
    # State of mind items
    StM =~ Q54  + Q55  + Q56 + Q57 + Q58 + Q59 + Q60 + Q61 + Q62 + Q63 
    
    # Somatic manifestation items
    SM =~ Q64 + Q65 + Q66 + Q67 + Q68 + Q69 + Q62 + Q63
    
# ITEM THRESHOLDS/INTERCEPTS

  # Item thresholds/intercepts all estimated
  # Item thresholds (=intercept*-1) use the | operator and start at value 1 after t
  # There are 4 response categories and C-1 thresholds/intercepts

  # State of mind items

    Q54 | t1; Q54 | t2; Q54 | t3;

    Q55 | t1; Q55 | t2; Q55 | t3;

    Q56 | t1; Q56 | t2; Q56 | t3;

    Q57 | t1; Q57 | t2; Q57 | t3;

    Q58 | t1; Q58 | t2; Q58 | t3;

    Q59 | t1; Q59 | t2; Q59 | t3;

    Q60 | t1; Q60 | t2; Q60 | t3;

    Q61 | t1; Q61 | t2; Q61 | t3;

    Q62 | t1; Q62 | t2; Q62 | t3;

    Q63 | t1; Q63 | t2; Q63 | t3;

  # Somatic manifestation items

    Q64 | t1; Q64 | t2; Q64 | t3;

    Q65 | t1; Q65 | t2; Q65 | t3;

    Q66 | t1; Q66 | t2; Q66 | t3;

    Q67 | t1; Q67 | t2; Q67 | t3;

    Q68 | t1; Q68 | t2; Q68 | t3;

    Q69 | t1; Q69 | t2; Q69 | t3;


# ITEM ERROR/RESIDUAL VARIANCES

  # Item error variances fixed=1 for identification

  # State of mind items

    Q54  ~~ 1*Q54; 

    Q55  ~~ 1*Q55; 

    Q56  ~~ 1*Q56; 
    
    Q57  ~~ 1*Q57; 
    
    Q58  ~~ 1*Q58; 
    
    Q59  ~~ 1*Q59; 
    
    Q60  ~~ 1*Q60; 
    
    Q61  ~~ 1*Q61; 
    
    Q62  ~~ 1*Q62; 
    
    Q63  ~~ 1*Q63; 

  # Somatic manifestation items
    
    Q64  ~~ 1*Q64; 
    
    Q65  ~~ 1*Q65; 
    
    Q66  ~~ 1*Q66; 
    
    Q67  ~~ 1*Q67; 
    
    Q68  ~~ 1*Q68; 
    
    Q69  ~~ 1*Q69; 

  # Allow error covariances based on modification indices from the 2-factor model

    Q55  ~~ Q56
    Q59  ~~ Q60


# FACTOR MEAN

  # Factor mean fixed=0 (* means fixed in lavaan)

    # State of mind
      StM ~ 0;
      
    # Somatic manifestation
      SM ~ 0;


# FACTOR VARIANCE

  # Factor variance fixed=1 (* means fixed in lavaan)

    # State of mind
      StM ~~ 1*StM 
  
    # Somatic manifestation
      SM ~~ 1*SM

# FACTOR COVARIANCES

    # Covariance between the state of mind and somatic manifestation factors estimated

      StM ~~ SM

"

###############################################################
## GLOBAL FIT ESTIMATION ##
###############################################################

# Use WLSMV estimation like in Mplus, z-scored latent variables (mean=0, SD=1)
Model2F_2P_M = lavaan(model=Syntax2F_2P_M, data=Minichaldata, mimic="mplus", std.lv=TRUE,  
                      estimator="WLSMV", parameterization="theta", ordered=Minichal,
                      missing="pairwise")

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

# Print solution: get fit, get effect size, STDYX solution
summary(object=Model2F_2P_M, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

###############################################################
## LOCAL FIT ESTIMATION ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for the test of local fit
print("TEST OF LOCAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for predicted means, variances and covariances
print("PREDICTED MEANS, VARIANCES, AND COVARIANCES")

# Insert a blank space in the output
cat("\n\n")

# Get model-implied means, variances, and covariances
fitted(object=Model2F_2P_M)

###############################################################
## LOCAL FIT REIDUALS ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2F_2P_M, type="raw"); 

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get residual (discrepancy) info for local misfit in covariance and correlation form
resid(object=Model2F_2P_M, type="cor");

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

# Get normalized residuals (like z-scores for discrepancies)
resid(object=Model2F_2P_M, type="normalized")

###############################################################
## LOCAL FIT MODIFICATION INDICES ##
###############################################################

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

# Get "cheat code" modification indices
modificationindices(object=Model2F_2P_M, sort.=TRUE)

# Insert a blank space in the output
cat("\n\n")

###############################################################
## TWO FACTOR MODEL WITH MODIFICATION END ##
###############################################################

###############################################################
## COMPUTING DISCRIMINATION AND DIFFICULTY PARAMETERS
###############################################################

# Extract all parameter estimates that is 
# Item means (Thresholds), Item variances (Loadings)
# error variances, error covariances, 
# factor mean, factor variance, factor covariances
# R-squared.

params <- parameterEstimates(Model2F_2P_M)

# Extract loadings parameters
loadings <- subset(params, op == "=~")

# Extract threshold parameters
thresholds <- subset(params, op == "|")

# Extract state of mind factor variance estimates
factor_var_StM <- subset(params, lhs == "StM" & op == "~~" & rhs == "StM")$est

# Extract somatic manifestation factor variance estimates
factor_var_SM <- subset(params, lhs == "SM" & op == "~~" & rhs == "SM")$est

# Extract state of mind factor mean estimates
factor_mean_StM <- subset(params, lhs == "StM" & op == "~1")$est

# Extract somatic manifestation factor mean estimates
factor_mean_SM <- subset(params, lhs == "SM" & op == "~1")$est


# Split loadings and thresholds by factor

# State of mind item-factor loadings
loadings_StM <- subset(loadings, lhs == "StM")

# State of mind threshold parameters
thresholds_StM <- subset(thresholds, lhs %in% c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63"))

# Somatic manifestation item-factor loadings
loadings_SM <- subset(loadings, lhs == "SM")

# Somatic manifestation threshold parameters
thresholds_SM <- subset(thresholds, lhs %in% c("Q62", "Q63", "Q64", "Q65", "Q66", "Q67", "Q68", "Q69"))

# Compute discrimination parameters
# Item discrimination = Item factor loading * Square root of (Factor variance)

# Computing discrimination parameters for the state of mind (StM) factor
discrimination_StM <- loadings_StM$est * sqrt(factor_var_StM)

# Computing discrimination parameters for the somatic manifestation (SM) factor
discrimination_SM <- loadings_SM$est * sqrt(factor_var_SM)

# Creating data-frame for State of mind discrimination parameters
discrimination_df_StM <- data.frame(Item = loadings_StM$rhs, Loading = loadings_StM$est, Discrimination = discrimination_StM)

# Creating data-frame for somatic manifestation discrimination parameters
discrimination_df_SM <- data.frame(Item = loadings_SM$rhs, Loading = loadings_SM$est, Discrimination = discrimination_SM)

# Computing difficulty parameters for the state of mind (StM) factor

# Item difficulty = (Item threshold-(Item factor loading * Factor mean)) / (Item factor loading * Square root of (Factor variance))

# Creating 3rows for each loading estimate that is each loading is repeated 3times
thresholds_StM$loading <- rep(loadings_StM$est, each = 3)

# Viewing the created rows using a dataframe
View(data.frame(thresholds_StM$loading))

# Computing the difficulty parameter for the state of mind factor
thresholds_StM$difficulty <- (thresholds_StM$est - (thresholds_StM$loading * factor_mean_StM)) / (thresholds_StM$loading * sqrt(factor_var_StM))

# Viewing the computation using a dataframe
View(data.frame(thresholds_StM$difficulty, thresholds_StM$est, thresholds_StM$loading))

# Computing difficulty parameters for the somatic manifestation (SM) factor

# Item difficulty = (Item threshold-(Item factor loading * Factor mean)) / (Item factor loading * Square root of (Factor variance))

# Creating 3rows for each loading estimate that is each loading is repeated 3times
thresholds_SM$loading <- rep(loadings_SM$est, each = 3)

# Viewing the created rows using a dataframe
View(data.frame(thresholds_SM$loading))

# Computing the difficulty parameter for the somatic manifestation factor
thresholds_SM$difficulty <- (thresholds_SM$est - (thresholds_SM$loading * factor_mean_SM)) / (thresholds_SM$loading * sqrt(factor_var_SM))

# Viewing the computation using a dataframe
View(data.frame(thresholds_SM$difficulty, thresholds_SM$est, thresholds_SM$loading))

# Create dataframes for both the state of mind and somatic manifestation difficulty parameters

# State of mind
difficulty_df_StM <- data.frame(Item = thresholds_StM$lhs, SubModel = thresholds_StM$rhs, Threshold = thresholds_StM$est, Difficulty = thresholds_StM$difficulty)

View(difficulty_df_StM)

# Somatic manifestation
difficulty_df_SM <- data.frame(Item = thresholds_SM$lhs, SubModel = thresholds_SM$rhs, Threshold = thresholds_SM$est, Difficulty = thresholds_SM$difficulty)

View(difficulty_df_SM)

# Print results
# Insert a blank space in the output
cat("\n\n")

print("Discrimination Parameters for StM")
print(discrimination_df_StM)

# Insert a blank space in the output
cat("\n\n")

print("Discrimination Parameters for SM")
print(discrimination_df_SM)

# Insert a blank space in the output
cat("\n\n")

print("Difficulty Parameters for StM")
print(difficulty_df_StM)

# Insert a blank space in the output
cat("\n\n")

print("Difficulty Parameters for SM")
print(difficulty_df_SM)

# Insert a blank space in the output
cat("\n\n")

# Close txt file
sink()

###############################################################
## USING THE MIRT PACKAGE
###############################################################

install.packages("mirt")
library(mirt)

# Define the model specification with cross-loadings and covariances

s <- '
  StM = Q54, Q55, Q56, Q57, Q58, Q59, Q60, Q61, Q62, Q63
  SM = Q64, Q65, Q66, Q67, Q68, Q69, Q62, Q63
  
  COV = StM*SM
'

# Fitting the model

fit <- mirt(data = Minichaldata, model = s, itemtype = 'graded', method = 'EM', SE = TRUE)

# Extract Parameters

params <- coef( fit, CI = 0.95, printSE = TRUE, rotate = "none", Target = NULL, IRTpars = FALSE, rawug = FALSE, as.data.frame = TRUE, simplify = FALSE, unique = FALSE, verbose = TRUE)


print(params)

# Close txt file
sink()


# Inspect the structure of the params object
str(params)

print(params)


# Convert params to data frame (if it's not already)
params_df <- as.data.frame(params)

# Print the data frame
print(params_df)

# Alternatively, use View for a spreadsheet-like display (if in RStudio)
View(params_df)














