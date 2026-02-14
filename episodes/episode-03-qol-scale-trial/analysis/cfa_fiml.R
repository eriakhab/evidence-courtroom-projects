############################################################### ALFRED_STUNNER ###############################################################

############################################################### PACKAGES INSTALLATION BEGIN ###############################################################

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

############################################################### PACKAGES INSTALLATION END ###############################################################

# Defining working directory
filesave = "C:/Users/eriak/OneDrive - University of Mississippi/Desktop/Alfred's thesis post proposal/Minichal"

# Setting the working directory to the folder specified above
setwd(dir=filesave)

# Syntax to open external file within the above folder to save results
sink(file = paste0(filesave, "/Alfred_Minichal_CFA_Output.txt"))

# Importing and checking the PMN dataset. Imported using point and click
pmn_data_updated <- read_sas("pmn_data_updated.sas7bdat", 
                             NULL)

# Create a vector/columns for the items
Minichal <- c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64", "Q65", "Q66", "Q67", "Q68", "Q69")

# Create a data frame with only these 16 variables from the pmn_data_updated dataset
Minichaldata <- pmn_data_updated[Minichal]

# Print the title for the descriptive analysis
print("Computing Descriptives")

# Insert a blank space in the output
cat("\n\n")

# Calculate and display descriptive statistics for the items
describe(x = Minichaldata)

# Insert a blank space in the output
cat("\n\n")

# Print the title for the correlation analysis
print("Computing Item correlations")

# Insert a blank space in the output
cat("\n\n")

# Calculate and display the Pearson correlation matrix for the items
# 'use = "pairwise.complete.obs"' means that pairwise deletion is used for handling missing data
cor(x = Minichaldata, use = "pairwise.complete.obs", method = "pearson")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the single factor model
print("SINGLE FACTOR MODEL")

# Insert a blank space in the output
cat("\n\n")

Syntax2 = "

# ITEM INTERCEPTS

    Q54  ~ 1; Q55  ~ 1; Q56  ~ 1; Q57  ~ 1; Q58  ~ 1; Q59  ~ 1; Q60  ~ 1; Q61  ~ 1; Q62  ~ 1; Q63  ~ 1; Q64  ~ 1; Q65  ~ 1; Q66  ~ 1; Q67  ~ 1; Q68  ~ 1; Q69  ~ 1 

# ITEM LOADINGS

    QOL =~ 1*Q54  + Q55  + Q56 + Q57 + Q58 + Q59 + Q60 + Q61 + Q62 + Q63 + Q64 + Q65 + Q66 + Q67 + Q68 + Q69 

# ITEM ERROR/RESIDUAL VARIANCES

    Q54  ~~ Q54; Q55  ~~ Q55; Q56  ~~ Q56; Q57  ~~ Q57; Q58  ~~ Q58; Q59  ~~ Q59; Q60  ~~ Q60; Q61  ~~ Q61; 
    Q62  ~~ Q62; Q63  ~~ Q63; Q64  ~~ Q64; Q65  ~~ Q65; Q66  ~~ Q66; Q67  ~~ Q67; Q68  ~~ Q68; Q69  ~~ Q69 

# FACTOR MEAN

    QOL ~ 0

# FACTOR VARIANCE

    QOL ~~ QOL

"

Model2 = lavaan(model=Syntax2, data=pmn_data_updated, estimator="MLR", mimic="mplus", std.lv=FALSE)

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

summary(object=Model2, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

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

fitted(object=Model2)

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model2, type="raw")

# Insert a blank space in the output
cat("\n\n")

# Print the title for correlation discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model2, type="cor")

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model2, type="normalized")

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

modificationindices(object=Model2, sort.=TRUE, information="observed")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the two factor model
print("TWO FACTOR MODEL")

# Insert a blank space in the output
cat("\n\n")

Syntax4 = "

# ITEM INTERCEPTS
  # Item intercepts all estimated

    # State of mind items
      Q54  ~ I1*1; Q55  ~ I2*1; Q56  ~ I3*1; Q57  ~ I4*1; Q58  ~ I5*1; 
      Q59  ~ I6*1; Q60  ~ I7*1; Q61  ~ I8*1; Q62  ~ I9*1; Q63  ~ I10*1

    # Somatic manifestation items
      Q64  ~ I11*1; Q65  ~ I12*1; Q66  ~ I13*1; Q67  ~ I14*1; 
      Q68  ~ I15*1; Q69  ~ I16*1  

# ITEM LOADINGS
  
    # State of mind items
      StM =~ 1*Q54  + L2*Q55  + L3*Q56 + L4*Q57 + L5*Q58 + L6*Q59 + L7*Q60 + L8*Q61 + L9*Q62 + LCr1*Q63  
    
    # Somatic manifestation items
      SM =~ 1*Q64 + L12*Q65 + L13*Q66 + L14*Q67 + L15*Q68 + L16*Q69 + LCr2*Q63 

# ITEM ERROR/RESIDUAL VARIANCES

    # State of mind items
      Q54  ~~ E1*Q54; Q55  ~~ Ecorr1*Q56; Q57  ~~ E4*Q57; Q58  ~~ E5*Q58; 
      Q59  ~~ Ecorr2*Q60; Q61  ~~ E8*Q61; Q62  ~~ E9*Q62; Q63  ~~ E10*Q63 
  
    # Somatic manifestation items
      Q64  ~~ E11*Q64; Q65  ~~ E12*Q65; Q66  ~~ E13*Q66; Q67  ~~ E14*Q67; 
      Q68  ~~ E15*Q68; Q69  ~~ E16*Q69  

# FACTOR MEAN

    # State of mind
      StM ~ 0
      
    # Somatic manifestation
      SM ~ 0

# FACTOR VARIANCE

    # State of mind
      StM ~~ StM 
  
    # Somatic manifestation
      SM ~~ SM

# FACTOR COVARIANCES

      StM ~~ FactCov*SM

"

Model4 = sem(model=Syntax4, data=pmn_data_updated, estimator="MLR", mimic="mplus", std.lv=FALSE)

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

summary(object=Model4, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

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

fitted(object=Model4)

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model4, type="raw")

# Insert a blank space in the output
cat("\n\n")

# Print the title for correlation discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model4, type="cor")

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model4, type="normalized")

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

modificationindices(object=Model4, sort.=TRUE, information="observed")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the likelihood ratio test
print("LRT for one factor vs two factors")

# Insert a blank space in the output
cat("\n\n")

anova(Model2, Model4)

# Insert a blank space in the output
cat("\n\n")


# Get factor scores
Model4Scores = lavPredict(object=Model4, newdata=NULL, type="lv", method="EBM",
                          se="standard", acov="standard", label=TRUE, fsm=FALSE, append.data=TRUE)

# Summarize each set of factor scores and print their common SE
#summary(Model4Scores); attr(Model4Scores,"se")[[1]][1,]


# Convert factor scores to a data frame
factor_scores_df <- as.data.frame(Model4Scores)

# Calculate and print the variance of factor scores 
StM_variance <- var(factor_scores_df$StM)
SM_variance <- var(factor_scores_df$SM)
#print(paste("Variance of StM: ", StM_variance))
#print(paste("Variance of SM: ", SM_variance))

# Function to calculate the reliability of factor scores
factorScoreReliability = function(lavObject){
  
  # Extract model parameter estimates from the lavaan object
  output = inspect(object = lavObject, what = "est")
  
  # Calculate the model-implied covariance matrix (sigma)
  sigma = output$lambda %*% output$psi %*% t(output$lambda) + output$theta
  
  # Calculate the variance of the factor scores
  varscores = output$psi - output$psi %*% t(output$lambda) %*% solve(sigma) %*% output$lambda %*% output$psi
  
  # Calculate and return the reliability of the factor scores
  return(diag(output$psi)/(diag(output$psi) + diag(varscores)))
}

# Print the title for the factor score reliability
print("FACTOR SCORE RELIABILITY")

# Insert a blank space in the output
cat("\n\n")

# Call function to get factor score reliability from Model 4
factorScoreReliability(lavObject=Model4)

# Insert a blank space in the output
cat("\n\n")

# Visualization using ggplot2
# Histogram overlaid with kernel density curve for State of Mind factor scores
ggplot(factor_scores_df, aes(x = StM)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = .5, colour = "black", fill = "white") + 
  xlim(c(-4, 4)) +
  labs(title = "State of Mind Factor Score") +
  geom_density(alpha = .2, fill = "#FF6666")


# Histogram overlaid with kernel density curve for Somatic Manifestation factor scores
ggplot(factor_scores_df, aes(x = SM)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = .5, colour = "black", fill = "white") + 
  xlim(c(-4, 4)) +
  labs(title = "Somatic Manifestation Factor Score") +
  geom_density(alpha = .2, fill = "#FF6666")


#PLOTTING MODEL-PREDICTED ITEM RESPONSES BY FACTOR SCORES


# Extract model parameter estimates
params <- parameterEstimates(Model4, standardized = FALSE)

# Extract intercepts and loadings for each factor
intercepts_StM <- params$est[params$op == "~1" & params$lhs %in% c("Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63")]
loadings_StM <- params$est[params$op == "=~" & params$lhs == "StM"]

intercepts_SM <- params$est[params$op == "~1" & params$lhs %in% c("Q64", "Q65", "Q66", "Q67", "Q68", "Q69", "Q63")]
loadings_SM <- params$est[params$op == "=~" & params$lhs == "SM"]

# Function to calculate predicted responses
predict_responses <- function(loadings, intercepts, factor_scores) {
  responses <- matrix(nrow = length(factor_scores), ncol = length(loadings))
  for (i in 1:length(loadings)) {
    responses[, i] <- intercepts[i] + loadings[i] * factor_scores  
  }
  return(responses)
}

# Calculate predicted responses for each factor
predicted_responses_StM <- predict_responses(loadings_StM, intercepts_StM, factor_scores_df$StM)

predicted_responses_SM <- predict_responses(loadings_SM, intercepts_SM, factor_scores_df$SM)

# Convert to data frames for plotting
plot_data_StM <- data.frame(FactorScore = factor_scores_df$StM, predicted_responses_StM)

plot_data_SM <- data.frame(FactorScore = factor_scores_df$SM, predicted_responses_SM)

# Rename columns to have item numbers
colnames(plot_data_StM) <- c("FactorScore", paste0("Item", 1:length(loadings_StM)))

colnames(plot_data_SM) <- c("FactorScore", paste0("Item", 1:length(loadings_SM)))

# Transposing the dataset from a wide to a long format. 
plot_data_StM_long <- pivot_longer(plot_data_StM, cols = starts_with("Item"), names_to = "Item", values_to = "Response")
plot_data_SM_long <- pivot_longer(plot_data_SM, cols = starts_with("Item"), names_to = "Item", values_to = "Response")

# Plot the state of mind factor scores for each individual (X axis) against their predicted item responses (Y axis)
ggplot(plot_data_StM_long, aes(x = FactorScore, y = Response, color = Item)) +
  geom_line() +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "State of Mind Factor Predicted Item Responses",
       x = "Factor Score (Mean = 0, Variance = estimated)",
       y = "Predicted Item Response") +
  theme_minimal() +
  xlim(-3, 3) + 
  ylim(0, 7)

# Plot the somatic manifestation factor scores for each individual (X axis) against their predicted item responses (Y axis)
ggplot(plot_data_SM_long, aes(x = FactorScore, y = Response, color = Item)) +
  geom_line() +
  geom_hline(yintercept = 4, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Somatic manifestation Predicted Item Responses",
       x = "Factor Score (Mean = 0, Variance = estimated)",
       y = "Predicted Item Response") +
  theme_minimal() +
  xlim(-3, 3) + 
  ylim(0, 7)







# Insert a blank space in the output
cat("\n\n")

# Print the title for the two factor model
print("TWO FACTOR MODEL WITH NO MODIFICATION")

# Insert a blank space in the output
cat("\n\n")

Syntax5 = "

# ITEM INTERCEPTS
  # Item intercepts all estimated

    # State of mind items
      Q54  ~ I1*1; Q55  ~ I2*1; Q56  ~ I3*1; Q57  ~ I4*1; Q58  ~ I5*1; 
      Q59  ~ I6*1; Q60  ~ I7*1; Q61  ~ I8*1; Q62  ~ I9*1; Q63  ~ I10*1

    # Somatic manifestation items
      Q64  ~ I11*1; Q65  ~ I12*1; Q66  ~ I13*1; Q67  ~ I14*1; 
      Q68  ~ I15*1; Q69  ~ I16*1  

# ITEM LOADINGS
  
    # State of mind items
      StM =~ 1*Q54  + L2*Q55  + L3*Q56 + L4*Q57 + L5*Q58 + L6*Q59 + L7*Q60 + L8*Q61 + L9*Q62 + L10*Q63  
    
    # Somatic manifestation items
      SM =~ 1*Q64 + L12*Q65 + L13*Q66 + L14*Q67 + L15*Q68 + L16*Q69 

# ITEM ERROR/RESIDUAL VARIANCES

    # State of mind items
      Q54  ~~ E1*Q54; Q55  ~~ E2*Q55; Q56  ~~ E3*Q56;
      Q57  ~~ E4*Q57; Q58  ~~ E5*Q58; Q59  ~~ E6*Q59;
      Q60  ~~ E7*Q60; Q61  ~~ E8*Q61; Q62  ~~ E9*Q62; 
      Q63  ~~ E10*Q63 
  
    # Somatic manifestation items
      Q64  ~~ E11*Q64; Q65  ~~ E12*Q65; Q66  ~~ E13*Q66; 
      Q67  ~~ E14*Q67; Q68  ~~ E15*Q68; Q69  ~~ E16*Q69  

# FACTOR MEAN

    # State of mind
      StM ~ 0
      
    # Somatic manifestation
      SM ~ 0

# FACTOR VARIANCE

    # State of mind
      StM ~~ StM 
  
    # Somatic manifestation
      SM ~~ SM

# FACTOR COVARIANCES

      StM ~~ FactCov*SM

"

Model5 = sem(model=Syntax5, data=pmn_data_updated, estimator="MLR", mimic="mplus", std.lv=FALSE)

# Print the title for the test of global fit
print("TEST OF GLOBAL FIT")

# Insert a blank space in the output
cat("\n\n")

# Print the title for the model fit indices and effect size
print("MODEL FIT INDICES AND EFFECT SIZE")

# Insert a blank space in the output
cat("\n\n")

summary(object=Model5, fit.measures=TRUE, rsquare=TRUE, standardized=TRUE)

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

fitted(object=Model5)

# Insert a blank space in the output
cat("\n\n")

# Print the title for covariance discrepancy
print("COVARIANCE DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model5, type="raw")

# Insert a blank space in the output
cat("\n\n")

# Print the title for correlation discrepancy
print("CORRELATION DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model5, type="cor")

# Insert a blank space in the output
cat("\n\n")

# Print the title for normalized discrepancy
print("NORMALIZED DISCREPANCY")

# Insert a blank space in the output
cat("\n\n")

resid(object=Model5, type="normalized")

# Insert a blank space in the output
cat("\n\n")

# Print the title for Modification indices
print("MODIFICATION INDICES")

# Insert a blank space in the output
cat("\n\n")

modificationindices(object=Model5, sort.=TRUE, information="observed")

# Insert a blank space in the output
cat("\n\n")


# Syntax to stop redirecting output
sink()
