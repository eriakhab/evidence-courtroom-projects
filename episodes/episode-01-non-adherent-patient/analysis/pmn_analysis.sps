* Encoding: UTF-8.
*Programmer codename: Alfred_Stunner
Date: 9/17/2023.

****Selecting only observations we had on or after  23rd of august for analysis(that is observation 37 and after) 
  This is becuase 23rd of August is when we began main data collection.

* Compute a new variable called case_number that replicates $CASENUM.
COMPUTE Case_number=$CASENUM.
EXECUTE.

* select cases where case_number is greater than or equal to 37.
USE ALL.
COMPUTE filter_$=(Case_number   >=  37).
VARIABLE LABELS filter_$ 'Case_number   >=  37 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Deleting cases where case number is greater than or equal to 37 since they were pilot tests.
FILTER OFF.
USE ALL.
SELECT IF (Case_number >= 37).
EXECUTE.

*Creating a new variable called hypertension_diagnosis which will have a value of 1 for folks who chose a yes to at least one (or any) of the hypertension diagnosis questions
     and 0 for folks who chose a no to all hypertension diagnosis questions.
*Signifies OR.

COMPUTE hypertension_diagnosis=Q01 = 1 |  Q02 = 1 |  Q03 = 1 |  Q04 = 1 |  Q05 = 1 |  Q06 = 1  .
EXECUTE.

*Computing the average for perceived severity.

COMPUTE Perceived_severity=MEAN(Q17,Q18,Q19,Q20,Q21,Q22,Q23).
EXECUTE.

*Computing the average for perceived barrier.

COMPUTE Perceived_barrier=MEAN(Q24,Q25,Q26,Q27,Q28).
EXECUTE.

*Computing the average for perceived benefit.

COMPUTE Perceived_benefit=MEAN(Q29,Q30,Q31).
EXECUTE.

*Computing the average for perceived susceptibility.
RECODE Q32 (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q32R.
EXECUTE.
RECODE Q35 (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q35R.
EXECUTE.
RECODE Q36 (1=5) (2=4) (3=3) (4=2) (5=1) INTO Q36R.
EXECUTE.

COMPUTE Perceived_susceptibility=MEAN(Q32R,Q33,Q34,Q35R,Q36R,Q37).
EXECUTE.

*Computing the average for Attitude.

COMPUTE Attitude=MEAN(Q38,Q39,Q40,Q41,Q42).
EXECUTE.

*Computing the average for Subjective norm.

COMPUTE Subjective_norm=MEAN(Q43,Q44,Q45,Q46).
EXECUTE.

*Computing the average for Perceived behavioral control.

COMPUTE Perceived_behav_cont=MEAN(Q47,Q48).
EXECUTE.


*Computing the average for Intention.

COMPUTE Intention=MEAN(Q49,Q50,Q51,Q52).
EXECUTE.


*Flagging folks that chose purple on the attention check as 1 and 0 otherwise.

COMPUTE Att_check=Q53 = 4.
EXECUTE.

*Computing the average for general health.
COMPUTE Gen_health=MEAN(Q70,Q71,Q72,Q73).
EXECUTE.

*Running descriptives.
*Age.
DESCRIPTIVES VARIABLES=Q08
  /STATISTICS=MEAN STDDEV MIN MAX.

*Age categories.
RECODE Q08 (18 thru 34=1) (35 thru 49=2) (50 thru 64=3) (65 thru 98=4) INTO Age_Cat.
VARIABLE LABELS  Age_Cat '1 = 18 to 34; 2 = 35 to 49; 3 = 50 to 64; 4 = 65 and older'.
EXECUTE.

FREQUENCIES VARIABLES=Age_Cat
  /ORDER=ANALYSIS.

*gender, race, ethnicity, level of education, employment status, marital status, annual income, hypertension diagnosis, medications taking, and comorbidities.

FREQUENCIES VARIABLES=Q09 Q10 Q11_2 Q11_3 Q11_4 Q11_7 Q11_5 Q11_6 Q11_6_TEXT Q12 Q13 Q14 Q15 Q16 
    hypertension_diagnosis Q74_1 Q74_2 Q74_3 Q74_4 Q74_5 Q74_6 Q74_7 Q74_8 Q74_9 Q74_10 Q74_11 Q74_12 
    Q74_13 Q74_14 Q74_15 Q74_16 Q74_23 Q74_18 Q74_19 Q74_20 Q74_21 Q74_22 Q76
  /ORDER=ANALYSIS.

*Recategorizing Race variables.

*Creating a race count variable to identify folks that selected one race category and folks that selected more than one race category.
COMPUTE Race_count=SUM(Q11_2, Q11_3, Q11_4, Q11_5, Q11_6, Q11_7).
EXECUTE.

*First, I want to identify folks who identified as belonging to one race group.
*Now for folks who Identified as belonging to one race group I want to create categories for which race groups they belong to.
*For folks who identified as belonging to more than one race groups, I am lumping them together in the multiracial category.

IF (race_count = 1 & Q11_2 = 1) Race_Recategorized = 1.
IF (race_count = 1 & Q11_3 = 1) Race_Recategorized = 2.
IF (race_count = 1 & Q11_4 = 1) Race_Recategorized = 3.
IF (race_count = 1 & Q11_5 = 1) Race_Recategorized = 4.
IF (race_count = 1 & Q11_6 = 1) Race_Recategorized = 5.
IF (race_count = 1 & Q11_7 = 1) Race_Recategorized = 6.
IF (race_count >= 2 & race_count <= 6) Race_Recategorized = 7.

EXECUTE.

*Adding labels to the newly created variable.
VALUE LABELS Race_Recategorized
1 'American Indian or Alaska native'
2 'Asian'
3 'Black or African American'
4 'White'
5 'Other'
6 'Native Hawaiian or Pacific islander'
7 'Multiracial/Multiple selections'.

EXECUTE.

FREQUENCIES VARIABLES=Race_Recategorized
  /ORDER=ANALYSIS.

*Chronic conditions score or comorbidities score.

*Computing a sum score for chronic conditions variables to serve as a proxy for comorbidities.
*Using the sum function in spss ensures that missing values for a variable are treated as 0 while summing.
*If the + operator is used, if there is a missing value in any of the variables, the sum will be missing.
COMPUTE CCS_Score=SUM(Q74_1, Q74_2, Q74_3, Q74_4, Q74_5, Q74_6, Q74_7, Q74_8, Q74_9, Q74_10, Q74_11, Q74_12, Q74_13, Q74_14, Q74_15, Q74_16, 
    Q74_18, Q74_19, Q74_20, Q74_21, Q74_22, Q74_23).
EXECUTE.

FREQUENCIES VARIABLES=CCS_Score
  /ORDER=ANALYSIS.

*Assessing internal consistency of items in each scale.

*Perceived severity.

RELIABILITY
  /VARIABLES=Q17 Q18 Q19 Q20 Q21 Q22 Q23
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived barrier.
RELIABILITY
  /VARIABLES=Q24 Q25 Q26 Q27 Q28
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived  benefit.
RELIABILITY
  /VARIABLES=Q29 Q30 Q31
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived susceptibility.
RELIABILITY
  /VARIABLES=Q32R Q33 Q34 Q35R Q36R Q37
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Attitude.
RELIABILITY
  /VARIABLES=Q38 Q39 Q40 Q41 Q42
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Subjective norm.
RELIABILITY
  /VARIABLES=Q43 Q44 Q45 Q46
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived behavioral control.
RELIABILITY
  /VARIABLES=Q47 Q48
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Intention.
RELIABILITY
  /VARIABLES=Q49 Q50 Q51 Q52
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Bivariate correlation among each of the constructs as well as between each construct and intention.
STATS CORRELATIONS VARIABLES=Perceived_severity Perceived_barrier Perceived_benefit 
    Perceived_susceptibility Attitude Subjective_norm Perceived_behav_cont Intention
/OPTIONS CONFLEVEL=95 METHOD=FISHER
/MISSING EXCLUDE=YES PAIRWISE=YES.

*Data cleaning (Missing values).

*Checking to see if there are any variables with 5% or more missing values.
MVA VARIABLES=perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
            Attitude Subjective_norm Perceived_behav_cont Intention Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 
            hypertension_diagnosis CCS_Score Q76 Gen_health
/TTEST
/MPATTERN
/EM(TOLERANCE=0.001 CONVERGENCE=0.0001 ITERATIONS=25).


*Multiple Partial F test to see the effect of Attitudes, Subjective norms, Perceived behavioral control, Perceived susceptibility, Perceived barrier, Perceived benefit, Perceived severity on 
    intention after controlling for demographic variables (age, gender, race, ethnicity, level of education, employment status, marital status, annual income, 
    hypertension diagnosis, comorbidities, and medications taking).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
  /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont.


*Partial regression plots to check for the presence of outliers as well as violation of regression assumptions.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
 /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont
  /PARTIALPLOT ALL.

*Next, examined residuals to further check for outliers and violation of regression assumption.
*If centered leverage is greater than 2 times the number of predictors divided by the sample size that is (2 times 19 /  1046) = 0.03 in this case, then investigate for outlier.
*if cook's distance is greater than 1 then investigate for influential data point.
*An absolute value of greater than 2 for the minimum or maximum value on the studentized residual warrants investigation for potential outliers.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
 /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont
  /SAVE COOK LEVER ZRESID SRESID SDRESID.

*Flagging potential outliers as cases with a studentized residual above 2 and cases with a centered_leverage above 0.03.
COMPUTE Potential_outlier=(ABS(SDR_1) > 2) | (LEV_1 > 0.036).
EXECUTE.

*sorting cases by potential_outliers.
SORT CASES BY Potential_outlier(D).

**************************************************************************************************************************************************************************************************************************.

**************************************************************checking homoscedasticity, linearity and gaussian errors using residual plots *****************************************************************.


REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
 /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont
  /SAVE PRED SDRESID.


* Chart Builder.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=PRE_1 SDR_2 MISSING=LISTWISE REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE
  /FITLINE TOTAL=NO SUBGROUP=NO.
BEGIN GPL
  GUIDE: axis(dim(1), label("Unstandardized Predicted Value"))
  GUIDE: axis(dim(2), label("Studentized Deleted Residual"))
  GUIDE: text.title(label("Scatter Plot of Studentized Deleted Residual by Unstandardized ",
    "Predicted Value"))
  ELEMENT: point(position(PRE_1*SDR_2))
END GPL.

********************************************************** Checking for normality using normal probability plots ************************************************************************************************************.

EXAMINE VARIABLES=SDR_2
  /PLOT BOXPLOT STEMLEAF NPPLOT
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

********************************************************* Checking collinearity diagnostics using variance inflation factor, tolerance and condition indices *****************************.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA COLLIN TOL
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
 /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont.

********************************************************************************************************************************************************************************************************.

*Selecting only outlier cases to run descriptives.
USE ALL.
COMPUTE filter_$=(Potential_outlier = 1).
VARIABLE LABELS filter_$ 'Potential_outlier = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Rerun descriptives.

*Running descriptives.
*Age.
DESCRIPTIVES VARIABLES=Q08
  /STATISTICS=MEAN STDDEV MIN MAX.

*(18 thru 34=1) (35 thru 49=2) (50 thru 64=3) (65 thru 98=4) .
FREQUENCIES VARIABLES=Age_Cat
  /ORDER=ANALYSIS.

*gender, race, ethnicity, level of education, employment status, marital status, annual income, hypertension diagnosis, medications taking, and comorbidities.

FREQUENCIES VARIABLES=Q09 Q10 Q11_2 Q11_3 Q11_4 Q11_7 Q11_5 Q11_6 Q11_6_TEXT Q12 Q13 Q14 Q15 Q16 
    hypertension_diagnosis Q74_1 Q74_2 Q74_3 Q74_4 Q74_5 Q74_6 Q74_7 Q74_8 Q74_9 Q74_10 Q74_11 Q74_12 
    Q74_13 Q74_14 Q74_15 Q74_16 Q74_23 Q74_18 Q74_19 Q74_20 Q74_21 Q74_22 Q76
  /ORDER=ANALYSIS.

* Race categories
1 'American Indian or Alaska native'
2 'Asian'
3 'Black or African American'
4 'White'
5 'Other'
6 'Native Hawaiian or Pacific islander'
7 'Multiracial/Multiple selections'.

FREQUENCIES VARIABLES=Race_Recategorized
  /ORDER=ANALYSIS.

*********************************************************************************************************************************************************************************************************************************.

**************************************************************Rerunning decriptives  and the regression procedure without the outlier cases for sensitivity analysis*********************************************************************.

*Selecting only non-outlier cases to rerun  descriptives and regression.
USE ALL.
COMPUTE filter_$=(Potential_outlier = 0).
VARIABLE LABELS filter_$ 'Potential_outlier = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

*Rerun descriptives without the outlier cases.

*Running descriptives.
*Age.
DESCRIPTIVES VARIABLES=Q08
  /STATISTICS=MEAN STDDEV MIN MAX.

*(18 thru 34=1) (35 thru 49=2) (50 thru 64=3) (65 thru 98=4) .
FREQUENCIES VARIABLES=Age_Cat
  /ORDER=ANALYSIS.

*gender, race, ethnicity, level of education, employment status, marital status, annual income, hypertension diagnosis, medications taking, and comorbidities.

FREQUENCIES VARIABLES=Q09 Q10 Q11_2 Q11_3 Q11_4 Q11_7 Q11_5 Q11_6 Q11_6_TEXT Q12 Q13 Q14 Q15 Q16 
    hypertension_diagnosis Q74_1 Q74_2 Q74_3 Q74_4 Q74_5 Q74_6 Q74_7 Q74_8 Q74_9 Q74_10 Q74_11 Q74_12 
    Q74_13 Q74_14 Q74_15 Q74_16 Q74_23 Q74_18 Q74_19 Q74_20 Q74_21 Q74_22 Q76
  /ORDER=ANALYSIS.

* Race categories
1 'American Indian or Alaska native'
2 'Asian'
3 'Black or African American'
4 'White'
5 'Other'
6 'Native Hawaiian or Pacific islander'
7 'Multiracial/Multiple selections'.

FREQUENCIES VARIABLES=Race_Recategorized
  /ORDER=ANALYSIS.

FREQUENCIES VARIABLES=CCS_Score
  /ORDER=ANALYSIS.

************** Obtaining summaries statistics that is frequencies and percentages for each risk of hypertension diagnosis screener items as well as general health items.

FREQUENCIES VARIABLES=Q01 Q02 Q03 Q04 Q05 Q06 Q07 Q70 Q71 Q72 Q73
  /ORDER=ANALYSIS.

*Assessing internal consistency of items in each scale.

*Perceived severity.

RELIABILITY
  /VARIABLES=Q17 Q18 Q19 Q20 Q21 Q22 Q23
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived barrier.
RELIABILITY
  /VARIABLES=Q24 Q25 Q26 Q27 Q28
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived  benefit.
RELIABILITY
  /VARIABLES=Q29 Q30 Q31
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived susceptibility.
RELIABILITY
  /VARIABLES=Q32R Q33 Q34 Q35R Q36R Q37
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Attitude.
RELIABILITY
  /VARIABLES=Q38 Q39 Q40 Q41 Q42
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Subjective norm.
RELIABILITY
  /VARIABLES=Q43 Q44 Q45 Q46
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Perceived behavioral control.
RELIABILITY
  /VARIABLES=Q47 Q48
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Intention.
RELIABILITY
  /VARIABLES=Q49 Q50 Q51 Q52
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*Bivariate correlation among each of the constructs as well as between each construct and intention.
STATS CORRELATIONS VARIABLES=Perceived_severity Perceived_barrier Perceived_benefit 
    Perceived_susceptibility Attitude Subjective_norm Perceived_behav_cont Intention
/OPTIONS CONFLEVEL=95 METHOD=FISHER
/MISSING EXCLUDE=YES PAIRWISE=YES.

*Rerun Multiple Partial F test to see the effect of Attitudes, Subjective norms, Perceived behavioral control, Perceived susceptibility, Perceived barrier, Perceived benefit, Perceived severity on 
    intention after controlling for demographic variables (age, gender, race, ethnicity, level of education, employment status, marital status, annual income, 
    hypertension diagnosis, comorbidities, and medications taking).

*Running a sensitivity analysis by refitting the regression model with and without the outliers/influential data points. Upon comparing the results, the coefficients from the model 
with the influential cases removed were different from the coefficients obtained with all the cases. Hence, the results reported are without the outlier cases.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
  /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont.

*********************************************************************************************************************************************************************************************************************************************************.
*Deleting the potential_outlier cases and saving to excel to facilitate running a relative importance analysis on tonidandel's website with non outlier cases.

FILTER OFF.
USE ALL.
SELECT IF (Potential_outlier = 0).
EXECUTE.


**********************************************************************************************************************************************************************************************************.

*************************************************************Rerunning the regression procedure for folks that have a hypertension diagnosis only****************************************************.

*Selecting folks that have a hypertension diagnosis.
USE ALL.
COMPUTE filter_$=(hypertension_diagnosis = 1).
VARIABLE LABELS filter_$ 'hypertension_diagnosis = 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*Rerunning the regression procedure for these folks.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
  /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont.

*Selecting folks that do not have a hypertension diagnosis.
USE ALL.
COMPUTE filter_$=(hypertension_diagnosis = 0).
VARIABLE LABELS filter_$ 'hypertension_diagnosis = 0 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.


*Rerunning the regression procedure for these folks.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA CHANGE
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN 
  /DEPENDENT Intention
  /METHOD=ENTER  Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
  /METHOD=ENTER Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
    Attitude Subjective_norm Perceived_behav_cont.






