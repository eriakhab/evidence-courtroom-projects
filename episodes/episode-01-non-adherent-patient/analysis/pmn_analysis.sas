/***********************************************************************************************************/
/***********************************************************************************************************/
 *PROGRAMMER CODENAME: ALFRED_STUNNER
  DATE: 17th October 2023
  PROJECT: PRIMARY NONADHERENCE;
/***********************************************************************************************************/
/***********************************************************************************************************/

options mprint mlogic symbolgen;

LIBNAME project "C:\Users\eriak\OneDrive - University of Mississippi\Desktop\Alfred's thesis post proposal\Edited data";

*Deleting pilot tests;
Data Pmn_data_updated;
set project.Pmn_data_updated;

If case_number LE 36 then delete;

run;

*Runnning proc robustreg;
PROC ROBUSTREG DATA=Pmn_data_updated METHOD=MM;
    MODEL Intention = Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
                      Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
                      Attitude Subjective_norm Perceived_behav_cont;
    OUTPUT OUT=work.Pmn_data_updated_2 r=residual;
RUN;

proc freq data = project.Pmn_data_updated;
tables case_number;
run;

/*************** Testing for heteroscedasticity using only variables with a potential_outlier value of 0 *********/
/***Breusch pagan test***/

proc reg data=Pmn_data_updated outest=outdata;
    where Potential_outlier = 0;
    model Intention = Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
                      Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
                      Attitude Subjective_norm Perceived_behav_cont;
    output out=resid_data r=residual;
run;
quit;

data squared_resid;
    set resid_data;
    squared_residual = residual**2;
run;


proc reg data=squared_resid;
    where Potential_outlier = 0;
    model squared_residual = Q08 Q10 Race_Recategorized Q12 Q13 Q14 Q15 Q16 hypertension_diagnosis CCS_Score Q76 Gen_health
                             Perceived_severity Perceived_barrier Perceived_benefit Perceived_susceptibility 
                             Attitude Subjective_norm Perceived_behav_cont;
run;
quit;










