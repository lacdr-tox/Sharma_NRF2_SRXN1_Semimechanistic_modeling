#------------------------------------------------------------------------------
# NRF2-SRXN1 Model
#------------------------------------------------------------------------------

#here it is modified the r and decay parameters were estimated from continous data set were used and then intial fraction value at the 8 hr value were used as a 
#intial fraction2 an additional paramters and then model has asked to fit only r2 and decay2 


States  = {Srxn1};             # SRXN1 expression



Inputs  = {NRF2_spline};            # Butadiene concentration inhaled (ppm)

Outputs  = {NRF2_spline1}; 


# parameters
# --------------------------------------------
buildSrxn1Base = 0.02262;
degradSrxn1 = 0.01577;
Km_Srxn1 = 107060;
hill_acetylated_SRXN1 = 9.82335;
Vmax_buildSrxn1 = 130492;
Km_NRF2_ACETYLATED_SRXN1 = 0.01361;
hill_Srxn1 = 1.00412;
initial_fraction = 1e-3;
#initial_fraction2 = 1E-3;   #fraction at the end of 24 hrs should be filled in the simulation file
r = 1;                      # this parmamter should be assigned the value in the simulation file estimated from continous exposure scenarios
decay =1;                   # this parmamter should be assigned the value in the simulation file estimated from continous exposure scenarios
r2 = 1;                     # this need to be esimated
decay2 = 1;                 # this need to be esimated
repeatedTime = 24.01;
initial_fraction2;
# aceylated are exchanged in the name of parameters

Initialize {

initial_fraction2 = 1/((1/initial_fraction - 1)*exp(-repeatedTime*r) + exp(repeatedTime*decay));
}

Dynamics {
  
  NRF2_spline1 = NRF2_spline;
  
  
  Fraction_acetylation1 = (t <= repeatedTime? 1/((1/initial_fraction - 1)*exp(-t*r) + exp(t*decay)):0);
  
  Fraction_acetylation2 = (t > repeatedTime? 1/((1/initial_fraction2 - 1)*exp(-t*r2) + exp(t*decay2)):0);
  
  Fraction_acetylation = (Fraction_acetylation1+Fraction_acetylation2);
  NRF2_spline2 = Fraction_acetylation*NRF2_spline1;    #acetylated_fraction
  NRF2_spline3 = (1-Fraction_acetylation)*NRF2_spline1;   #non acetylated_fraction
  dt(Srxn1) = buildSrxn1Base + Vmax_buildSrxn1 * ((pow(NRF2_spline3,hill_Srxn1)/(pow(Km_Srxn1,hill_Srxn1) + pow(NRF2_spline3,hill_Srxn1)))*
                                                    (pow(NRF2_spline2,hill_acetylated_SRXN1) / (pow(Km_NRF2_ACETYLATED_SRXN1,hill_acetylated_SRXN1) + pow(NRF2_spline2 ,hill_acetylated_SRXN1)))) - degradSrxn1 * Srxn1;
  
  
  
} # End of Dynamics


End.