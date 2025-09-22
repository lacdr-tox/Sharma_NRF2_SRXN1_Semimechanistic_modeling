#------------------------------------------------------------------------------
# NRF2-SRXN1 Model
#------------------------------------------------------------------------------

States  = {Srxn1};             # SRXN1 expression
                             


Inputs  = {NRF2_spline};            # Butadiene concentration inhaled (ppm)

Outputs  = {NRF2_spline1}; 


# parameters
# --------------------------------------------
buildSrxn1Base = 1; 
degradSrxn1 = 1;
hill_Srxn1 = 1;                   
Km_Srxn1 =    1;              
Vmax_buildSrxn1 = 1;
hill_acetylated_SRXN1 = 1;
Km_NRF2_ACETYLATED_SRXN1 = 1.48;
initial_fraction = 1E-3;
r = 1;
decay = 1;

# aceylated are exchanged in the name of parameters

Dynamics {
NRF2_spline1 = NRF2_spline;
Fraction_acetylation = 1/((1/initial_fraction - 1)*exp(-t*r) + exp(t*decay));
NRF2_spline2 = Fraction_acetylation*NRF2_spline1;    #acetylated_fraction
NRF2_spline3 = (1-Fraction_acetylation)*NRF2_spline1;   #non acetylated_fraction
dt(Srxn1) = buildSrxn1Base + Vmax_buildSrxn1 * ((pow(NRF2_spline3,hill_Srxn1)/(pow(Km_Srxn1,hill_Srxn1) + pow(NRF2_spline3,hill_Srxn1)))*
                             (pow(NRF2_spline2,hill_acetylated_SRXN1) / (pow(Km_NRF2_ACETYLATED_SRXN1,hill_acetylated_SRXN1) + pow(NRF2_spline2 ,hill_acetylated_SRXN1)))) - degradSrxn1 * Srxn1;


  
} # End of Dynamics


End.