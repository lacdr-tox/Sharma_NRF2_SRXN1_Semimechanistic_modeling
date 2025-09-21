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
hill = 1;                   
Km =    1;              
Vmax_buildSrxn1 = 1;
hill_act = 1;
Kact = 1.48;
fraction_x = 1;



# aceylated are exchanged in the name of parameters

Dynamics {
  NRF2_spline1 = NRF2_spline;
  NRF2_spline2 = fraction_x*NRF2_spline1;    #acetylated_fraction
  NRF2_spline3 = (1-fraction_x)*NRF2_spline1;   #non acetylated_fraction
  dt(Srxn1) = buildSrxn1Base + Vmax_buildSrxn1 * ((pow(NRF2_spline3,hill)/(pow(Km,hill) + pow(NRF2_spline3,hill)))*
                                                    (pow(NRF2_spline2,hill_act) / (pow(Kact,hill_act) + pow(NRF2_spline2 ,hill_act)))) - degradSrxn1 * Srxn1;
  
  
  
} # End of Dynamics


End.