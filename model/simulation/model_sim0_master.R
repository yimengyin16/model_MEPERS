# Simulation 


# Valuation name
# rm(list = ls())

#source("libraries.R")


cat("Running simulation", sim_paramlist$sim_name, "\n")

#*******************************************************************************
#                         Load data                                         ####
#*******************************************************************************

# sim_paramlist$run_val <- FALSE
# if(sim_paramlist$run_val){
#   source(paste0("model/valuation/model_val_create_", sim_paramlist$val_name, ".R"))
# }

# Load tier data
dir_val <- "model/valuation/outputs_val/"

# tierData_miscAll

#*******************************************************************************
#              Actual investment return, for all tiers                      ####
#*******************************************************************************
source("model/simulation/model_sim_invReturns.R")
sim_paramlist$seed <- 123
i.r <- gen_returns()
#i.r
i.r[1:10, 1:5]


#*******************************************************************************
#                          Simulation ####
#*******************************************************************************

# if(sim_paramlist$useContingentCOLA){
#   source("model/simulation/model_sim_simulation_contingentCOLA.R")
# } else {
#   source("model/simulation/model_sim_simulation.R")
# }

# source("model/simulation/model_sim_simulation_MEPERS(1).R") # no caps, no risk sharing
# source("model/simulation/model_sim_simulation_MEPERS(1).R") # caps, no risk sharing

source("model/simulation/model_sim_simulation_MEPERS(4).R") # caps, simple contingent COLA

{
  start_time <- Sys.time()	
  penSim_results <- run_sim()
  end_time <- Sys.time()
  print(end_time  - start_time)
  suppressMessages(gc())
}






#*******************************************************************************
#                        Saving results ####
#*******************************************************************************

outputs_list <- list(sim_paramlist    = sim_paramlist,
                     Global_paramlist = Global_paramlist,
                     results          = penSim_results)


saveRDS(outputs_list, file = paste0(dir_outputs, "sim_", sim_name_run, ".rds"))





#*******************************************************************************
#                        TEMP: Examine results  ####
#*******************************************************************************


# Display1 Basic examination
var_display1 <- c("sim_name", "val_name", "sim", "year", 
                  "AL", "FR_MA",  "UAAL", "ERC", "ERC_PR","EEC_PR", "NC",
                  "MA",
                  "AL", 
                  "AL.active", "AL.nonactive",
                  "AL.defrRet",
                  "AL.servRet",
                  "PVFB",
                  "PVFB.active",
                  "cola_actual",
                  "B",
                  "NC_PR",
                  "ERC_PR",
                  "EEC_PR",
                  # "ADC", 
                  "NC", "ERC", "EEC", "SC", "LG", "i.r", "PR",
                  "nactives"
)


# Display: Decomposition of AL and PVFB
#var_display_decomp_Liab <- c("sim", "year", "FR_MA", "PVFB.act.laca", "PVFB.act.v", "PVFB.act.disbRet", "PVFB.act.death")

# Display: Decompsition of Benefit
#var_display_decomp_B <- c( "sim", "year", "FR_MA", "AL.act.death", "NC.death", "AL.death", "B.death")

# Display: demograhpics
#var_display_demo <- c("sim", "year", "FR_MA", "nactives", "nla", "nterms", "ndisbRet")
# "n.ca.R1", "n.ca.R0S1", "nterms",
# "ndisb.la", "ndisb.ca.R1", "ndisb.ca.R0S1" )

penSim_results %>% filter(sim == 5)  %>% select(one_of(var_display1))  %>% print
# penSim_results %>% filter(sim == 1)  %>% select(one_of(var_display1))  %>% print
# penSim_results %>% filter(sim == -2) %>% select(one_of(var_display1))  %>% print

print(end_time  - start_time)



# 
# penSim_results %>% filter(sim == 0, year == max(year)) %>% 
#   mutate(defr_pct.active = AL.defrRet/AL.active,
#          defr_pct_servRet = AL.defrRet / AL.servRet)


# sim_misc_bf100_cola2$results   %>% filter(sim == 0, year <= 2027)  %>% select(one_of(var_display1))  %>% print
# sim_misc_bf100_colaCut$results %>% filter(sim == 0, year <= 2027)  %>% select(one_of(var_display1))  %>% print


