




## Tools #####


# df_results <- 
# 	map(filter(df_simNames, !str_detect(simName, "all2t"))$simName, ~ readRDS(paste0(here::here(), "/", dir_sims, "sim_", .x, ".rds"))$results) %>% 
# 	bind_rows() %>% 
# 	mutate(NC.ER_PR = 100*(NC - EEC)/PR ) 
# # mutate(
# #        #sim_name = str_replace(sim_name, "&", "_"),
# #        sim_name = factor(sim_name, levels = df_simNames$simName )
# #        )




#*******************************************************************************
##                            Loading data                                  ####
#*******************************************************************************

df_simNames <- 
	tribble(
		~simName, ~label_benPolicy,
		"regularAll_sharedADC_vCola",      "Shared ADC and Variable COLA",
		"regularAll_sharedADC_fixedCola",  "Shared ADC and fixed COLA",
		"regularAll_sharedNC_fixedCola",   "shared NC and Variable COLA; EEC floor 0"
		)


results_all <- 
	map(df_simNames$simName, ~readRDS(paste0(dir_modelResults, "sim_", .x, ".rds"))$results) %>% 
	bind_rows()
	#mutate(NC.ER_PR = 100*(NC - EEC)/PR ) 


# results_all %>% nrow()



# 
# # Loading results
# results_all <- get_results(dir_modelResults) %>% 
# 	filter(runname %in% names(run_labels_all)) %>% 
# 	select(runname, sim, year, everything()) %>% 
# 	mutate(runname_wlabel =  factor(runname, levels = run_levels_all, labels = run_labels_all),
# 				 runname = factor(runname, levels = run_levels_all),
# 				 #ERC_PR = ERC / salary,
# 				 #ERC2   = NC.ER + SC, # For SDRS policy analysis only
# 				 #ERC2_PR = ERC2 / salary
# 				 PR = ifelse(str_detect(runname, "DA"), PR, EEC/EEC_PR),
# 				 SC_legacy = na2zero(SC_legacy)
# 	) 
# 
# #results_all %>% head
# # results_all$runname %>% unique
# #results_all %>% select(runname, sim, year, ERC, SC_legacy)
# 
# 
# 
# 
# #*******************************************************************************
# ##                      Create additional variables                         ####
# #*******************************************************************************
# 
# # Add DC contributions for hybrid plans
# 
# results_all %<>% 
# 	mutate(EEC_DB = EEC,
# 				 ERC_DB = ERC,
# 				 C_DB   = EEC_DB + ERC_DB, 
# 				 
# 				 EEC_DC = ifelse(str_detect(runname, "hybrid_DB"), salary * DC_EECrate, 0),
# 				 ERC_DC = ifelse(str_detect(runname, "hybrid_DB"), salary * DC_ERCrate, 0),
# 				 C_DC   = EEC_DC + ERC_DC, 
# 				 
# 				 EEC = EEC_DB + EEC_DC,
# 				 ERC = ERC_DB + ERC_DC,
# 				 
# 				 # adding legacy SC, currently only applied to hybrid plans
# 				 ERC = ERC + SC_legacy,
# 				 
# 				 C = EEC + ERC,
# 				 
# 				 ERC_PR =  ifelse(str_detect(runname, "DA"), ERC/PR, ERC / salary),
# 				 EEC_PR =  ifelse(str_detect(runname, "DA"), EEC/PR, EEC / salary)
# 	)
# 
# # results_all %>% filter(runname %in% c("baseline", "hybrid_DB"), sim == 0, year <=20) %>% 
# # select(runname, year, C_DB, C_DC)

