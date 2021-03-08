# Constructing tier "regular.t1"



#*******************************************************************************
#                                Notes ####
#*******************************************************************************

#' Inputs:
#'   - inputs/data_proc/Data_MEPERS_decrements_AV2020_imputed.RData
#'   - inputs/data_proc/Data_MEPERS_demographics_20200630_fillin.RData


#' What this file does
#'   - produce the following model inputs for tier "regular.t1"
#'     - decrement table
#'     - salary scale
#'     - initial demographics   
#'     - tier specific parameters


#*******************************************************************************
#                               Tier specification  ####
#*******************************************************************************


##' Members included 
#'  - Regular plan members, tier 1 and 2



###' Service retirement 
#'  
#'  - Benefit rules
#'      - Use benefit rules of Regular plan AC tier 1 (members joining before July 1, 2014)
#'      
#'       
#'      
#'  - Final compensation (FAS) 
#'      - The plan policy
#'        - 3 years of highest compensation
#'      - Model:
#'        - same
#'  
#'  - Benefit formula
#'      - benfactor: 2%
#'      - min benefit: $1200/year if YOS >= 10 (AV2020 value)  
#'  
#'  
#   - Normal retirement age
#       - 60 for regular.t1
#       - 65 for regular.t2
#'  
#'  - Eligibility
#'      - YOS >= 25 or
#'      - age >= normal ret age and YOS >= 5 
#'      
#'  - Vesting: 
#'    - Need to further check, 
#'    - use yos >=5 for March 15 
#'  
#' 
#' 
###' Deferred retirement  
#' - Plan policy: retirement benefit deterred to normal retirement age if vested.
#  - Model: same
#' - Simplification: do not model refund upon separation if not vested



###' Disability retirement
#  
#  - Based on no-age benefit(disability after Oct 16, 1992), but greatly simplified
#    
#  - Eligibility: 
#      - plan policy: yos >5 for disabled in line of duty
#      - model: yos >= 5 
#
#  - Benefit: 
#'      - plan poicy: see notes
#'       - model: 59% of FAS upon disability, last until death
#'                Calibratoin needed to match AV


# Death benefit: pre-retirement
#  - based on accidental death benefit
#  - model: 100% FAS upon death, no YOS requirement. 




## Assumptions, needs to be revisited 

# see "other" tab in the raw demographic file

#' Active members (AV2020 np71)
#'  - regular: 0.75
#'  - special: 0.25
#'  based on (approx.) average weights for actives and retirees

#' For now, the weights of active members above are applied all types of decrements, 
#' including rates for retirees. 


# gender ratio:
# - No gender ratio provided in AV and CAFR, 
#   = Assumption for regular: 60% female and 40% male
#   - Assumption for special: 10% female and 90% male




## Assumptions on demographics
# 
# Regular t1 and regular t2 mebmers: to be modified
#  Active members:
#   - Members who joined the plan on or after Jan 1, 2013 are PEPRA members
#   - In the model, we assume that a member attains 1 yos after a full year of employment. 
#   - The dempgrahpic data from AV2018 are up to 6/30/2018
#   - Therefore, members who joined on 6/30/2013 had just attained 5 YOS and 
#     members who joined between 7/1/2012 and 6/30/2013 have 5 yos in the membership data. 
#   - Assuming the entry of new members uniformly distributed during year, we can assume that 
#     all members with YOS <= 4 and half of new members with YOS = 5 are PEPRA members. 
#  
#
#  Serivice retirees for regular members
#     In the model we assume all service retirees and beneficiaires in the AV data are tier 1 regular members
#
#
#  Initial terminated members
#   - For now, we assume that for each tier the liability of initial terminated members(in or not in pay status) 
#     is a fixed percentage of the AL of retirees. 
#   - As we assume the regular tier 2 has no retirees in the model, there are no AL for initial terminated members 
#     under the current simplification method. The should not be an issue because the actual AL for termianted should be 
#     very small as the PEPRA tier is still new. 





#*******************************************************************************
#                      ## Global settings  ####
#*******************************************************************************

dir_data    <- "inputs/data_proc/"
dir_outputs <- "model/tiers/tierData/"



# Model settings
range_age <- 20:100
range_ea  <- 20:69  # max retirement age is assumed to be 70 (qxr = 1 at age 70 in AV tables) 



# Tier specific parameters
tier_name <- "regular.t1"
age_vben  <- 60 # assumed age of starting receiving deferred retirement benefits (normal retirement age)
v.year    <- 5
fasyears  <- 3  
bfactor   <- 0.02
cola_assumed <- 0.0191 # assumed cola rates for valuation  
# EEC_rate <- 0.0735 # use EEC and ERC caps 


#*******************************************************************************
#                      ## Loading data  ####
#*******************************************************************************

load(paste0(dir_data, "Data_MEPERS_decrements_AV2020_imputed.RData"))
load(paste0(dir_data, "Data_MEPERS_demographics_20200630_fillin.RData"))

# Data loaded:

## Decrements:
# df_qxr_regular_imputed
# df_qxr_special_imputed
# df_qxd_imputed
# df_qxt_imputed
# df_qxm.pre_imputed
# df_qxm.post_imputed
# df_qxmd_imputed
# df_salScale_imputed

## Member data
# df_nactives_fillin
# df_n_servRet_fillin
# df_n_disbRet_occ_fillin
# df_n_disbRet_nonocc_fillin
# df_n_beneficiaries_fillin


#*******************************************************************************
#                      ## Decrements 1: combining groups ####
#*******************************************************************************

## Service retirement rates

# groups included
grp_include <- df_qxr_regular_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include , "regular.t1|regular.t2")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "regular.t1","wgt"] <-  1
wgts[wgts$grp == "regular.t2","wgt"] <-  0


## calculate weighted average
df_qxr_tier <- 
  df_qxr_regular_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(age) %>% 
  summarise(qxr = weighted.mean(qxr, wgt), .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Disability retirement rates

# groups included
# grp_include <- df_qxd_imputed$grp %>% unique
# grp_include <- grp_include[str_detect(grp_include, "misc|inds")]

# weight for each group
# wgts <- tibble(grp = grp_include, wgt = 0)
# 
# wgts[wgts$grp == "misc_t1_female","wgt"] <-  0.679 * 0.918/(0.918+0.018) * 0.6
# wgts[wgts$grp == "misc_t1_male",  "wgt"] <-  0.679 * 0.918/(0.918+0.018) * 0.4
# wgts[wgts$grp == "misc_t2_female","wgt"] <-  0.679 * 0.018/(0.918+0.018) * 0.6
# wgts[wgts$grp == "misc_t2_male",  "wgt"] <-  0.679 * 0.018/(0.918+0.018) * 0.4
# wgts[wgts$grp == "inds",  "wgt"]         <-  0.046


## calculate weighted average
# Need to combine two types of disability rates: adding the two rates

df_qxd_tier <- 
  df_qxd_imputed %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  ungroup()



## Termination with refund

# groups included
grp_include <- df_qxt_imputed$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "regular|special")]

# weight for each group
wgts <- tibble(grp = grp_include, wgt = 0)

wgts[wgts$grp == "regular",  "wgt"] <- 1 
wgts[wgts$grp == "special",  "wgt"] <- 0



## calculate weighted average
df_qxt_tier <- 
  df_qxt_imputed %>% 
  filter(grp %in% grp_include) %>% 
  left_join(wgts, by = "grp") %>% 
  group_by(yos) %>% 
  summarise(qxt = weighted.mean(qxt, wgt),
            .groups = "drop") %>% 
  mutate(grp = tier_name) %>% 
  relocate(grp) %>% 
  arrange(yos) %>% 
  ungroup()



## Pre-retirement mortality
df_qxm.pre_tier <-  
  df_qxm.pre_imputed %>% 
  mutate(qxm.pre = 0.6 * qxm.pre.female + 0.4 * qxm.pre.male,
         grp = tier_name
         ) %>% 
  select(grp, age, qxm.pre)




## Post-retirement mortality for healthy retirees, without projection

df_qxm.post_tier <-  
  df_qxm.post_imputed %>% 
  mutate(qxm.post         = 0.6 * qxm.post.female  + 0.4 * qxm.post.male,
         grp = tier_name
  ) %>% 
  select(grp, age, qxm.post)


## Post-retirement mortality for disability retirees, without projection
# TODO: check if the weight for male should be higher

df_qxmd.post_tier <-  
  df_qxmd_imputed %>% 
  mutate(qxmd.post = 0.6 * qxmd.female  + 0.4 * qxmd.male,
         grp  = tier_name
  ) %>% 
  select(grp, age, qxmd.post)



# df_qxr_tier
# df_qxd_tier
# df_qxt.
# df_qxm.pre_tier
# df_qxm.post_tier
# df_qxmd_tier



#*******************************************************************************
#        ## Decrements 2: Single decrement table ####
#*******************************************************************************

# df_qxr_tier
# df_qxd_tier
# df_qxt.refund_tier
# df_qxt.vest_tier
# df_qxm.pre_tier
# df_qxm.post_tier
# df_qxm.post_proj_tier

decrements_tier <- 
  expand.grid(age = range_age, 
              ea  = range_ea) %>% 
  mutate(yos = age - ea,
         grp = tier_name) %>% 
  filter(age >= ea) %>% 
  left_join(df_qxm.pre_tier,        by = c("grp", "age")) %>%         # pre-retirement mortality 
  left_join(df_qxm.post_tier,       by = c("grp", "age")) %>%         # post-retirement mortality, healthy
  left_join(df_qxmd.post_tier,      by = c("grp", "age")) %>%         # post-retirement mortality, disabled
  left_join(df_qxt_tier,            by = c("grp", "yos")) %>%         # termination 
  left_join(df_qxr_tier,            by = c("grp", "age")) %>%         # service retirement
  left_join(df_qxd_tier,            by = c("grp", "age")) %>%         # disability

  select(grp, ea, age, yos, 
         qxm.pre, 
         qxm.post, 
         qxmd.post, 
         qxt, 
         qxr, 
         qxd, 
         everything())%>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.)

# decrement_tier




#*******************************************************************************
#        ## Decrements 3: adding eligibility information ####
#*******************************************************************************

# Create 2 columns for each tier
 # elig_servRet_full:  number of year of being eligible for full or greater retirement benefits
 # elig_servRet_early: number of year of being eligible for early retirement benefits; 
 #             0 after being eligible for full retirement benefits

decrements_tier  %<>% 
  group_by(ea) %>% 
  mutate(
    # Eligibility for full (or greater) retirement benefit
    elig_servRet_full = ifelse( ((age >= 60 & yos >= 5)|(yos >= 25)) & age >= 60 , 1, 0) %>% cumsum,
    
    # Eligibility for early retirement benefit
    elig_servRet_early = ifelse( (age >= 60 & yos >= 5)| (yos >= 25), 1, 0) %>% cumsum,
    elig_servRet_early = ifelse( elig_servRet_full, 0, elig_servRet_early)
    ) %>% 

  ## Adjustments to decrement rates based on eligibility
  #   1. Only keep retirement rates when a member is eligible
  #   2. Coerce termination rates to 0 when eligible for early retirement or full retirement, or age >= age_vben 
  
  mutate(
    qxr        = ifelse(elig_servRet_early | elig_servRet_full, qxr, 0),
    qxt        = ifelse((elig_servRet_early == 0 & elig_servRet_full == 0) & age < age_vben, qxt, 0)
  ) %>% 
  ungroup

# decrements_tier %>% 
#   filter(ea == 30)




#*******************************************************************************
#                      ## Decrements 4: Improvement table  ####
#*******************************************************************************

# improvement for post retirement mortality: N/A for MEPERS

# decrements_improvement <- 
#   expand_grid(year =  2017:(2017+14),
#               age  =  range_age) %>% 
#   left_join(
#     bind_rows(
#       # df_qxm.post_imputed %>% mutate(year = 2017),
#       # df_qxm.post_proj_imputed %>% 
#       #   rename_with( ~str_remove(.x, "_proj" )) %>% 
#       #   mutate(year = 2017+14)
#       
#       df_qxm.post_tier %>% mutate(year = 2017),
#       df_qxm.post_proj_tier %>%
#         rename_with( ~str_remove(.x, "_proj" )) %>%
#         mutate(year = 2017+14)
#       ),
#     by = c("year", "age")
#     )
# 
# decrements_improvement %<>% 
#   group_by(age) %>% 
#   arrange(age, year) %>% 
#   # filter(age == 90) %>% 
#   mutate(across(!c(year, grp), ~ seq(first(.x), last(.x), length.out = n()))) %>% 
#   mutate(across(!c(year, grp), ~ .x / .x[year == min(year)])) %>% 
#   rename_with(~ paste0("impr_", .x), !c(year, age, grp)) %>% 
#   mutate(grp = tier_name )

#*******************************************************************************
#                      ## Salary Scale  ####
#*******************************************************************************

# df_salScale_imputed

df_salScale_tier <- 
  df_salScale_imputed %>% 
  mutate(grp = tier_name,
         salScale = salScale_tot ) %>%
  select(grp, yos, salScale) %>%
  arrange(yos)
  

# # groups included
# grp_include <- df_salScale_imputed$grp %>% unique
# grp_include <- grp_include[str_detect(grp_include, "misc|inds")]
# 
# # weight for each group
# wgts <- tibble(grp = grp_include, wgt = 0)
# 
# wgts[wgts$grp == "misc", "wgt"] <-  0.679
# wgts[wgts$grp == "inds", "wgt"] <-  0.046
# wgts
# 
# ## calculate weighted average
# df_salScale_tier <- 
#   df_salScale.merit_imputed %>% 
#   filter(grp %in% grp_include) %>% 
#   left_join(wgts, by = "grp") %>% 
#   group_by(yos, ea) %>% 
#   summarise(salScale.merit = weighted.mean(salScale.merit, wgt),
#             .groups = "rowwise") %>% 
#   mutate(grp = tier_name,
#          salScale.infl = 0.0275,
#          salScale = salScale.merit + salScale.infl) %>% 
#   relocate(grp) %>% 
#   arrange(ea, yos) %>% 
#   ungroup()




#*******************************************************************************
#                      ## Initial demographics  ####
#*******************************************************************************

##  View the inputs
# df_nactives_fillin
# df_n_servRet_fillin
# df_n_beneficiaries_fillin


## groups included 
grp_include <- df_nactives_fillin$grp %>% unique
grp_include <- grp_include[str_detect(grp_include, "regular")]



## Active members

# all active members 
df_n_actives_tier <- 
  df_nactives_fillin %>% 
  filter(grp %in% grp_include) %>% 
  group_by(yos, ea) %>% 
  summarise(salary   = weighted.mean(salary, nactives, na.rm = TRUE) %>% na2zero(),
            nactives = sum(nactives, na.rm= TRUE) %>% na2zero,
            .groups = "drop") %>% 
  mutate(grp = tier_name,
         age = ea + yos) %>% 
  relocate(grp) %>% 
  arrange(ea, age) %>% 
  ungroup() %>% 
  
  #TEMP:
  filter(ea >=20,
         age <= 69,
         yos <= 49) # 153 members removed (1.3% members)

# df_n_actives_tier %>% pull(nactives) %>% sum

# CalPERS: Check total salary againt the AV value: payroll
# sum(df_n_actives_tier$nactives * df_n_actives_tier$salary)
# model/target: 12951558687/12950836352 = 100.0056%


# Keep classic members only
#  assume 
#    - members with yos <= 4 are all pepra members
#    - 50% of members with yos == 5 are pepra members
#    - the rest are classic membrs 

# df_n_actives_tier %<>% 
#   mutate(nactives = case_when(
#     yos <= 4 ~ 0,
#     yos == 5 ~ nactives * 0.5,
#     TRUE ~ nactives
#   ))




## Retirees (all types included)

# assume all service retirees are classic members

df_n_servRet_tier <- 
  df_n_servRet_fillin %>% 
  filter(grp %in% grp_include) %>% 
  group_by(age) %>% 
  summarise(benefit_servRet = weighted.mean(benefit_servRet, n_servRet, na.rm= TRUE),
            n_servRet       = sum(n_servRet, na.rm = TRUE),
            
            .groups = "drop") %>% 
  colwise(na2zero)(.) %>% 
  mutate(grp = tier_name) %>% 
  select(grp, age, n_servRet, benefit_servRet) %>% 
  arrange(age) %>% 
  ungroup()

# MEPERS: Check total benefit againt the AV value (AV2018 ep141-145)
# (df_n_servRet_tier$n_servRet*df_n_servRet_tier$benefit_servRet) %>% sum
# model/target:169508194/169508194  = 100%




## View the results
# df_n_actives_tier
# df_n_servRet_tier


 
#*******************************************************************************
#                    ## Saving tier information in a list  ####
#*******************************************************************************

# collect tier-specific parameters in a list
tier_params <- 
  list(
    tier_name = tier_name,
    age_vben  = age_vben,
    v.year    = v.year,
    fasyears  = fasyears,  # based on policy before PEPRA
    cola_assumed = cola_assumed,
    
    bfactor = bfactor
    #EEC_rate = EEC_rate
  )


# Store all tier data in a list
assign(paste0("tierData_", tier_name), 
       
         list(
           tier_name = tier_name,
           
           decrements = decrements_tier,
           #decrements_improvement = decrements_improvement,
           
           df_n_actives = df_n_actives_tier,
           df_n_servRet = df_n_servRet_tier,
           
           df_salScale  = df_salScale_tier,
           
           tier_params = tier_params
         )
       )

# Save the list of tier data in a .rds (single object) file
saveRDS(get(paste0("tierData_", tier_name)), 
        file = paste0(dir_outputs, "tierData_", tier_name, ".rds"))


# tierData <- readRDS(paste0(dir_outputs, "tierData_", tier_name, ".rds"))


