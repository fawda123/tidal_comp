# Comparison of GAMs and WRTDS for tidal waters
Marcus W. Beck, beck.marcus@epa.gov  

This repository contains materials for comparing generalized additive models (GAMs) and the recent adapation of Weighted Regression on Time, Discharge, and Season (WRTDS) for tidal waters.  Please see the [WRTDStidal](https://github.com/fawda123/wtreg_for_estuaries) page for an R package to implement WRTDS in tidal waters.

### Data files

`bestLE12.RData` combined dataset for best WRTDS and GAM for LE12, includes observed data, created in `create_final_mods.R`

`bestLE12_wrtds.RData` Optimal WRTDS model for LE12, created in `create_final_mods.R`, files with `_min` or `_nomin` suffix are used in `samp_size.R` to evaluate sample sizes for the interpolation grids using a restriction on number of obsevations with greater than zero weights and without.  
 
`bestLE12_gams.RData` Optimal GAM for LE12, created in `create_final_mods.R`
 
`bestsim_gam.RData` Best GAMs for each of three simulated monthly time series, created in `create_final_mods.R`.
 
`bestsim_wrtds.RData` Best WRTDS model objects for each of three simulated monthly time series, created in `create_final_mods.R`.
 
`bestTF16.RData` combined dataset for best WRTDS and GAM for TF16, includes observed data, created in `create_final_mods.R`
 
`bestTF16_wrtds.RData` Optimal WRTDS model for TF16, created in `create_final_mods.R`, files with `_min` or `_nomin` suffix are used in `samp_size.R` to evaluate sample sizes for the interpolation grids using a restriction on number of obsevations with greater than zero weights and without. 

`bestTF16_gams.RData` Optimal GAM for TF16, created in `create_final_mods.R`

`obs_dat.RData` Daily chlorophyll and discharge data that is used to create simulated datasets on monthly time steps, created in `sim_dat.R`.

`optimLE12_opt.RData` Results after running `winsrch_optim` for LE12, created in `get_optim.R`, files with `_min` or `_nomin` suffix are used in `samp_size.R` to evaluate sample sizes for the interpolation grids using a restriction on number of obsevations with greater than zero weights and without. 

`optimTF16_opt.RData` Results after running `winsrch_optim` for TF16, created in `get_optim.R`, files with `_min` or `_nomin` suffix are used in `samp_size.R` to evaluate sample sizes for the interpolation grids using a restriction on number of obsevations with greater than zero weights and without. 

`pax_chldata.RData` Chlorophyll data for Patuxent stations including bottom and surface.

`pax_clip.RData` SpatialPolygonsDataFrame of the Patuxent taken from a cut polygon of Chesapeake Bay.

`pax_data.RData` Processed water quality data for water quality stations in the Patuxent River Estuary, 1985 to 2014.  Created in dat_proc.Rnw.

`pax_meta.RData` Metadata for water quality stations in the Patuxent River Estuary, created in dat_proc.Rnw.

`sim1_opt.RData` Results of `winsrch_optim` for sim1 dataset in `sims_mos.RData`, created in `get_optim.R.`

`sim2_opt.RData` Results of `winsrch_optim` for sim2 dataset in `sims_mos.RData`, created in `get_optim.R.`

`sim3_opt.RData` Results of `winsrch_optim` for sim3 dataset in `sims_mos.RData`, created in `get_optim.R.`

`sim_res.RData` Combined results from WRTDS and GAM for predictions and flow-normalized predictions for each of three simulated time series in `sims_mos.RData`, created in `create_final_mods.R`.  Data are in long format with columns for `date`, `lnchla_noQ ` (biological chlorophyll for comparison with `norm`), `simval` (simulated time series as observed to predict with each model), `mod` (model type), `sim` (which simulation the results apply to), `fits` (model fits to `simval`), and `norm` (flow-normlized predictions to compare to `lnchla_noQ`)

`sims_day.RData` Simulated data for manuscript of daily time series that includes chlorophyll with varying effect of discharge (sim1 none, sim2 constant, sim3 increasing).  `lnchla_noQ` is the biological component of chlorophyll, `lnQ_sim` is simulated discharge and was used to simulate the flow effect of chlorpohyll.  Created in `sim_dat.R`.

`sims_mos.RData` Same dataset as `sims_day.RData` but using a monthly time step.  Created in `sim_dat.R`.
