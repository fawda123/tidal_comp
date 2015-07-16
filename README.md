# Comparison of GAMs and WRTDS for tidal waters
Marcus W. Beck, beck.marcus@epa.gov  

This repository contains materials for comparing generalized additive models (GAMs) and the recent adapation of Weighted Regression on Time, Discharge, and Season (WRTDS) for tidal waters.  Please see the [WRTDStidal](https://github.com/fawda123/wtreg_for_estuaries) page for an R package to implement WRTDS in tidal waters.

### Data files

`bestLE12.RData` Optimal WRTDS model for LE12, created in `create_final_mods.R`
 
`bestTF16.RData` Optimal WRTDS model for TF16, created in `create_final_mods.R`

`obs_dat.RData` Daily chlorophyll and discharge data that is used to create simulated datasets on monthly time steps.

`optimLE12_opt.RData` Results after running `winsrch_optim` for LE12, created in `get_optim.R`

`optimTF16_opt.RData` Results after running `winsrch_optim` for TF16, created in `get_optim.R`

`pax_chldata.RData` Chlorophyll data for Patuxent stations including bottom and surface.

`pax_clip.RData` SpatialPolygonsDataFrame of the Patuxent taken from a cut polygon of Chesapeake Bay.

`pax_data.RData` Processed water quality data for water quality stations in the Patuxent River Estuary, 1985 to 2014.  Created in dat_proc.Rnw.

`pax_fits.RData` Fitted WRTDS results for `pax_data.RData`, created in wrtds_ex.RData. All stations.

`pax_meta.RData` Metadata for water quality stations in the Patuxent River Estuary, created in dat_proc.Rnw.

`pax_wrtds.RData` WRTDS fits for LE1.2, TF1.6, flow and salinity as explanatory variables, set half-window widths of 0.5 mos, 10 yrs, and 0.5 flo/sal
