# Comparison of GAMs and WRTDS for tidal waters
Marcus W. Beck, beck.marcus@epa.gov  

This repository contains materials for comparing generalized additive models (GAMs) and the recent adapation of Weighted Regression on Time, Discharge, and Season (WRTDS) for tidal waters.  Please see the [WRTDStidal](https://github.com/fawda123/wtreg_for_estuaries) page for an R package to implement WRTDS in tidal waters.

### Data files
 
`pax_chldata.RData` Chlorophyll data for Patuxent stations including bottom and surface.

`pax_clip.RData` SpatialPolygonsDataFrame of the Patuxent taken from a cut polygon of Chesapeake Bay.

`pax_data.RData` Processed water quality data for water quality stations in the Patuxent River Estuary, 1985 to 2014.  Created in dat_proc.Rnw.

`pax_fits.RData` Fitted WRTDS results for `pax_data.RData`, created in wrtds_ex.RData.

`pax_meta.RData` Metadata for water quality stations in the Patuxent River Estuary, created in dat_proc.Rnw.
