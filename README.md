[![lint](https://github.com/NIEHS/brassens/actions/workflows/lint.yaml/badge.svg)](https://github.com/NIEHS/brassens/actions/workflows/lint.yaml)
[![DOI](https://zenodo.org/badge/775679144.svg)](https://doi.org/10.5281/zenodo.15596059)

# mercury

## Run a comparative analysis of daily temperature products across the US

We investigate the Urban Heat Island (UHI) representativeness in different products:

- daymet (daily ~1km- resolution)

Thornton, P. E., R. Shrestha, M. Thornton, S.-C. Kao, Y. Wei, and B. E. Wilson. 2021. Gridded daily weather data for North America with comprehensive uncertainty quantification. Scientific Data 8. https://doi.org/10.1038/s41597-021-00973-0

Thornton, P.E., Running, S.W., White, M.A. 1997. Generating surfaces of daily meteorological variables over large regions of complex terrain. Journal of Hydrology 190: 214 - 251. https://doi.org/10.1016/S0022-1694(96)03128-9

- gridMET (daily ~4km-resolution)

Abatzoglou, J. T. (2013), Development of gridded surface meteorological data for ecological applications and modelling. Int. J. Climatol., 33: 121–131.

- mobile measurement campaign HEATWATCH (NOAA + CAPA) 

- our own product developped with a Bayesian approach with personal weather stations [samba](https://github.com/NIEHS/samba) (hourly, ~1km-resolution)

Marques, E., & Messier, K. P. (2025). Improved high resolution heat exposure assessment with personal weather stations and spatiotemporal Bayesian models. Authorea Preprints.
