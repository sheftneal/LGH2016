# LGH2016

Data and selected code from 2016 LGH paper [`Sources of variation in under-5 mortality across sub-Saharan Africa: a spatial analysis'](https://www.sciencedirect.com/science/article/pii/S2214109X16302121) by Burke, Heft-Neal, and Bendavid



### Data Download
See [README](https://github.com/sheftneal/LGH2016/blob/master/data/outputs/final_data/DATA-README.txt) for further documentation.

**Points Data**: Data frame stored as RDS file with latitude, longitude, child mortality estimates for 1980s, 1990s, and 2000s, and associated uncertainty estimates. 

[Download points data](https://github.com/sheftneal/LGH2016/tree/master/data/outputs/final_data/ChildMortEstimates5m0_points.rds).

**Gridded Data**: Gridded data stored as tif with one layer each for child mortality estimates for 1980s, 1990s, and 2000s, respectively.

[Download gridded data](https://github.com/sheftneal/LGH2016/tree/master/data/outputs/final_data/ChildMortEstimates5m0_gridded.tif). 

Note while the points data includes uncertainty estimates the gridded data does not.



### Figures
All published figures are available for download [here](https://github.com/sheftneal/LGH2016/tree/master/figures/final).


### Scripts
Scripts to generate the data and figures from the paper (*in progress*).

* [scripts/1_interpolation](https://github.com/sheftneal/LGH2016/tree/master/scripts/1_interpolation) - Script for interpolating the mortality data from DHS cluster averages (raw DHS data not included due to licensing)
* [scripts/2_makefigures](https://github.com/sheftneal/LGH2016/tree/master/scripts/2_makefigures) - Script for generating figures from the final data




### Questions or comments?

Contact Sam Heft-Neal at sheftneal@stanford.edu.