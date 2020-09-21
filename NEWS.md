## version 2.0.0

---

### new features
- Gradient multi-scale mob method added to the function `get_delta_stats` as
described in McGlinn et al. ([2020](https://doi.org/10.1101/851717)
- Added a new k-nearest neighbor algorithm for the spatial sample based rarefaction curve (sSBR)
- ggplot integrated into plotting method for `get_delta_stats` -> `plot.mob_out`
- Added automated tests
- Improved documentation throughout the package

### bug fixes
- Function `sphere_dist` did not appear to be calculating great circle distances
from latitude and longitude coordinates correctly using the Haversine formula.
This was corrected with a new function this is correct according to comparisons
with `fields::rdist.earth` and `geosphere::distHaversine`.

### omitted feature
- Removed the function `overlap_effects` which provided an overlapped image of the three components SAD, N, and agg across scale. This may be added back in the future

### code refactoring
- Refactored code surrounding the `get_delta_stats` function which computes the 
differences in richness across all possible scales. 
- Refactoring was primarily accomplished by integrating in the tidyverse tools
which should make the code easier to understand and follow
- Computational time has increased as the tidyverse tools are not necessarily 
the most computationally efficient solutions
 



## version 1.0.0

---

- provided core functionality of mob tools described in McGlinn et al. 
([2019](https://doi.org/10.1111/2041-210X.13102)) and Chase et al. 
([2018](https://doi.org/10.1111/ele.13151)) 
