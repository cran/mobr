## ----install package, eval = F------------------------------------------------
#  install.packages('devtools')
#  library(devtools)
#  install_github('MoBiodiv/mobr')

## ----load pkg and data, message=FALSE-----------------------------------------
library(mobr)
data(inv_comm)      # Community matrix
data(inv_plot_attr) # Plot attributes data.frame

## ----examine data-------------------------------------------------------------
str(inv_comm)
head(inv_plot_attr)

## ----make mob_in--------------------------------------------------------------
inv_mob_in <- make_mob_in(inv_comm, inv_plot_attr, coord_names = c('x', 'y'))
inv_mob_in

## ---- fig.width = 5, fig.height = 5-------------------------------------------
plot_rarefaction(inv_mob_in, 'group', 'sSBR', lwd = 4)

## ---- fig.width = 7, fig.height=4---------------------------------------------
oldpar <- par(no.readonly = TRUE)       
par(mfrow = c(1, 2))
plot_rarefaction(inv_mob_in, 'group', 'IBR', pooled = FALSE, lwd = 2,
                 leg_loc = 'topright')
plot_rarefaction(inv_mob_in, 'group', 'IBR', pooled = TRUE, lwd = 4,
                 leg_loc = NA)
par(oldpar)

## ---- fig.width = 7, fig.height=4---------------------------------------------
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2))
plot_abu(inv_mob_in, 'group', type = 'rad', pooled = FALSE, log = 'x')
plot_abu(inv_mob_in, 'group', type = 'rad', pooled = TRUE , log = 'x')
par(oldpar)

## ----run two-scale analysis, eval = FALSE-------------------------------------
#  inv_stats <- get_mob_stats(inv_mob_in, group_var = "group",
#                             n_perm = 199)

## ---- echo = FALSE------------------------------------------------------------
load('../vignettes/inv_stats.Rdata')

## -----------------------------------------------------------------------------
names(inv_stats)

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(inv_stats, 'S')

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(inv_stats, 'N')

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(inv_stats, 'S_n')

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(inv_stats, 'S_PIE')

## ---- eval=FALSE--------------------------------------------------------------
#  plot(inv_stats)

## ----multi-scale analysis, eval=FALSE-----------------------------------------
#  inv_deltaS = get_delta_stats(inv_mob_in, 'group', ref_level='uninvaded',
#                               type='discrete', log_scale=TRUE, n_perm = 199)

## ---- echo=FALSE--------------------------------------------------------------
load('../vignettes/inv_deltaS.Rdata')

## ---- fig.width=7, fig.height=3.5---------------------------------------------
plot(inv_deltaS, stat = 'b1', scale_by = 'indiv', display='S ~ effort')

## ---- fig.width=7, fig.height=3.5---------------------------------------------
plot(inv_deltaS, stat = 'b1', scale_by = 'indiv', display='stat ~ effort')

## ----load fire data-----------------------------------------------------------
# plant community in response to a prescribed fire treatment in a
# central US woodland
data(fire_comm)
data(fire_plot_attr)
fire_mob_in = make_mob_in(fire_comm, fire_plot_attr,
                          coord_names = c('x', 'y'))

## -----------------------------------------------------------------------------
plot_rarefaction(fire_mob_in, 'group', 'IBR')
plot_rarefaction(fire_mob_in, 'group', 'sSBR')

## -----------------------------------------------------------------------------
oldpar <- par(no.readonly = TRUE) 
par(mfrow = c(1, 2))
plot_abu(fire_mob_in, 'group', 'rad', leg_loc = 'topright',)
plot_abu(fire_mob_in, 'group', 'sad', leg_loc = NA)
par(oldpar)

## ---- eval = FALSE------------------------------------------------------------
#  fire_stats = get_mob_stats(fire_mob_in, 'group')

## ---- echo = FALSE------------------------------------------------------------
load('../vignettes/fire_stats.Rdata')

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(fire_stats)

## ---- eval = FALSE------------------------------------------------------------
#  fire_deltaS = get_delta_stats(fire_mob_in, 'group', ref_level = 'unburned',
#                                type = 'discrete', log_scale = T, n_perm = 199,
#                                overall_p = T)

## ---- echo = FALSE------------------------------------------------------------
load('../vignettes/fire_deltaS.Rdata')

## ---- fig.width = 7, fig.height = 6-------------------------------------------
plot(fire_deltaS, stat = 'b1', scale_by = 'indiv')

## -----------------------------------------------------------------------------
data(tank_comm)
data(tank_plot_attr)
tank_mob_in = make_mob_in(tank_comm, tank_plot_attr,
                          coord_names = c('x', 'y'))

## ---- fig.width = 5, fig.height = 5-------------------------------------------
plot_rarefaction(tank_mob_in, 'group', 'sSBR')

## ---- fig.width = 7, fig.height=4---------------------------------------------
oldpar <- par(no.readonly = TRUE) 
par(mfrow = c(1, 2))
plot_abu(tank_mob_in, 'group', 'rad')
plot_abu(tank_mob_in, 'group', 'sad', leg_loc = NA)
par(oldpar)

## ---- eval = FALSE------------------------------------------------------------
#  tank_stats = get_mob_stats(tank_mob_in, 'group')

## ---- echo = FALSE------------------------------------------------------------
load('../vignettes/tank_stats.Rdata')

## ---- fig.width = 7, fig.height = 3.5-----------------------------------------
plot(tank_stats)

## ---- eval = FALSE------------------------------------------------------------
#  tank_deltaS = get_delta_stats(tank_mob_in, 'group', ref_level = 'low',
#                                inds = 10, log_scale = TRUE, type = 'discrete',
#                                n_perm=199)

## ---- echo = FALSE------------------------------------------------------------
load('../vignettes/tank_deltaS.Rdata')

## ---- fig.width = 7, fig.height = 6-------------------------------------------
plot(tank_deltaS, stat = 'b1')

