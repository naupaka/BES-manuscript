# BES-manuscript

# Materials and Methods

To compute a soil flux measurement, we acquired environmental
observations and associated uncertainties from NEON servers using the
`NEONUtilities` R package. Specific data products included half hourly
soil water content, half-hourly soil CO$_{2}$, half-hourly atmospheric
pressure, half-hourly soil temperature, and soil properties (e.g. bulk
density). These static soil properties are periodically collected and
assumed to be constant.

Each NEON site samples from three to five different spatial positions at
three to five different soil depths. The `NEONSoils` R package acquires
the data for a given site and month and stacks observations into one
nested list column for ease of computation or export measurements into a
csv file. Quality flags with an observation are reported as an indicator
variable. If a given observation did not pass are reported we applied a
gap filling method (described below) the measurements monthly mean was
used.

Following this initial data processing, temperature, soil water content
were interpolated using splines to the same depth at CO$_{2}$
concentration measurements. CO$_{2}$ was converted to molar
concentration ($\mu$mol m$^{-3}$).

## Computation of soil fluxes

For a given half hour, we applied Fick’s law (Equation ) to estimate the
soil flux $F_{s}$, units $\mu$mol m$^{-2}$ s$^{-1}$:

where $D_{a}$ is the diffusion coefficient (m$^{2}$ s$^{-1}$).
Computation of $D_{a}$ equals:

The diffusion

Diagram of soil flux package flow in R Mathematical description of soil
flux calculation We use Ficks law to compute the flux: Fc - Da*dC/dz.
The diffusion coefficient is computed by Da = tortuosity*diffusion
coefficient in free air diffusion coefficient in free air = 0.0000147 \*
((temperature - absZero) / (20 - absZero))^1.75 \* (pressure / 101.3)
Tortosity = (porosity - swc)^(10/3) / porosity^2
\[@millingtonDiffusionAggregatedPorous1971\] \# Calculate 2-20 mm rock
volume (cm3 cm-3). Assume 2.65 g cm-3 density.

We compute a site specific soil volume fraction ($f_{V}$, cm${^3}$
cm$^{-3}$) with particle densities of different size classes (less than
2 mm, 2-5 mm, and 5-20 mm). We assume the particle density of mineral
soil ($\rho_{m}$) equals 2.65 g cm$^{-3}$.

$f_{rock} = \left(1- \frac{\rho_{Coarse}}{\rho_{m}} \right)\left(1-\frac{\rho_{2-5}}{\rho_{m}}+\frac{\rho_{5-20}}{\rho_{m}}\right)$

WHY ARE THESE PROPORTIONALLY MULTIPLED??

porosSub2mm \<- 1 -
mgp$bulkDensExclCoarseFrag/2.65  Porosity = (1 - mgp$bulkDensExclCoarseFrag/2.65)\*
(1 - rockVol). This includes the different fractions from 0 to 20 mm of
the rocks - the fraction that are below 2mm and then 2-20mm rock volume.
The soil particle density is assumed to be 2.65 g cm^-3 CO2 in molar
density computed. We inferred the surface gradient of soil CO2 molar
concentration from the three closest measurement depths to the surface.
To estimate the surface gradient of soil CO2 molar concentration
(dC/dz), For ease of use, we say the triplet (zi, Ci, Di) is the depth,
co2 molar concentration and diffusivity at a measured depth i. Four
different approaches were used in the flux gradient method, based on a
review from Maier Schack Kirchner. De Jong and Schappert (1972) computes
a linear regression of CO2 molar concentration at depth. Once that is
computed, the surface gradient is then approximated by F = -D1 \* (C0 -
C1)/(0-z1), where (z1,C1) is the measurement depth closest to the
surface. The The flux is estimated by does a linear regression for the
C(z) and then uses C0 Hirano et al (2005) also takes the linear
regression to create a new vector and the center between each
measurement depth C = (C0, Ci) and z = (0, zi) and then estimates the
gradient at each depth. (dC/dz)\_i. A flux at each depth is computed to
then extrapolate the surface concentration. \[\[ FILL IN MORE HERE \]\]
Tang et al 2003 assumes the co2 molar density and diffusivity increases
linearly through the soil profile, so the computed surface flux is the
extrapolated diffusivity times the slope of the linear regression of CO2
vs z. Tang et al 2005 computed the mean diffusivity between two
different layers and then computed the gradient at each layer for a
diffusive flux. This was then extrapolated to the surface concentration.
These four methods were then implemented along with any uncertainty
analysis (found via quadrature).

Error handling We use the fact that reported error measurements occur at
a given sampling location i and depth j. NEON reports QF flags as a
binary value. Since we want the QF for a given flux measurement to
represent 0 if no measurements across depths were fine, and 1 if either
a smoothed mean was used for a given depth. We assign an aggregate QF
value for a given time. Figure XXX shows representative plots for the
SJER site across the timeperiod studied

Gap filling module To compute the monthly mean we used a bootstrapped
sample of measurements from the reported measurements which passed the
QC and the uncertainty by bootstrapping. We assume that the measurement
comes from a normally distributed distribution with mean and standard
deviation. A given number of bootstrap samples are specified so that new
replicate samples are created, then the mean is taken. a measurement a
specified number of times \[LINK\] with x + epsilon Assume there is a
vector of means x, standard deviation, and 95% confidence interval of
the uncertainty. The bias in a measurement is given by the difference
between the 95% confidence interval and the standard deviation in
quadrature standard deviation and For a specified number of bootstrap
samples. Data processing & validation For a given month, the user than
can return the computed diffusivity, flux at each sampling location at a
NEON site. The user can assess the final quality flag assesed for each
of the 4 environmental measurements (CO2, temperature, and
