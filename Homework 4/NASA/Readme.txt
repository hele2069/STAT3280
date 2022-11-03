This folder contains the data of 24x24 grid of central america
atmosphere measurements. There are 72 months over 6 years, and for
each measurment, we have the monthly average at the 24x24 locations in
each of the 72 months, thus at each location, it is a time series of
length 72 for each measurement. At four locations, there are many
missing values, due to the high elevation. 

The R code DataStructure.R creat a grid network adjacency matrix corrsponding to the
grid. Then it transforms the data files to be a 576 x 7 data matrix at
each time point with
each row being a measurments at one location,
corresponding to the order in adjacency matrix. The 72 snapshots are
stored as a list.

One can potentially use the data as a network with 7 covariates
associated.
