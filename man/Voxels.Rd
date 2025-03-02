% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Voxels.R
\name{Voxels}
\alias{Voxels}
\title{Voxelize point cloud}
\usage{
Voxels(a, filename = "XXX", dimVox = 2, th = 2, output_path = tempdir())
}
\arguments{
\item{a}{- input file}

\item{filename}{- file output prefix}

\item{dimVox}{- voxel dimension in cm - Default = 2}

\item{th}{Minimum number of point to generate a voxel (Default = 1)
Is a parameter that should be used with caution;
it generates a lightened cloud with fewer points.
To be evaluated in relation with the dimVox parameter,
for high point densities it is efficae to remove noise (outliers)}

\item{output_path}{Directory in cui scrivere i file di output. Default = tempdir()}
}
\value{
Voxelized pointcloud
}
\description{
Transform pointcloud in voxel
}
