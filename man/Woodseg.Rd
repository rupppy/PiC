% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Woodseg.R
\name{Woodseg}
\alias{Woodseg}
\title{Wood voxels segmentation}
\usage{
Woodseg(a, filename = "XXX", eps = 1, mpts = 4, N = 1000, R = 30, output_path = tempdir())
}
\arguments{
\item{filename}{- Output file prefix}

\item{a}{- AGB voxelized input file}

\item{eps}{- size (radius) of the epsilon neighborhood - Default = 1}

\item{mpts}{- number of minimum points required in the eps neighborhood for core points (including the point itself) - Default = 4}

\item{N}{- Minimum number of voxel in a wood cluster - Default = 1000}

\item{R}{- R = Standard deviation * Proportion of Variance - Default = 30}

\item{output_path}{Directory in cui scrivere i file di output. Default = tempdir()}
}
\value{
One file (.txt) output - Wood voxels (vox)
}
\description{
Point cloud segmentation to identify wood voxels
}
