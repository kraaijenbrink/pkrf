% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkDownDriveDir.r
\name{getGDriveDir}
\alias{getGDriveDir}
\alias{pkDownDriveDir}
\title{Download Google Drive folder contents}
\usage{
getGDriveDir(drivePath, localPath, email = NULL, rmFromDrive = FALSE)

pkDownDriveDir(drivePath, localPath, email = NULL, rmFromDrive = FALSE)
}
\arguments{
\item{drivePath}{Path to directory on Google Drive that contains the files to download.}

\item{localPath}{Path where the files will be stored. Will be created if inexistent. No warnings if exists.}

\item{email}{Google account email address.}

\item{rmFromDrive}{Remove the files from Google Drive after download (logical).}
}
\value{
Nothing
}
\description{
Download content of entire folder on Google Drive to a local directory
}
