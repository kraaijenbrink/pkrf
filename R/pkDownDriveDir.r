# download files from google drive folder to local folder
# philip kraaijenbrink


#' Download Google Drive folder contents
#'
#' Download content of entire folder on Google Drive to a local directory
#' @param drivePath Path to directory on Google Drive that contains the files to download.
#' @param localPath Path where the files will be stored. Will be created if inexistent. No warnings if exists.
#' @param authFile Path to RDS file with authentication token. If not exist, web authentication is performd and new file is created at path.
#' @param rmFromDrive Remove the files from Google Drive after download (logical).
#'
#' @return Nothing
#' @export
getGDriveDir <- function(drivePath, localPath, authFile, rmFromDrive=FALSE){

  dp <- require(googledrive); if(dp==FALSE){stop('"googledrive" package is missing')}
  # authenticate with google
  if (file.exists(authFile)){    # use provided auth file
    drive_auth(authFile)
  }else{                         # use browser and generate auth file
    token <- drive_auth(cache=F)
    saveRDS(token,authFile)
  }
  
  # get list of files in provided folder
  files <- drive_ls(drivePath)
  
  # create output directory
  dir.create(localPath, showWarnings=F, recursive=T)
  
  # loop over files and download to local dir
  for (i in 1:nrow(files)){
    cat(paste0('\ndownloading file ', i, ' of ',nrow(files),'\n'))
    drive_download(file=files[i,], path=file.path(localPath, as.character(files[i,1])), overwrite=T)
    if (rmFromDrive){
      drive_rm(file=files[i,])   # remove file from server
    }
  }
  return(NULL)
}


#' @rdname getGDriveDir
#' @export
pkDownDriveDir <- getGDriveDir