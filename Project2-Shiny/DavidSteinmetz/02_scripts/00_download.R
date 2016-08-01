# NYCDSA Shiny project
# David Richard Steinmetz
# davidsteinmetz@gmail.com
# Last updated on: 2016-07-31


# Define download function ------------------------------------------------

dlefile <- function(filename, url, loc_file, loc_dir=NULL, ext_files=NULL, tsmp='timestamp.txt'){
    if (is.null(loc_dir)) loc_dir<-getwd()
    
    # Download file
    rem_file <- paste(url, filename, sep='')
    if (!file.exists(loc_file)) {
        download.file(url = rem_file, destfile = loc_file, mode = 'wb')
    } else {
        print('File exists, will not overwrite')
        return()
    }
    
    # Record timestamp of download
    timestamp <-
        as.character(strptime(date(), format = '%a %b %d %H:%M:%S %Y'))
    write(timestamp, file = paste(loc_dir, tsmp, sep = ''), append = TRUE)
    
    # Extract files
    if (!is.null(ext_files)) {
        if (file.exists(loc_file)) {
            if (!dir.exists(loc_dir))
                dir.create(loc_dir)
            curdir <- getwd()
            setwd(loc_dir)
            unzip(filename, files = ext_files, overwrite = FALSE)
            setwd(curdir)
        } else {
            print('File exists, will not extract')
            return()
        }
    }
}

# Set parameters ----------------------------------------------------------

# Set file locations
# filename <- 'FARS2014.zip'
# filename <- 'NST-EST2014-01.csv'
filename <- 'oes_research_2015_sec_48-49.xlsx'
timestamp_file <- 'timestamp.txt'
# url <- 'ftp://ftp.nhtsa.dot.gov/FARS/2014/DBF/'
# url <- 'https://www.census.gov/popest/data/state/totals/2014/tables/'
url <- 'http://www.bls.gov/oes/special.requests/'
loc_dir <- '01_data/'
loc_file <- paste(loc_dir,filename,sep='')
# ext_files = c('accident.dbf') # files to extract


# Download and extract files ----------------------------------------------

dlefile(filename = filename, url = url, loc_file = loc_file, 
        loc_dir = loc_dir)


# Cleanup environment -----------------------------------------------------

rm(filename, timestamp_file, url, rem_file, loc_dir, loc_file, ext_files)
