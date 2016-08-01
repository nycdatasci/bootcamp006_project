# NYCDSA Shiny project
# David Richard Steinmetz
# davidsteinmetz@gmail.com
# Last updated on: 2016-07-28

# Set working directory to Github dir
if (getwd()=='D:/Projects/DataScienceBootcamp/22_Project2_Shiny') {
    setwd('02_Selfgen/01_App/')
}

# Source ------------------------------------------------------------------

source('02_scripts/01_load_libraries.R')
source('02_scripts/02_load_and_clean_data.R')
source('02_scripts/03_inspect.R')

# Plots -------------------------------------------------------------------

# Bar chart of categorical values
g <- ggplot(data=freq_tab, aes_string(x=col,y='N'))
g <- g + geom_bar(stat='identity')
g <- g + ggtitle(paste('Frequency Distribution for',col))
g <- g + xlab(col_formatted)
g <- g + ylab('Frequency')
g

# Sorted horizontal bar plot
g <- ggplot(data=freq_tab[order(N)], 
            aes_string(x=as.integer(rownames(freq_tab)),y='N'))
g <- g + geom_bar(stat='identity') + coord_flip()
g <- g + ggtitle(paste('Sorted Frequency Distribution for',col))
g <- g + xlab(col_formatted)
g <- g + ylab('Frequency')
g

# Density plot
g <- ggplot(data=acc[,col,with=FALSE], aes_string(x=col))
g <- g + geom_density()
g <- g + ggtitle(paste('Frequency Distribution for',col))
g <- g + xlab(col_formatted)
g <- g + ylab('Frequency')
g





