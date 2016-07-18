# Melt separate columns into one column
melt_count <- function(df, melt_with, melt_string, melt_target) {
  temp <- 
    df %>%
    select(ID = `UNIQUE KEY`,
           # Conditional match for different melt needs
           switch(melt_with, 
                  starts = starts_with(melt_string),
                  ends   = ends_with(melt_string),
                  matches = matches(melt_string)
           )
    )                                  %>% # Select only factors & key
    melt(id = 'ID')                    %>% # Melt factors to one column
    filter(!value %in% c('', 0)) # Filter out blank factor rows
    # return (temp)}
    switch(melt_target,
           cat  = { # Categorical value
             output <- temp            %>%
               mutate(value_count = 1) %>% # Add count to each row
               group_by(ID, value)     %>%
               summarise(TOTAL = sum(value_count))
           },
           stat = { # Statisticcal value
             output <- temp            %>%
               group_by(ID, variable)  %>%
               summarise(TOTAL = sum(value))
           }
    )
  output <- as.data.table(output)
  colnames(output) <- c('ID', melt_string, paste(melt_string, 'V', sep = '_'))
  return(output)
}
