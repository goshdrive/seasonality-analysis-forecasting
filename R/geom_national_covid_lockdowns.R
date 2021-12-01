##' @param shade_ld Logical setting whether to shade lockdowns or not
geom_national_covid_lockdowns <- function(shade_ld = FALSE) {
    
    c <- 'red'  # get_nhs_colours('Red')
    
    # Add start and finish lines
    ggp <- geom_vline(
        xintercept=as.double(national_covid_lockdowns[[1]][[1]]),
        color = c, alpha = 0.5, size = 0.8)
    
    ggp <- append(ggp, geom_vline(
        xintercept=as.double(national_covid_release),
        color = c, alpha = 0.5, size = 0.8))
    
    # Add lockdown periods
    if (shade_ld) {
        for (ld in national_covid_lockdowns) {
            ggp <- append(ggp,
                          annotate(
                              geom = "rect",
                              ymin = -Inf, ymax = Inf,
                              xmin = ld[[1]], xmax = ld[[2]],
                              fill = c, alpha = 0.2
                          ))
        }
    }
    
    return(ggp)
}
