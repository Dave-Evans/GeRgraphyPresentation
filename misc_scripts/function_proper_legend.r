#### for proper legend #####
# returns proper legend text for intervals created from classInterval-color object, the output of findColours
properLegend <- function(prop_colr, sig_figs = 0){
    legTxt <- attr(attr(prop_colr,'table'),'dimnames')[[1]]
    newTxt <- NULL
    for (rw in 1:length(legTxt)){
        r <- legTxt[rw]
        splt <- strsplit(r, split=',')[[1]]
        opn <- as.numeric(strsplit(splt[1],'[',fixed=T)[[1]][2])
        if (rw == length(legTxt)){
            cls <- as.numeric(strsplit(splt[2],']')[[1]][1])
        } else {
            cls <- as.numeric(strsplit(splt[2],')')[[1]][1])
        }
#         print(paste(opn,cls))
        opn <- round(opn, sig_figs)
        cls <- round(cls, sig_figs)
        nw <- paste('[',opn,',',cls,')',sep='')
        newTxt <- c(newTxt, nw)
    }
    return(newTxt)
}
