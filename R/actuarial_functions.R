############################################################h#
### PRIVATE UTILITY FUNCTIONS => Not to be exported ##########
############################################################h#


annuity = function(table, i = 0, age = begin - YOB, YOB = begin - age, begin = YOB+age, verbose = FALSE, n = getOmega(table) - age, selectionAge = 0) {
    ages = age:(age + n)
    qx = deathProbabilities(table, YOB = YOB, ages = ages, selectionAge = selectionAge)
    v = 1/(1+i)
    ax = head(Reduce(function(qqx, ax1) { 1 + v * (1 - qqx) * ax1}, qx, 1, right = TRUE, accumulate = TRUE), -1)
    if (verbose) {
        data.frame(age = ages, qx = qx, ax = ax)
    } else {
        ax[1]
    }
}
