#-*- coding: latin-9 -*-

### File: splitAmongPings_priorityDay.R
### Time-stamp: <2019-03-03 11:49:15 yreecht>
###
### Created: 02/03/2019	16:32:37
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################



##' Re-implementation of splitAmongPings, optionally conservative at the day level with priority over ICES rectangle.
##' Based on the original function splitAmongPings in the vmstools package which splits the values or landings as listed in the
##' eflalo file over the tacsat pings, while taking different levels into account such as by day, ICESrectangle or by
##' trip number. Also there is a possibility to merge the eflalo records without a matching tacsat trip.
##' @title splitAmongPings2: Split values or landings from eflalo over tacsat pings
##' @param tacsat Tacsat object
##' @param eflalo Eflalo object
##' @param variable Indicating what to split: "all","value","kgs"
##' @param level Levels can be: "day", "ICESrectangle", "trip"
##' @param conserve Logical, if kgs or value needs to be conserved if merging by trip number is not possible (default =
##'     TRUE)
##' @param by Name of tacsat column by which KG and EURO should be dispatched. Default to NULL which distributes KG and
##'     EURO equally by each ping. A tacsat column can be used instead to generate a 'weighted' dispatch of KG and EURO.
##' @param priorityDay Logical, whether to give priority to days over ICESrectangles. Behave like the original function
##'     by default.
##' @return Merged tacsat file will be returned including the splitted values over the tacsat pings where SI_STATE is
##'     not zero.
##' @author Niels T. Hintzen, Francois Bastardie and Yves Reecht
splitAmongPings3 <- function (tacsat, eflalo, variable = "all", level = "day", conserve = TRUE,
                              by = NULL, priorityDay = FALSE)
{
    ## Argument (partial) matching:
    level <- match.arg(level, c("day", "ICESrectangle", "trip"), several.ok = FALSE)
    variable <- match.arg(variable, c("all", "kgs", "value"), several.ok = FALSE)

    if (!"FT_REF" %in% colnames(tacsat))
        stop("tacsat file needs FT_REF detailing trip number")
    if (!"SI_STATE" %in% colnames(tacsat))
        stop("tacsat file needs SI_STATE detailing activity of vessel")
    if (level == "trip" & conserve == TRUE)
        stop("conserve catches only at level = ICESrectangle or day")
    if (!"SI_DATIM" %in% colnames(tacsat))
        tacsat$SI_DATIM <- as.POSIXct(paste(tacsat$SI_DATE, tacsat$SI_TIME,
                                            sep = " "), tz = "GMT", format = "%d/%m/%Y  %H:%M")
    if (!"LE_CDATIM" %in% colnames(eflalo))
        eflalo$LE_CDATIM <- as.POSIXct(eflalo$LE_CDAT, tz = "GMT",
                                       format = "%d/%m/%Y")
    if ( ! is.null(by))
    {
        if (any(is.na(tacsat[, by])) | any(tacsat[, by] == 0))
            stop("'by' column in tacsat contains NA or zero's. Cannot execute with NA's or zeros")
    }

    if (level == "day") {
        level <- c("day", "ICESrectangle", "trip")
    }else{
        if (level == "ICESrectangle")
        {
            level <- c("ICESrectangle", "trip")
        }else{
            if (level == "trip")
            {
                level <- c("trip")
            }
        }
    }

    tacsat$ID <- 1:nrow(tacsat)
    eflaloCol <- colnames(eflalo)
    kgs <- grep("LE_KG", colnames(eflalo))
    eur <- grep("LE_EURO", colnames(eflalo))
    tacsat <- subset(tacsat, SI_STATE != 0)
    tacsatTrip <- subset(tacsat, FT_REF != 0)
    remainTacsat <- sort(unique(tacsatTrip$ID))
    eflalo$ID <- 1:nrow(eflalo)
    eflaloTrip <- subset(eflalo, FT_REF %in% sort(unique(tacsatTrip$FT_REF)) &
                                 VE_REF %in% sort(unique(tacsatTrip$VE_REF)))
    eflaloNoTrip <- eflalo[which(!eflalo$ID %in% eflaloTrip$ID),
                           -match("ID", colnames(eflalo))]
    eflaloVessel <- eflaloNoTrip[which(paste(eflaloNoTrip$VE_REF,
                                             format(eflaloNoTrip$LE_CDATIM, "%Y")) %in%
                                       unique(paste(tacsatTrip$VE_REF,
                                                    format(tacsatTrip$SI_DATIM, "%Y")))), ]
    eflaloNoVessel <- eflaloNoTrip[which(!paste(eflaloNoTrip$VE_REF,
                                                format(eflaloNoTrip$LE_CDATIM, "%Y")) %in%
                                         unique(paste(tacsatTrip$VE_REF,
                                                      format(tacsatTrip$SI_DATIM, "%Y")))), ]
    if (dim(tacsatTrip)[1] > 0 & dim(eflaloTrip)[1] > 0)
    {
        if ("day" %in% level)
        {
            print("level: day")
            if (!"SI_YEAR" %in% colnames(tacsatTrip))
                tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM,
                                                format = "%Y"))
            if (!"SI_DAY" %in% colnames(tacsatTrip))
                tacsatTrip$SI_DAY <- an(format(tacsatTrip$SI_DATIM,
                                               format = "%j"))
            if (!"LE_RECT" %in% colnames(tacsatTrip))
                tacsatTrip$LE_RECT <- ICESrectangle(tacsatTrip)
            if (!"SI_YEAR" %in% colnames(eflaloTrip))
                eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM,
                                                format = "%Y"))
            if (!"SI_DAY" %in% colnames(eflaloTrip))
                eflaloTrip$SI_DAY <- an(format(eflaloTrip$LE_CDATIM,
                                               format = "%j"))
            nPings <- countPings(~year + VE_REF + FT_REF + icesrectangle +
                                     day, tacsatTrip, by = by)
            res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings,
                                c("SI_YEAR", "VE_REF", "FT_REF", "LE_RECT", "SI_DAY"),
                                eflaloCol[c(kgs, eur)], remainTacsat, by = by)
            eflaloTrip <- res[["eflalo"]]
            byDayTacsat <- res[["tacsat"]]
            remainTacsat <- res[["remainTacsat"]]

            ## If mismatch of rectangles on the day, priority can be given to allocation to pings
            ##   of the same day:
            if (isTRUE(priorityDay))
            {
                print("level: day (excl. rectangle)")

                nPings <- countPings(~year + VE_REF + FT_REF + day,
                                     tacsatTrip, by = by)
                res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings,
                                    c("SI_YEAR", "VE_REF", "FT_REF", "SI_DAY"),
                                    eflaloCol[c(kgs, eur)], remainTacsat, by = by)
                eflaloTripTmp <- res[["eflalo"]]
                byDayOnlyTacsat <- res[["tacsat"]]
                remainTacsat <- res[["remainTacsat"]]

                ## Needs to recover the "ICESrect" info from the previous eflaloTrip:
                eflaloTripMerged <- merge(eflaloTrip, eflaloTripTmp,
                                          by = c("SI_YEAR", "VE_REF", "FT_REF", "SI_DAY"),
                                          suffixes = c("", ".b"))

                ## Detection of combinations (days) which have been allocated already:
                idxAttr <- apply(eflaloTripMerged[ , paste0(eflaloCol[c(kgs, eur)], ".b")],
                                 1, sum, na.rm = TRUE) < sqrt(.Machine$double.eps)

                ## ... and setting them to zero quantity left:
                eflaloTripMerged[idxAttr, eflaloCol[c(kgs, eur)]] <- 0
                eflaloTrip <- eflaloTripMerged[ , (! colnames(eflaloTripMerged) %in%
                                                   paste0(eflaloCol[c(kgs, eur)], ".b"))]
            }else{
                byDayOnlyTacsat <- NULL
            }
        }
        if ("ICESrectangle" %in% level)
        {
            print("level: rectangle")
            if (!"SI_YEAR" %in% colnames(tacsatTrip))
                tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM,
                                                format = "%Y"))
            if (!"LE_RECT" %in% colnames(tacsatTrip))
                tacsatTrip$LE_RECT <- ICESrectangle(tacsatTrip)
            if (!"SI_YEAR" %in% colnames(eflaloTrip))
                eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM,
                                                format = "%Y"))
            nPings <- countPings(~year + VE_REF + FT_REF + icesrectangle,
                                 tacsatTrip, by = by)
            res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings,
                                c("SI_YEAR", "VE_REF", "FT_REF", "LE_RECT"),
                                eflaloCol[c(kgs, eur)], remainTacsat, by = by)
            eflaloTrip <- res[["eflalo"]]
            byRectTacsat <- res[["tacsat"]]
            remainTacsat <- res[["remainTacsat"]]
        }
        if ("trip" %in% level)
        {
            print("level: trip")
            if (!"SI_YEAR" %in% colnames(tacsatTrip))
                tacsatTrip$SI_YEAR <- an(format(tacsatTrip$SI_DATIM,
                                                format = "%Y"))
            if (!"SI_YEAR" %in% colnames(eflaloTrip))
                eflaloTrip$SI_YEAR <- an(format(eflaloTrip$LE_CDATIM,
                                                format = "%Y"))
            nPings <- countPings(~year + VE_REF + FT_REF, tacsatTrip,
                                 by = by)
            res <- eflalo2Pings(eflaloTrip, tacsatTrip, nPings,
                                c("SI_YEAR", "VE_REF", "FT_REF"),
                                eflaloCol[c(kgs, eur)], remainTacsat, by = by)
            eflaloTrip <- res[["eflalo"]]
            byTripTacsat <- res[["tacsat"]]
            remainTacsat <- res[["remainTacsat"]]
        }
        if (length(remainTacsat) > 0)
            warning("Not all tacsat records with tripnumber have been merged!!")
        if (nrow(eflaloTrip) > 0)
            warning("Not all eflalo records with matching VMS tripnumber have been merged!!")
        if ("day" %in% level)
        {
            tacsatFTREF <- rbind(byDayTacsat, byDayOnlyTacsat, byRectTacsat, byTripTacsat)
        }else{
            if ("ICESrectangle" %in% level)
            {
                tacsatFTREF <- rbind(byRectTacsat, byTripTacsat)
            }else{
                tacsatFTREF <- byTripTacsat
            }
        }
        tacsatFTREF[, kgeur(colnames(tacsatFTREF))] <-
            sweep(tacsatFTREF[, kgeur(colnames(tacsatFTREF))],
                  1, tacsatFTREF$pings,
                  "/")
        tacsatFTREF$ID <- af(ac(tacsatFTREF$ID.x))
        DT <- data.table::data.table(tacsatFTREF)
        eq1 <- c.listquote(paste("sum(",
                                 colnames(tacsatFTREF[, kgeur(colnames(tacsatFTREF))]),
                                 ",na.rm=TRUE)", sep = ""))
        tacsatFTREF <- DT[, eval(eq1), by = ID.x]
        tacsatFTREF <- data.frame(tacsatFTREF)
        data.table::setnames(tacsatFTREF, colnames(tacsatFTREF),
                             c("ID",
                               colnames(eflaloTrip[, kgeur(colnames(eflaloTrip))])))
    }
    if (isTRUE(conserve))
    {
        if (dim(tacsat)[1] > 0 & dim(eflaloVessel)[1] > 0)
        {
            if ("day" %in% level)
            {
                print("level: day & conserve = T, by vessel")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"SI_DAY" %in% colnames(tacsat))
                    tacsat$SI_DAY <- an(format(tacsat$SI_DATIM,
                                               format = "%j"))
                if (!"LE_RECT" %in% colnames(tacsat))
                    tacsat$LE_RECT <- ICESrectangle(tacsat)
                if (!"SI_YEAR" %in% colnames(eflaloVessel))
                    eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM,
                                                      format = "%Y"))
                if (!"SI_DAY" %in% colnames(eflaloVessel))
                    eflaloVessel$SI_DAY <- an(format(eflaloVessel$LE_CDATIM,
                                                     format = "%j"))
                nPings <- countPings(~year + VE_REF + icesrectangle + day,
                                     tacsat, by = by)
                res <- eflalo2Pings(eflaloVessel, tacsat, nPings,
                                    c("SI_YEAR", "VE_REF", "LE_RECT", "SI_DAY"),
                                    eflaloCol[c(kgs, eur)], NULL, by = by)
                eflaloVessel <- res[["eflalo"]]
                byDayTacsat <- res[["tacsat"]]

                ## If mismatch of rectangles on the day, priority can be given to allocation to pings
                ##   of the same day:
                if (isTRUE(priorityDay))
                {
                    print("level: day (excl. rectangle) & conserve = T, by vessel")

                    nPings <- countPings(~year + VE_REF + day,
                                         tacsat, by = by)
                    res <- eflalo2Pings(eflaloVessel, tacsat, nPings,
                                        c("SI_YEAR", "VE_REF", "SI_DAY"),
                                        eflaloCol[c(kgs, eur)], NULL, by = by)

                    eflaloVesselTmp <- res[["eflalo"]]
                    byDayOnlyTacsat <- res[["tacsat"]]

                    ## Needs to recover the "ICESrect" info from the previous eflaloVessel:
                    eflaloVesselMerged <- merge(eflaloVessel, eflaloVesselTmp,
                                                by = c("SI_YEAR", "VE_REF", "SI_DAY"),
                                                suffixes = c("", ".b"))

                    ## Detection of combinations (days) which have been allocated already:
                    idxAttr <- apply(eflaloVesselMerged[ , paste0(eflaloCol[c(kgs, eur)], ".b")],
                                     1, sum, na.rm = TRUE) < sqrt(.Machine$double.eps)

                    ## ... and setting them to zero quantity left:
                    eflaloVesselMerged[idxAttr, eflaloCol[c(kgs, eur)]] <- 0
                    eflaloVessel <- eflaloVesselMerged[ , (! colnames(eflaloVesselMerged) %in%
                                                           paste0(eflaloCol[c(kgs, eur)], ".b"))]
                }else{
                    byDayOnlyTacsat <- NULL
                }
            }
            if ("ICESrectangle" %in% level)
            {
                print("level: rectangle & conserve = T, by vessel")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"LE_RECT" %in% colnames(tacsat))
                    tacsat$LE_RECT <- ICESrectangle(tacsat)
                if (!"SI_YEAR" %in% colnames(eflaloVessel))
                    eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM,
                                                      format = "%Y"))
                nPings <- countPings(~year + VE_REF + icesrectangle,
                                     tacsat, by = by)
                res <- eflalo2Pings(eflaloVessel, tacsat, nPings,
                                    c("SI_YEAR", "VE_REF", "LE_RECT"),
                                    eflaloCol[c(kgs, eur)], NULL, by = by)
                eflaloVessel <- res[["eflalo"]]
                byRectTacsat <- res[["tacsat"]]
            }
            if (TRUE)
            {
                print("level: year & conserve = T, by vessel")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"SI_YEAR" %in% colnames(eflaloVessel))
                    eflaloVessel$SI_YEAR <- an(format(eflaloVessel$LE_CDATIM,
                                                      format = "%Y"))
                nPings <- countPings(~year + VE_REF, tacsat,
                                     by = by)
                res <- eflalo2Pings(eflaloVessel, tacsat, nPings,
                                    c("SI_YEAR", "VE_REF"), eflaloCol[c(kgs, eur)],
                                    NULL, by = by)
                eflaloVessel <- res[["eflalo"]]
                byVessTacsat <- res[["tacsat"]]
            }
            if ("day" %in% level)
            {
                tacsatVEREF <- rbind(byDayTacsat, byDayOnlyTacsat, byRectTacsat,
                                     byVessTacsat)
            }else{
                if ("ICESrectangle" %in% level)
                {
                    tacsatVEREF <- rbind(byRectTacsat, byVessTacsat)
                }else{
                    tacsatVEREF <- byVessTacsat
                }
            }
            tacsatVEREF[, kgeur(colnames(tacsatVEREF))] <-
                sweep(tacsatVEREF[ , kgeur(colnames(tacsatVEREF))],
                      1, tacsatVEREF$pings,
                      "/")
            tacsatVEREF$ID <- af(ac(tacsatVEREF$ID.x))
            DT <- data.table::data.table(tacsatVEREF)
            eq1 <- c.listquote(paste("sum(",
                                     colnames(tacsatVEREF[, kgeur(colnames(tacsatVEREF))]),
                                     ",na.rm=TRUE)",
                                     sep = ""))
            tacsatVEREF <- DT[, eval(eq1), by = ID.x]
            tacsatVEREF <- data.frame(tacsatVEREF)
            data.table::setnames(tacsatVEREF, colnames(tacsatVEREF),
                                 c("ID",
                                   colnames(eflaloVessel[, kgeur(colnames(eflaloVessel))])))
        }
        if (dim(tacsat)[1] > 0 & dim(eflaloNoVessel)[1] > 0)
        {
            if ("day" %in% level)
            {
                print("level: day & conserve = T, no vessel match")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"SI_DAY" %in% colnames(tacsat))
                    tacsat$SI_DAY <- an(format(tacsat$SI_DATIM,
                                               format = "%j"))
                if (!"LE_RECT" %in% colnames(tacsat))
                    tacsat$LE_RECT <- ICESrectangle(tacsat)
                if (!"SI_YEAR" %in% colnames(eflaloNoVessel))
                    eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM,
                                                        format = "%Y"))
                if (!"SI_DAY" %in% colnames(eflaloNoVessel))
                    eflaloNoVessel$SI_DAY <- an(format(eflaloNoVessel$LE_CDATIM,
                                                       format = "%j"))
                nPings <- countPings(~year + icesrectangle + day,
                                     tacsat, by = by)
                res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings,
                                    c("SI_YEAR", "LE_RECT", "SI_DAY"),
                                    eflaloCol[c(kgs, eur)], NULL, by = by)
                eflaloNoVessel <- res[["eflalo"]]
                byDayTacsat <- res[["tacsat"]]

                ## If mismatch of rectangles on the day, priority can be given to allocation to pings
                ##   of the same day:
                if (FALSE) ##  -- DEACTIVATED WHEN NO VESSEL MATCH -- kept in case of future change of heart!
                    ## (isTRUE(priorityDay))
                {
                    print("level: day (excl. rectangle) & conserve = T, no vessel match")

                    nPings <- countPings(~year + day,
                                         tacsat, by = by)
                    res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings,
                                        c("SI_YEAR", "SI_DAY"),
                                        eflaloCol[c(kgs, eur)], NULL, by = by)

                    eflaloNoVesselTmp <- res[["eflalo"]]
                    byDayOnlyTacsat <- res[["tacsat"]]

                    ## Needs to recover the "ICESrect" info from the previous eflaloNoVessel:
                    eflaloNoVesselMerged <- merge(eflaloNoVessel, eflaloNoVesselTmp,
                                                  by = c("SI_YEAR", "SI_DAY"),
                                                  suffixes = c("", ".b"))

                    ## Detection of combinations (days) which have been allocated already:
                    idxAttr <- apply(eflaloNoVesselMerged[ , paste0(eflaloCol[c(kgs, eur)], ".b")],
                                     1, sum, na.rm = TRUE) < sqrt(.Machine$double.eps)

                    ## ... and setting them to zero quantity left:
                    eflaloNoVesselMerged[idxAttr, eflaloCol[c(kgs, eur)]] <- 0
                    eflaloNoVessel <- eflaloNoVesselMerged[ , (! colnames(eflaloNoVesselMerged) %in%
                                                               paste0(eflaloCol[c(kgs, eur)], ".b"))]
                }else{
                    byDayOnlyTacsat <- NULL
                }
            }
            if ("ICESrectangle" %in% level)
            {
                print("level: rectangle & conserve = T, no vessel match")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"LE_RECT" %in% colnames(tacsat))
                    tacsat$LE_RECT <- ICESrectangle(tacsat)
                if (!"SI_YEAR" %in% colnames(eflaloNoVessel))
                    eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM,
                                                        format = "%Y"))
                nPings <- countPings(~year + icesrectangle, tacsat,
                                     by = by)
                res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings,
                                    c("SI_YEAR", "LE_RECT"), eflaloCol[c(kgs, eur)],
                                    NULL, by = by)
                eflaloNoVessel <- res[["eflalo"]]
                byRectTacsat <- res[["tacsat"]]
            }
            if (TRUE)
            {
                print("level: year & conserve = T, no vessel match")
                if (!"SI_YEAR" %in% colnames(tacsat))
                    tacsat$SI_YEAR <- an(format(tacsat$SI_DATIM,
                                                format = "%Y"))
                if (!"SI_YEAR" %in% colnames(eflaloNoVessel))
                    eflaloNoVessel$SI_YEAR <- an(format(eflaloNoVessel$LE_CDATIM,
                                                        format = "%Y"))
                nPings <- countPings(~year, tacsat, by = by)
                res <- eflalo2Pings(eflaloNoVessel, tacsat, nPings,
                                    c("SI_YEAR"), eflaloCol[c(kgs, eur)], NULL,
                                    by = by)
                eflaloNoVessel <- res[["eflalo"]]
                byVessTacsat <- res[["tacsat"]]
            }
            if ("day" %in% level)
            {
                tacsatREF <- rbind(byDayTacsat, byDayOnlyTacsat, byRectTacsat,
                                   byVessTacsat)
            }else{
                if ("ICESrectangle" %in% level)
                {
                    tacsatREF <- rbind(byRectTacsat, byVessTacsat)
                }else{
                    tacsatREF <- byVessTacsat
                }
            }
            tacsatREF[, kgeur(colnames(tacsatREF))] <-
                sweep(tacsatREF[ , kgeur(colnames(tacsatREF))],
                      1, tacsatREF$pings,
                      "/")
            tacsatREF$ID <- af(ac(tacsatREF$ID.x))
            DT <- data.table::data.table(tacsatREF)
            eq1 <- c.listquote(paste("sum(",
                                     colnames(tacsatREF[, kgeur(colnames(tacsatREF))]),
                                     ",na.rm=TRUE)",
                                     sep = ""))
            tacsatREF <- DT[, eval(eq1), by = ID.x]
            tacsatREF <- data.frame(tacsatREF)
            data.table::setnames(tacsatREF, colnames(tacsatREF),
                                 c("ID",
                                   colnames(eflaloVessel[, kgeur(colnames(eflaloVessel))])))
        }
    }
    if (isTRUE(conserve))
    {
        if (exists("tacsatFTREF"))
        {
            one <- tacsatFTREF
        }else{
            one <- numeric()
        }
        if (exists("tacsatVEREF"))
        {
            two <- tacsatVEREF
        }else{
            two <- numeric()
        }
        if (exists("tacsatREF"))
        {
            three <- tacsatREF
        }else{
            three <- numeric()
        }
        tacsatTot <- rbind(one, two, three)
        DT <- data.table::data.table(tacsatTot)
        eq1 <- c.listquote(paste("sum(",
                                 colnames(tacsatTot[, kgeur(colnames(tacsatTot))]),
                                 ",na.rm=TRUE)", sep = ""))
        tacsatTot <- DT[, eval(eq1), by = ID]
        tacsatTot <- data.frame(tacsatTot)
        data.table::setnames(tacsatTot, colnames(tacsatTot),
                             c("ID", colnames(eflalo[,
                                                     kgeur(colnames(eflalo))])))
        tacsatReturn <- merge(tacsat, tacsatTot, by = "ID", all.x = TRUE)
        if (variable == "value")
            tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2],
                                             grep("EURO", colnames(tacsatReturn)))]
        if (variable == "kgs")
            tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2],
                                             grep("KG", colnames(tacsatReturn)))]
        if (variable == "all")
            tacsatReturn <- tacsatReturn
    }else{
        if (exists("tacsatFTREF") == FALSE)
        {
            stop("You have selected not to conserve catches, but there is no trip identifier in the tacsat file")
        }
        tacsatReturn <- merge(tacsat, tacsatFTREF, by = "ID",
                              all.x = TRUE)
        if (variable == "value")
            tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2],
                                             grep("EURO", colnames(tacsatReturn)))]
        if (variable == "kgs")
            tacsatReturn <- tacsatReturn[, c(1:dim(tacsat)[2],
                                             grep("KG", colnames(tacsatReturn)))]
        if (variable == "all")
            tacsatReturn <- tacsatReturn
    }
    return(orderBy(~ID, data = tacsatReturn)[, -match("ID", colnames(tacsatReturn))])
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 120
### End:
