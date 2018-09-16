#' Calculate some summary measures
#'
#' @description
#' This function calculates fragmentation measures
#'
#' @param x placeholder
#'
#' @param w placeholder
#'
#' @param h_sed placeholder
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @references
#'
#'
#' @export
get_features <- function(x,w, act_cols,h_sed = 100, h_mvpa = 2020){

        x <- as.vector(unlist(x[act_cols]))
        w <- as.vector(unlist(w[act_cols]))
        ret <- c(get_TAC(x=x,w=w),
                 get_frag(x=x, w=w, h_sed=h_sed),
                 get_states(x=x,w=w, h_sed=h_sed, h_mvpa=h_mvpa))
        names(ret) <- c("TAC","TLAC",
                        "SBout","ABout","TPSA","TPAS",
                        "WT","ST","MVPA")

        ret
        # data.frame(get_TAC(x=x,w=w),
        #            get_frag(x=x, w=w, h_sed=h_sed),
        #            get_states(x=x,w=w, h_sed=h_sed, h_mvpa=h_mvpa))
}











#' Calculate fragmentation measures
#'
#' @description
#' This function calculates fragmentation measures
#'
#' @param x placeholder
#'
#' @param w placeholder
#'
#' @param h_sed placeholder
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @references
#'
#'
#' @export
get_frag <- function(x,w,h_sed = 100){
        x = as.integer(x)
        x[which(is.na(x))] <- 0
        w[which(w == 0)] = NA



        y = accel.bouts(counts = x, thresh.lower = h_sed, bout.length = 1)
        y = y * w
        mat = rle2(y)
        mat = mat[which(!is.na(mat[,1])),,drop=FALSE]
        rest = as.matrix(mat[mat[,1] == 0,,drop=FALSE])[,2]
        act = as.matrix(mat[mat[,1] == 1,,drop=FALSE])[,2]

        l.r = NA
        if(length(rest) == 0){
                mu.r = NA
        } else {
                mu.r = mean(rest,na.rm=TRUE)
                if(mu.r != 0) l.r = 1/mu.r
        }

        l.a = NA
        if(length(act) == 0){
                mu.a = NA
        } else {
                mu.a = mean(act,na.rm=TRUE)
                if(mu.a != 0) l.a = 1/mu.a
        }



        # return(data.frame("SBout" = mu.r, "ABout" = mu.a, "TPSA" = l.r, "TPAS" = l.a))
        c(mu.r,mu.a,l.r,l.a)

}





#' Calculate TAC/LTAC
#'
#' @description
#' This function calculates total activity volume
#'
#' @param x placeholder
#'
#' @param w placeholder
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @references
#'
#'
#' @export
get_TAC  <- function(x,w){
        y <- x * w
        tac <- sum(y,na.rm=TRUE)
        tlac <- sum(log(y+1),na.rm=TRUE)

        # return(data.frame("TAC" = tac, "TLAC" = tlac))
        c(tac,tlac)
}





#' Calculate Total weartime plus sedentary/mvpa time
#'
#' @description
#' This function calculates total activity volume
#'
#' @param x placeholder
#'
#' @param w placeholder
#'
#' @param h_sed placeholder
#'
#' @param h_mvpa placeholder
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @references
#'
#'
#' @export
get_states <- function(x,w,h_sed = 100,h_mvpa = 2020){
        WT <- sum(w,na.rm=TRUE)

        w2 <- w

        x <- as.integer(x)
        x[which(is.na(x))] <- 0

        w2[which(w2 == 0)] <- NA

        y <- accel.bouts(counts = x, thresh.lower = h_sed, bout.length = 1)
        y <- na.omit(y * w2)
        sedtime <- sum(y == 0)

        y <- accel.bouts(counts = x, thresh.lower = h_mvpa, bout.length = 1)
        y <- y * w2
        mvpa <- sum(y,na.rm = TRUE)

        # return(data.frame("WT" = WT, "TST" = sedtime, "MVPA" = mvpa))
        c(WT,sedtime,mvpa)
}










