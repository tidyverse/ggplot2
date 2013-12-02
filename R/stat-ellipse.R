#' Calculate Data Ellipses
#' 
#' @param level The confidence level at which to draw an ellipse (default is 0.95)
#' @param type The type of ellipse. 
#'             The default \code{"t"} assumes a multivariate t-distribution, and 
#'             \code{"norm"} assumes a multivariate normal distribution.
#' @param segments The number of segments to be used in drawing the ellipse.  
#' @param na.rm If \code{FALSE} (the default), removes missing values with
#'    a warning.  If \code{TRUE} silently removes missing values.
#' @inheritParams stat_identity
#' 
#' @details The code for calculating the ellipse is largely borrowed from car::ellipse.
#' 
#' @export
#' @importFrom MASS cov.trob
#' 
#' @examples
#' \donttest{
#' ggplot(faithful, aes(waiting, eruptions))+
#'  geom_point()+
#'  stat_ellipse()
#'  
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'  geom_point()+
#'  stat_ellipse()
#'  
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'  geom_point()+
#'  stat_ellipse(type = "norm", linetype = 2)+
#'  stat_ellipse(type = "t")
#'  
#' ggplot(faithful, aes(waiting, eruptions, color = eruptions > 3))+
#'  stat_ellipse(geom = "polygon")
#' }

stat_ellipse <- function(mapping=NULL, data=NULL, geom="path", position="identity", 
                         type = "t", level = 0.95, segments = 51, na.rm = FALSE, ...) {
  StatEllipse$new(mapping=mapping, data=data, geom=geom, position=position, 
                  type = type, level=level, segments=segments, na.rm = na.rm, ...)
}


StatEllipse <- proto(Stat,
	{
	  objname <- "ellipse"
    
    required_aes <- c("x", "y")
		default_geom <- function(.) GeomPath

		calculate_groups <- function(., data, scales, ...){
			.super$calculate_groups(., data, scales,...)
		}
		calculate <- function(., data, scales, type = "t", level = 0.95, segments = 51, na.rm = FALSE, ...){
		  data <- remove_missing(data, na.rm, vars = c("x","y"), name = "stat_ellipse", 
		                         finite = TRUE)
		  
      dfn <- 2
      dfd <- length(data$x) - 1
      
      if(!type %in% c("t","norm")){
        message("Unrecognized ellipse type")
        ellipse <- rbind(as.numeric(c(NA,NA)))
      } else if (dfd < 3){
        message("Too few points to calculate an ellipse")
      	ellipse <- rbind(as.numeric(c(NA,NA)))
      } else {
        if(type == "t"){
          v <- cov.trob(cbind(data$x, data$y))
        }else if(type == "norm"){
          v <- cov.wt(cbind(data$x, data$y))
        }
        shape <- v$cov
        center <- v$center                  
        radius <- sqrt(dfn * qf(level, dfn, dfd))
        angles <- (0:segments) * 2 * pi/segments
        unit.circle <- cbind(cos(angles), sin(angles))
        ellipse <- t(center + radius * t(unit.circle %*% chol(shape)))
      }
    
      ellipse <- as.data.frame(ellipse)
      colnames(ellipse) <- c("x","y")
      return(ellipse)
		}
	}
)

