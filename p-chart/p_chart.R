library(plotly)

p_chart <- function(d, n, funnel = FALSE, plot_title = NULL, x_title = NULL, y_title = NULL, obs_name = NULL,
                   obs_colour = NULL, ucl_name = NULL, ucl_colour = NULL, lcl_name = NULL,
                   lcl_colour = NULL, avg_name = NULL, avg_colour = NULL) {
  
  if(is.null(obs_name)) obs_name <- "Observations"
  if(is.null(obs_colour)) obs_colour <- "#390099"
  if(is.null(ucl_name)) ucl_name <- "UCL"
  if(is.null(ucl_colour)) ucl_colour <- "#ff0054"
  if(is.null(lcl_name)) lcl_name <- "LCL"
  if(is.null(lcl_colour)) lcl_colour <- "#ff0054"
  if(is.null(avg_name)) avg_name <- "Avg."
  if(is.null(avg_colour)) avg_colour <- "#9e0059"
  
  if (funnel) {
    n <- n[order(d)]
    
    d <- d[order(d)]
  }
  
  avg <- sum(n) / sum(d)
  
  p <- (n / d) * 100
  
  if (funnel) {
    
    pad <- (function(x){
      
      r <- range(d)
      
      fivepc <- ceiling((r[2] - r[1]) * 0.05)
      
      lh <- c(r[1] - (fivepc * 3), r[1] - (fivepc * 2), r[1] - fivepc)
      
      rh <- c(r[2] + fivepc, r[2] + (fivepc * 2), r[2] + (fivepc * 3))
      
      return(c(lh, rh))
      
    })(x = d)
    
    na_pad <- rep(NA, 3)
    
    d <- c(pad[1:3], d, pad[4:6])
    
    n <- c(NA, NA, NA, n, NA, NA, NA)
    
    p <- c(NA, NA ,NA, p, NA, NA, NA)
    
  }
  
  vari <- sqrt(avg*(1-avg)/d)
  
  ucl <- (avg+3*vari) * 100
  
  lcl <- (avg-3*vari) * 100
  
  avg <- avg * 100
  
  data <- data.frame(d, p = round(p, 1), ucl = round(ucl, 1), lcl = round(lcl, 1), avg= round(avg, 1))
  
  fig <- plot_ly(data, x = ~d)
  
  fig <- add_trace(fig, y = ~p,
                   name = obs_name,
                   type = "scatter", mode = 'markers',
                   marker = list(color = obs_colour))
  
  fig <- add_trace(fig, y = ~lcl,
                   name = lcl_name,
                   type = "scatter", mode = 'lines',
                   line = list(color = lcl_colour))
  
  fig <- add_trace(fig, y = ~ucl,
                   name = ucl_name,
                   type = "scatter", mode = 'lines',
                   line = list(color = ucl_colour))
  
  fig <- add_trace(fig, y = ~avg,
                   name = avg_name,
                   type = "scatter", mode = 'lines',
                   line = list(color = avg_colour, dash ="dash"))
  
  if(!is.null(plot_title)) fig <- layout(fig, title = plot_title)
  
  if(!is.null(x_title)) fig <- layout(fig, xaxis = list(title = x_title))
  
  if(!is.null(y_title)) fig <- layout(fig, yaxis = list(title = y_title))
  
  return(fig)
}


n_patients = c(105,120,121,136,175,188,195,200,203,220,230,241,245,247,249)
n_events = c(15,16,8,20,13,29,35,22,25,35,28,17,34,30,39)

test <- p_chart(d = n_patients, n = n_events)

test
