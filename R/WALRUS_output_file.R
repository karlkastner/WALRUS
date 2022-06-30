
##   The Wageningen Lowland Runoff Simulator (WALRUS): 
##   a lumped rainfall-runoff model for catchments with shallow groundwater
##   
##   Copyright (C) 2014 Claudia Brauer
##   
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##   
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Makes output files.
#' @description Creates a data file with all model output.
#' @param o the output of a WALRUS run (result of \code{WALRUS_loop}).
#' @param n the name of the run.
#' @return a data file with all model output
#' @export WALRUS_output_file
#' @examples
#' x=1
#' 
WALRUS_output_file = function(o, n, extra_digits = 0)
{
  
  # expand output data frame
  L    = nrow(o)
  
  # extract dates and write in normal output YYYYmmddHHMMSS
  date = WALRUS_date_conversion(output_date)
    
  # forcing
  step_start = output_date[1:(L-1)]
  step_end   = output_date[2:L    ]
  P          = c(NA,func_P    (step_end) - func_P    (step_start))
  ETpot      = c(NA,func_ETpot(step_end) - func_ETpot(step_start))
  Qobs       = c(NA,func_Qobs (step_end) - func_Qobs (step_start))
  fXG        = c(NA,func_fXG  (step_end) - func_fXG  (step_start))
  fXS        = c(NA,func_fXS  (step_end) - func_fXS  (step_start))
  hSmin      = func_hSmin(output_date)
  dGobs      = func_dGobs (output_date)

  # make data frame with forcing and model output
  o = data.frame(cbind(
      d    = output_date     , 
      date                   , 
      P    = round(P      ,4+extra_digits), 
      ETpot= round(ETpot  ,4+extra_digits), 
      Qobs = round(Qobs   ,4+extra_digits), 
      fXG  = round(fXG    ,4+extra_digits), 
      fXS  = round(fXS    ,4+extra_digits), 
      hSmin= round(hSmin  ,2+extra_digits), 
      dGobs= round(dGobs  ,2+extra_digits),
      ETact= round(o$ETact,4+extra_digits), 
      Q    = round(o$Q    ,4+extra_digits),  
      fGS  = round(o$fGS  ,4+extra_digits), 
      fQS  = round(o$fQS  ,4+extra_digits), 
      dV   = round(o$dV   ,2+extra_digits), 
      dVeq = round(o$dVeq ,2+extra_digits), 
      dG   = round(o$dG   ,2+extra_digits), 
      hQ   = round(o$hQ   ,2+extra_digits), 
      hS   = round(o$hS   ,2+extra_digits), 
      W    = round(o$W    ,4+extra_digits)))
    
  # cut off the warming-up period
  o = o[warming_up_idx:L,] # keep initial values for balance computation, so if no warming up period, it will keep the whole vector
  
  # write tables
  options(scipen=999) # to suppress e-4 notation in data files
  write.table(o, paste("output/output_" , n, ".dat", sep=""), row.names=FALSE)  
  
  return(o)
  
}
