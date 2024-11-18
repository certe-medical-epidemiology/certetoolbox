# ===================================================================== #
#  An R package by Certe:                                               #
#  https://github.com/certe-medical-epidemiology                        #
#                                                                       #
#  Licensed as GPL-v2.0.                                                #
#                                                                       #
#  Developed at non-profit organisation Certe Medical Diagnostics &     #
#  Advice, department of Medical Epidemiology.                          #
#                                                                       #
#  This R package is free software; you can freely use and distribute   #
#  it for both personal and commercial purposes under the terms of the  #
#  GNU General Public License version 2.0 (GNU GPL-2), as published by  #
#  the Free Software Foundation.                                        #
#                                                                       #
#  We created this package for both routine data analysis and academic  #
#  research and it was publicly released in the hope that it will be    #
#  useful, but it comes WITHOUT ANY WARRANTY OR LIABILITY.              #
# ===================================================================== #

.onLoad <- function(...) {
  # some microorganisms from GLIMS do not exist in the AMR microorganisms data set, so add them there manually:
  tryCatch({
    add_mo_manually <- which(mo_table_glims$manual_add == TRUE)
    suppressMessages(
      AMR::add_custom_microorganisms(
        data.frame(
          genus = mo_table_glims$genus[add_mo_manually],
          species = mo_table_glims$species[add_mo_manually],
          subspecies = mo_table_glims$subspecies[add_mo_manually]
        )
      )
    )
  }, error = function(e) NULL)
}

.onUnload <- function(...) {
  tryCatch({
    suppressMessages(
      AMR::clear_custom_microorganisms()
    )
  }, error = function(e) NULL)
}
