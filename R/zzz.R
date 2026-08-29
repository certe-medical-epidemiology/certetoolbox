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
  register_glims_mo_hook()
}

.onUnload <- function(...) {
  # remove our pending hook, if AMR was never actually loaded in this session
  setHook(packageEvent("AMR", "onLoad"), NULL, action = "replace")
  if (isNamespaceLoaded("AMR")) {
    tryCatch(suppressMessages(AMR::clear_custom_microorganisms()), error = function(e) NULL)
  }
}

# some microorganisms from GLIMS do not exist in the AMR microorganisms data set, so add them there manually
add_glims_mo <- function(...) {
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

# Loading and assessing the full AMR taxonomy inside add_glims_mo() is too slow to run
# unconditionally on every `library(certetoolbox)` (which most sessions never even use AMR
# in), but the custom MOs must be in place before any AMR lookup (mo_name(), eucast_rules(),
# etc.) runs. So instead of calling add_glims_mo() here, hook it onto AMR's own load event:
# it then runs automatically, exactly once, at the moment AMR actually gets loaded by anyone
# in the session (including an implicit `AMR::foo()` call from another package).
register_glims_mo_hook <- function() {
  if (isNamespaceLoaded("AMR")) {
    # AMR is already loaded (e.g. the user ran `library(AMR)` before `library(certetoolbox)`),
    # so there is no future "onLoad" event left to hook into: add the custom MOs right away
    add_glims_mo()
  } else {
    # defer: this fires the first time AMR's namespace is loaded in this session
    setHook(packageEvent("AMR", "onLoad"), add_glims_mo, action = "replace")
  }
}
