# This file is released under terms of BSD license
# See LICENSE file for more information

# To avoid multiple include of different compiler
if(__claw_base_compiler)
  return()
endif()
set(__claw_base_compiler YES)

# Std option for gfortran
set(FPPFLAGS "-o - -Wp,-w,-P -F -fpp") # for preprocessing only
set(FPP_REDIRECT true)                 # use redirection > to save file
set(CLAW_TEST_FFP_FLAGS "-fpp")        # force preprocessing
set(TEST_BASE_FLAGS "")                # Base flags for test case compilation
set(OPENACC_FLAGS "")                  # flags to compile with OpenACC support
set(OPENMP_FLAGS "-openmp")            # flags to compile with OpenMP support
set(COMPILER_MACRO "-DNAGFOR")         # predefined macro by compiler
