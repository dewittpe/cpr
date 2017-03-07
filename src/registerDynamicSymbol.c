// RegisteringDynamic Symbols
// This file was added to fix the note pre R 3.4.0
// checking compiled code ... NOTE
// File 'cpr/lis/cpr.so':
//   Found no calls to: 'R_registerRoutines', 'R-useDynamicSymbols'
//
// It is good practice to register native routines and to disable symbol search.
//
// this file was part of the solution here:
// http://stackoverflow.com/questions/42313373/r-cmd-check-note-found-no-calls-to-r-registerroutines-r-usedynamicsymbols
//
// and here:
// https://github.com/RcppCore/Rcpp/issues/636

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void R_init_cpr(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
