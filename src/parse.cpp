#include "cpp11/R.hpp"
#include "cpp11/integers.hpp"
#include "cpp11/list.hpp"
#include "cpp11/sexp.hpp"
#include <memory>

#include "Collector.h"
#include "LocaleInfo.h"
#include "Warnings.h"

[[cpp11::register]] SEXP parse_vector_(
    const cpp11::strings& x,
    const cpp11::list& collectorSpec,
    const cpp11::list& locale_,
    const std::vector<std::string>& na,
    bool trim_ws) {
  Warnings warnings;
  int n = x.size();

  LocaleInfo locale(locale_);

  std::shared_ptr<Collector> col(Collector::create(collectorSpec, &locale));
  col->setWarnings(&warnings);
  col->resize(n);

  for (int i = 0; i < n; ++i) {
    Token t;
    if (x[i] == NA_STRING) {
      t = Token(TOKEN_MISSING, i, -1);
    } else {
      SEXP string = x[i];
      t = Token(CHAR(string), CHAR(string) + Rf_length(string), i, -1, false);
      if (trim_ws) {
        t.trim();
      }
      t.flagNA(na);
    }
    col->setValue(i, t);
  }

  return warnings.addAsAttribute(static_cast<SEXP>(col->vector()));
}
