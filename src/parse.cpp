#include "cpp11/R.hpp"
#include "cpp11/integers.hpp"
#include "cpp11/list.hpp"
#include "cpp11/sexp.hpp"
#include <memory>

#include "Collector.h"
#include "LocaleInfo.h"
#include "Source.h"
#include "Tokenizer.h"
#include "TokenizerLine.h"
#include "Warnings.h"

[[cpp11::register]] cpp11::list guess_header_(
    const cpp11::list& sourceSpec,
    const cpp11::list& tokenizerSpec,
    const cpp11::list& locale_) {
  Warnings warnings;
  LocaleInfo locale(locale_);
  SourcePtr source = Source::create(sourceSpec);
  TokenizerPtr tokenizer = Tokenizer::create(tokenizerSpec);
  tokenizer->tokenize(source->begin(), source->end());
  tokenizer->setWarnings(&warnings);

  CollectorCharacter out(&locale.encoder_);
  out.setWarnings(&warnings);
  Token t = tokenizer->nextToken();
  size_t row_num = t.row();

  size_t max_size = 0;
  size_t capacity = 0;

  for (; t.type() != TOKEN_EOF && t.row() == row_num;
       t = tokenizer->nextToken()) {
    if (t.col() >= max_size) {
      max_size = t.col();
    }

    if (max_size >= capacity) {
      capacity = (max_size + 1) * 2;
      out.resize(capacity);
    }

    if (t.type() == TOKEN_STRING) {
      out.setValue(t.col(), t);
    }
  }

  out.resize(max_size + 1);

  using namespace cpp11::literals;
  return cpp11::writable::list(
      {"header"_nm = out.vector(), "skip"_nm = source->skippedRows() + 1});
}

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
