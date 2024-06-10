#include "cpp11/R.hpp"
#include "cpp11/list.hpp"
#include "cpp11/strings.hpp"

#include "DateTime.h"
#include "DateTimeParser.h"
#include "LocaleInfo.h"
#include "QiParsers.h"
#include "utils.h"

typedef bool (*canParseFun)(const std::string&, LocaleInfo* pLocale);

bool canParse(
              const cpp11::strings& x, const canParseFun& canParseF, LocaleInfo* pLocale, unsigned int guess_max) {
  unsigned int n = 0;
  for (const auto & i : x) {
    n++;
    //Rprintf("%u\n", n);
    //Rprintf(i);
    if (n > guess_max) {
      break;
    }
    if (i == NA_STRING) {
      continue;
    }

    if (i.size() == 0) {
      continue;
    }

    if (!canParseF(std::string(i), pLocale)) {
      return false;
    }
  }
  return true;
}

bool allMissing(const cpp11::strings& x) {
  for (const auto & i : x) {
    if (i != NA_STRING && i.size() > 0) {
      return false;
    }
  }
  return true;
}

bool isLogical(const std::string& x, LocaleInfo* /*unused*/) {
  const char* const str = x.data();
  bool res = isLogical(str, str + x.size());
  return res;
}

bool isNumber(const std::string& x, LocaleInfo* pLocale) {
  // Leading zero not followed by decimal mark
  if (x[0] == '0' && x.size() > 1 && x[1] != pLocale->decimalMark_) {
    return false;
  }

  double res = 0;
  std::string::const_iterator begin = x.begin();

  std::string::const_iterator end = x.end();

  bool ok = parseNumber(
      pLocale->decimalMark_, pLocale->groupingMark_, begin, end, res);
  return ok && begin == x.begin() && end == x.end();
}

bool isInteger(const std::string& x, LocaleInfo* /*unused*/) {
  // Leading zero
  if (x[0] == '0' && x.size() > 1) {
    return false;
  }

  double res = 0;
  std::string::const_iterator begin = x.begin();

  std::string::const_iterator end = x.end();

  return parseInt(begin, end, res) && begin == end;
}

bool isDouble(const std::string& x, LocaleInfo* pLocale) {
  // Leading zero not followed by decimal mark
  if (x[0] == '0' && x.size() > 1 && x[1] != pLocale->decimalMark_) {
    return false;
  }

  double res = 0;
  const char* begin = x.c_str();
  const char* end = begin + x.size();

  return parseDouble(pLocale->decimalMark_, begin, end, res) &&
         end == begin + x.size();
}

bool isTime(const std::string& x, LocaleInfo* pLocale) {
  DateTimeParser parser(pLocale);

  parser.setDate(x.c_str());
  return parser.parseLocaleTime();
}

bool isDate(const std::string& x, LocaleInfo* pLocale) {
  DateTimeParser parser(pLocale);

  parser.setDate(x.c_str());
  return parser.parseLocaleDate();
}

static bool isDateTime(const std::string& x, LocaleInfo* pLocale) {
  DateTimeParser parser(pLocale);

  parser.setDate(x.c_str());
  bool ok = parser.parseISO8601();

  if (!ok) {
    return false;
  }

  if (!parser.compactDate()) {
    return true;
  }

  // Values like 00014567 are unlikely to be dates, so don't guess
  return parser.year() > 999;
}

[[cpp11::register]] std::string collectorGuess(
    const cpp11::strings& input,
    const cpp11::list& locale_,
    bool guessInteger,
    unsigned int guess_max) {
  LocaleInfo locale(static_cast<SEXP>(locale_));

  if (input.size() == 0) {
    return "character";
  }

  if (allMissing(input)) {
    return "logical";
  }

  // Work from strictest to most flexible
  if (canParse(input, isLogical, &locale, guess_max)) {
    return "logical";
  }
  if (guessInteger && canParse(input, isInteger, &locale, guess_max)) {
    return "integer";
  }
  if (canParse(input, isDouble, &locale, guess_max)) {
    return "double";
  }
  if (canParse(input, isNumber, &locale, guess_max)) {
    return "number";
  }
  if (canParse(input, isTime, &locale, guess_max)) {
    return "time";
  }
  if (canParse(input, isDate, &locale, guess_max)) {
    return "date";
  }
  if (canParse(input, isDateTime, &locale, guess_max)) {
    return "datetime";
  }

  // Otherwise can always parse as a character
  return "character";
}
