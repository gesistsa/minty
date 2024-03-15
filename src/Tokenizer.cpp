#include "cpp11/as.hpp"
#include "cpp11/integers.hpp"
#include "cpp11/list.hpp"

#include "Tokenizer.h"
#include "TokenizerLine.h"

TokenizerPtr Tokenizer::create(const cpp11::list& spec) {
  std::string subclass(cpp11::strings(spec.attr("class"))[0]);

  if (subclass == "tokenizer_line") {
    std::vector<std::string> na =
        cpp11::as_cpp<std::vector<std::string>>(spec["na"]);
    bool skipEmptyRows = cpp11::as_cpp<bool>(spec["skip_empty_rows"]);
    return TokenizerPtr(new TokenizerLine(na, skipEmptyRows));
  }

  cpp11::stop("Unknown tokenizer type");
  return TokenizerPtr();
}
