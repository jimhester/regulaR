#RegulaR
Regular expressions for humans, a port of Ruby's Regularity library

[![Build Status](https://travis-ci.org/jimhester/regulaR.png?branch=master)](https://travis-ci.org/jimhester/regulaR)

regulaR is a human friendly regular expression builder for R. While regular
expressions are a powerful way to match text, they are sometimes difficult to
document and understand when written.  R also has escaping issues with regular
expression this library hopes to solve.

So instead of writing
```r
regex = "^[0-9]{3}-[A-Za-z]{2}#?(?:a|b)c{2,4}\\$$"
```

You can write
```r

regex = regulaR %>% start_with(3, digits) %>%
  then('-') %>%
  then(2L, letters) %>%
  maybe('#') %>%
  one_of(c('a','b')) %>%
  between(c(2L,4L), 'c') %>%
  end_with('$')
```

### See Also
- [rex](https://github.com/kevinushey/rex) for an alternate (better?) implementation

This package is inspired by @hadley's [tweet](https://twitter.com/hadleywickham/status/514102801081708544)
