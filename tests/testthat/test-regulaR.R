library(regulaR)

re = regulaR()

context('regex methods')
test_that('responds to regex methods', {
  expect_equal(class(re), 'regex')
})

context('#start_with')
test_that('matches basic characters', {
  expect_equal(re %>% start_with('f'), '^f')
})

test_that('escapes special characters', {
  expect_equal(re %>% start_with('.'), '^\\.')
})

test_that('matches basic characters', {
  expect_equal(re %>% start_with(3, 'x'), '^x{3}')
})

test_that('matches special identifiers', {
  expect_equal(re %>% start_with(2, digits), '^[0-9]{2}')
})

test_that('raises an error when called twice', {
  expect_error(re %>% start_with('x') %>% start_with('x'))
})

context('#append')
test_that('adds basic characters', {
      expect_equal(re %>% append('x') %>% append('y') %>% append('z'),
        'xyz')
})

test_that('also works as #then', {
      expect_equal(re %>% start_with('x') %>% maybe('y') %>% then('z'),
      '^xy?z')
})

test_that('escapes special characters', {
      expect_equal(re %>% between(c(0L,2L), digits) %>% then('.') %>% end_with('$'),
        structure('[0-9]{0,2}\\.\\$$', ended=TRUE))
})

test_that('raises an error after ending', {
        expect_error(re %>% end_with('x') %>% append('y'))
})

context('#maybe')
test_that('recognizes basic characters', {
    regex = re %>% append('x') %>% maybe('y') %>% append('z')
    expect_equal(regex, 'xy?z')
    expect_true(grepl(regex, 'xyz'))
    expect_true(grepl(regex, 'xz'))
})

context('#not')
test_that('creates a negative lookahead', {
  regex = re %>% append('x') %>% not('y') %>% append('z')
  expect_equal(regex, 'x(?!y)z')
  expect_true(grepl(regex, 'xzabc', perl=TRUE))
  expect_true(grepl(regex, 'xzabc', perl=TRUE))
  expect_false(grepl(regex, 'xyzabc', perl=TRUE))
})

context('#one_of')
test_that('creates an alternation', {
  regex = re %>% append('w') %>% one_of(c('x', 'y')) %>% append('z')
  expect_equal(regex, 'w(?:x|y)z')
  expect_true(grepl(regex, 'wxz'))
  expect_true(grepl(regex, 'wyz'))
  expect_false(grepl(regex, 'waz'))
})

context('#between')
test_that('creates a bounded repetition', {
  expect_equal(re %>% between(c(2L,4L), 'x'),
    'x{2,4}')
})

context('#at_least')
test_that('creates a repetition of n times at least', {
  expect_equal(re %>% at_least(3L, 'x'),
    'x{3,}')
})

context('#at_most')
test_that('creates a repetition of n times at most', {
  expect_equal(re %>% at_most(3L, 'x'),
    'x{,3}')
})

context('zero_or_more')
test_that('recognizes basic characters', {
  expect_equal(re %>% zero_or_more('a') %>% then('b'),
    'a*b')
})

test_that('recognizes special identifiers', {
  expect_equal(re %>% zero_or_more(digits),
    '[0-9]*')
})

context('one_or_more')
test_that('recognizes basic characters', {
  expect_equal(re %>% one_or_more('a') %>% then('b'),
    'a+b')
})

test_that('recognizes special identifiers', {
  expect_equal(re %>% one_or_more(letters),
    '[A-Za-z]+')
})

context('#end_with')
test_that('matches basic characters', {
  expect_equal(re %>% append('x') %>% end_with('y'),
    structure('xy$', ended=TRUE))
})

test_that('escapes special characters', {
  expect_equal(re %>% append('x') %>% end_with('$'),
    structure('x\\$$', ended=TRUE))
})

test_that('raises an error when called twice', {
  expect_error(re %>% end_with('x') %>% end_with('x'))
})

context('#regex')
test_that('returns a well-formed regex', {
  expect_equal(re %>% start_with('w') %>% one_of(c('x', 'y')) %>% end_with('z'),
    structure('^w(?:x|y)z$', ended=TRUE))
})

context('special identifiers')
test_that('recognizes digits', {
      expect_equal(re %>% append(2, digits),
        '[0-9]{2}')
})

test_that('recognizes lowercase characters', {
      expect_equal(re %>% append(3, lowercase),
        '[a-z]{3}')
})

test_that('recognizes uppercase characters', {
      expect_equal(re %>% append(3, uppercase),
        '[A-Z]{3}')
})

test_that('recognizes uppercase characters', {
      expect_equal(re %>% append(3, alphanumeric),
        '[A-Za-z0-9]{3}')
})

test_that('recognizes spaces', {
      expect_equal(re %>% append(4, spaces),
        ' {4}')
})

test_that('recognizes whitespace', {
      expect_equal(re %>% append(4, whitespaces),
        '\\s{4}') # TODO: check posix whitespace
})

test_that('recognizes tabs', {
      expect_equal(re %>% append(1, tabs),
        '\t{1}') # TODO: check posix whitespace
})

context('examples')
regex = re %>% start_with(3, digits) %>%
  then('-') %>%
  then(2L, letters) %>%
  maybe('#') %>%
  one_of(c('a','b')) %>%
  between(c(2L,4L), 'c') %>%
  end_with('$')

expect_equal(regex, structure('^[0-9]{3}-[A-Za-z]{2}#?(?:a|b)c{2,4}\\$$', ended=TRUE))

expect_true(grepl(regex, "123-xy#accc$"))
expect_true(grepl(regex, "999-dfbcc$"))
expect_false(grepl(regex, "000-df#baccccccccc$"))
expect_false(grepl(regex, "444-dd3ac$"))
