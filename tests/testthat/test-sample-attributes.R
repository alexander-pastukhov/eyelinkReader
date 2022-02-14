test_that("sample attribute logical flags work ", {
  sample_attr_labels <- c('time', 'px', 'py', 'hx', 'hy', 'pa',
                          'gx', 'gy', 'rx', 'ry',
                          'gxvel', 'gyvel', 'hxvel', 'hyvel', 'rxvel', 'ryvel',
                          'fgxvel', 'fgyvel', 'fhxvel', 'fhyvel', 'frxvel', 'fryvel',
                          'hdata', 'flags', 'input', 'buttons', 'htype', 'errors')


  expect_equal(logical_index_for_sample_attributes(TRUE, c('time', 'gx', 'gy')),
               sample_attr_labels %in% c('time', 'gx', 'gy'))

  # all attributes by default (NULL instead of character list)
  expect_equal(logical_index_for_sample_attributes(TRUE, NULL),
               rep(TRUE, length(sample_attr_labels)))
  expect_equal(logical_index_for_sample_attributes(FALSE, NULL),
               rep(FALSE, length(sample_attr_labels)))


  # attributes override the logical flag
  expect_equal(logical_index_for_sample_attributes(FALSE, c('time', 'gx', 'gy')),
               sample_attr_labels %in% c('time', 'gx', 'gy'))
  # wrong attribute name time2 instead of time
  expect_error(logical_index_for_sample_attributes(TRUE, c('time2', 'gx', 'gy')))
  expect_error(logical_index_for_sample_attributes(TRUE, NA))
  expect_error(logical_index_for_sample_attributes(TRUE, c(1, 2, 3)))

  # wrong import_samples flag
  expect_error(logical_index_for_sample_attributes("TRUE", c('time2', 'gx', 'gy')))


})
