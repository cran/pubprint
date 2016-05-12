#############################################################################
# test-style_default.R
# 
# Testing default style functions
#############################################################################

# style.apa.character
#############################################################################

test_that("character takes character vectors",
{
    expect_identical(style.apa.character(list("test")),
                     "test")
    expect_identical(style.apa.character(list(c("test",
                                           "test"))),
                     c("test", "test"))
})

test_that("character takes no empty input",
{
    expect_error(style.apa.character())
})


# style.apa.numeric
#############################################################################

test_that("numeric takes numeric vectors",
{
    expect_identical(style.apa.numeric(12),
                     "12.00")
    #expect_identical(style.apa.numeric(c(12, 13)),
    #                 c("12.00", "13.00"))
})

test_that("numeric takes no empty input",
{
    expect_error(style.apa.numeric())
})
