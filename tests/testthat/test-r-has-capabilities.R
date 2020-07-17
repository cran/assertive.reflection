test_that(
  "test.r_has_capabilities.returns_true_if_r_has_capabilities",
  {
   caps <- capabilities()
## does not support those added after 2016
    OK <- c("jpeg", "png", "tiff", "tcltk", "X11", "aqua", "http/ftp",
	    "sockets", "libxml", "fifo", "cledit", "iconv", "NLS",
	    "profmem", "cairo", "ICU", "long.double", "libcurl")
    caps <- capabilities(caps)
    caps <- caps[intersect(names(caps), OK)]
    fns <- paste(
      "r_has", 
      sub("[/.]", "_", tolower(names(caps))), 
      "capability",
      sep = "_"
    )
    for(i in seq_along(caps))
    {
      actual <- get(fns[i], envir = as.environment("package:assertive.reflection"))()
      
      if(caps[i])
      {
        expect_true(actual, info = fns[i])
      } else
      {
        expect_false(actual)
        expect_equal(
          cause(actual), 
          noquote(
            paste(
              "R does not have",
              names(caps)[i],
              "capability."
            )
          )
        )
      }      
    }
  }
)
