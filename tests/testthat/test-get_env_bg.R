context("get_env_bg")


test_that("get_env_bg returns correct format", {

   library(S4DM)
   library(terra)

   # occurrence points
     data("sample_points")
     occurrences <- sample_points

   # environmental data
     env <- rast(system.file('ex/sample_env.tif', package="S4DM"))

   # rescale the environmental data

     env <- scale(env)

  bg_data <- get_env_bg(coords = occurrences,
                        env = env,
                        method = "buffer",
                        width = 100,
                        standardize = FALSE)

  length(bg_data)


  # test below assume a data.frame and will be skipped if one isn't returned
  expect_equal(object = length(bg_data),expected = 4)
  expect_contains(object = names(bg_data),
                  expected = c("env","bg_cells","env_mean","env_sd" ))

})
