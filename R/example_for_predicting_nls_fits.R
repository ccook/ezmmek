
dummy_fun <- function(lm_obj, grid) {
  predict(lm_obj, newdata = grid)
}

test_obj %>% mutate(predictions = map2(.x = lm.col, .y = pred_df, .f = dummy_fun))

tst <- new_ezmmek_sat_fit("data/victor_ashe_std_03172020.csv", #standard curve file
                                     "data/victor_ashe_sat_steen_03172020.csv", #saturation curve file
                                     site.name, #Group by site.name and std.type
                                     std.type,
                                     method = "steen") #Steen protocol

# Function to mkae results of predict a data frame
predict_df <- function(lm_obj, pred_grid) {
  pred.vec <- predict(lm_obj, pred_grid)
  pred_df <- data.frame(sub.conc = pred_grid$sub.conc, activity.m = pred.vec)
  pred_df
}

# Quick function to plot the results of the mm fit
plot_preds <- function(df) {
  rand.text <- paste(sample(letters, size = 10, replace = TRUE), sep = "", collapse = "")
  p <- ggplot2::ggplot(df, aes(x=sub.conc, y=activity.m)) +
    ggplot2::geom_line() +
    ggplot2::ggtitle(label = rand.text)
  print(p)
  p
}


result_df <- tst %>%
  mutate(preds = map2(.x = mm.fit.obj, .y = pred_grid, .f = predict_df),
         plots = map(.x = preds, .f = plot_preds))

