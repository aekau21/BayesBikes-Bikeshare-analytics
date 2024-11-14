#Head of ds
head(bikes)


ggplot(data = bikes, aes(x = temp_actual, y = rides, fill = as.factor(year))) +
  geom_col() +
  facet_wrap(~season)

