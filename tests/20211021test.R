ggplot(samp_data, aes(x=X4, y=elpd_15)) + geom_boxplot() + facet_grid(~X2)
ggplot(samp_data, aes(x=X2, y=elpd_15)) + geom_boxplot() + facet_grid(~X4)
ggplot(samp_data, aes(x=X1, y=elpd_15)) + geom_boxplot() + facet_grid(~X3)
ggplot(samp_data, aes(x=X1, y=elpd_15)) + geom_boxplot() + facet_grid(~X4)



