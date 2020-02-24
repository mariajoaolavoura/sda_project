data2 = read.csv("./../data/cleaned-cars.csv")
View(data2)

# TODO:
#   *minimamente simetricos (hist, boxplot)
#   *linearmente relacionados com a resposta (pairs)
#   *multicolinearidade (corr(vars))
#   *outliers
#   *missing values
#   *analise modelo
#   *multicolinearidade (vif(model))
#   *homocedasticidade (plot fitted values vs stard residuals)
#   *pontos influentes (cook dist, leverages(hatvalues) )


pairs(data2[15000, 2:10])

