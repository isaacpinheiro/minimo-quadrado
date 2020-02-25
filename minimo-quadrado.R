#!/usr/bin/Rscript

calcularA = function(y, err_y, x) {

  err_y2 = err_y ** 2
  n1_vec = y / err_y2
  n2_vec = x / err_y2
  n3_vec = 1 / err_y2
  n4_vec = (x * y) / err_y2
  n5_vec = x / err_y2
  n6_vec = (x ** 2) / err_y2
  n7_vec = 1 / err_y2

  n1 = 0
  n2 = 0
  n3 = 0
  n4 = 0
  n5 = 0
  n6 = 0
  n7 = 0

  for (i in n1_vec) { n1 = n1 + i }
  for (i in n2_vec) { n2 = n2 + i }
  for (i in n3_vec) { n3 = n3 + i }
  for (i in n4_vec) { n4 = n4 + i }
  for (i in n5_vec) { n5 = n5 + i }
  for (i in n6_vec) { n6 = n6 + i }
  for (i in n7_vec) { n7 = n7 + i }

  a = ((n1 * n2) - (n3 * n4)) / (n5 - (n6 * n7))
  return(a)

}

calcularErroA = function(x, err_y) {

  err_y2 = err_y ** 2
  m1_vec = x / err_y2
  m2_vec = 1 / err_y2
  m3_vec = (x ** 2) / err_y2

  m1 = 0
  m2 = 0
  m3 = 0

  for (i in m1_vec) { m1 = m1 + i }
  for (i in m2_vec) { m2 = m2 + i }
  for (i in m3_vec) { m3 = m3 + i }

  n1 = length(x) / m2
  n2 = m3 / m2
  n3 = m1 / m2

  delta_a = (1 / sqrt(length(x))) * sqrt(n1 / ((n2) - (n3 ** 2)))
  return(delta_a)

}

calcularB = function(y, err_y, x, a) {

  err_y2 = err_y ** 2
  n1_vec = y / err_y2
  n2_vec = x / err_y2
  n3_vec = 1 / err_y2

  n1 = 0
  n2 = 0
  n3 = 0

  for (i in n1_vec) { n1 = n1 + i }
  for (i in n2_vec) { n2 = n2 + i }
  for (i in n3_vec) { n3 = n3 + i }

  b = (n1 - (a * n2)) / n3
  return(b)

}

calcularErroB = function(x, err_y) {

  err_y2 = err_y ** 2
  m1_vec = x / err_y2
  m2_vec = 1 / err_y2
  m3_vec = (x ** 2) / err_y2

  m1 = 0
  m2 = 0
  m3 = 0

  for (i in m1_vec) { m1 = m1 + i }
  for (i in m2_vec) { m2 = m2 + i }
  for (i in m3_vec) { m3 = m3 + i }

  n1 = length(x) / m2
  n2 = m3 / m2
  n3 = m1 / m2

  delta_b = (1 / sqrt(length(x))) * sqrt((n1 * n2) / ((n2) - (n3 ** 2)))
  return(delta_b)

}

calcularMinimoQuadrado = function(y, err_y, x, a, b) {

  m = (y - ((a * x) + b)) / err_y
  m2 = m ** 2
  minimoQuadrado = 0

  for (i in m2) {
    minimoQuadrado = minimoQuadrado + i
  }

  return(minimoQuadrado)

}

x = c(1, 2, 3, 4, 5)
y = c(2, 4, 6, 8, 10)
err_y = c(0.1, 0.1, 0.1, 0.1, 0.1)

a = calcularA(y, err_y, x)
erroA = calcularErroA(x, err_y)
cat('A: ', a, ' +/- ', erroA, '\n')

b = calcularB(y, err_y, x, a)
erroB = calcularErroB(x, err_y)
cat('B: ', b, ' +/- ', erroB, '\n')

minQuadrado = calcularMinimoQuadrado(y, err_y, x, a, b)
cat('Mínimo Quadrado: ', minQuadrado, '\n')

