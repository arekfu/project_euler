phi = 0.5 * (1.0 + sqrt 5.0) :: Double
psi = 1.0 - phi :: Double

number = ceiling $ (999.0 + logBase 10.0 (sqrt 5.0)) / (logBase 10.0 phi)
