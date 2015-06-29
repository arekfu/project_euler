import Utils

nNumbers nDig = 9 * 10^(nDig-1)

lens = map nNumbers [1..]

relativeShifts = zipWith (*) lens [1..]

absoluteShifts = zip [1..] $ scanl (+) 0 relativeShifts

nthDigit n = digit
--nthDigit n = (nDigits, shift, numberShift, nAsDigits, digitShift, digit)
        where (nDigits, shift) = last $ takeWhile (\(a,b) -> b<n) absoluteShifts
              numberShift = ((n - shift - 1) `div` nDigits) + 1
              nAsDigits = numberToDigits $ 10^(nDigits-1) + numberShift - 1
              digitShift = (n - shift - 1) `mod` nDigits
              digit = nAsDigits !! (fromIntegral digitShift)

answer = product digits

digits = map nthDigit [1, 10, 100, 1000, 10000, 100000, 1000000]
