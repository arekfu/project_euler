isRightTriangle (a,b,c) = a^2+b^2==c^2

genPerims p = [ (a,b,c) | a <- [1..(p-2)], b <- [(a+1)..(p-2)], let c=p-a-b, c>0, c<a+b, c>(abs (a-b)) ]

solutions p = filter isRightTriangle $ genPerims p

nSolutions p = length $ solutions p

answer = maximum $ zip (map nSolutions [3..1000]) [3..1000]

main = do
        print $ answer
        print $ solutions (snd answer)
