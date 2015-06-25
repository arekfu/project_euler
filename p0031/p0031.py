from collections import Counter

britishDenominations = [ 1, 2, 5, 10, 20, 50, 100, 200 ]

def countSplits(n, dens):
    print('call: {}, {}'.format(n, dens), flush=True)
    remainingDens = [den for den in dens if den<=n]
    if n>0 and len(remainingDens)==0:
        print('return 0', flush=True)
        return 0

    if n==0 or len(remainingDens)==1:
        print('return 1', flush=True)
        return 1

    nSplits = 0
    for den in remainingDens:
        smallerDens = [den2 for den2 in remainingDens if den2<=den]
        nSplits += countSplits(n-den, smallerDens)

    print('return {}'.format(nSplits), flush=True)
    return nSplits

for n in list(range(1,6)) + [200]:
    print('splitting {}'.format(n), flush=True)
    print('result: {}'.format(countSplits(n, britishDenominations), flush=True))
    print()
