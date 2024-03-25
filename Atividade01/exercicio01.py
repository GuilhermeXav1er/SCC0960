def isPrime(n, divisor=2):
    if n <= 1:
        return False
    if divisor * divisor > n:
        return True
    if n % divisor == 0:
        return False
    return isPrime(n, divisor + 1)

def primeNumbers(x, y):
    if x > y:
        return []
    if isPrime(x):
        return [x] + primeNumbers(x + 1, y)
    return primeNumbers(x + 1, y)

def primeIntervals(primeList):
    if len(primeList) < 2:
        return []
    return [primeList[1] - primeList[0]] + primeIntervals(primeList[1:])

def longestInterval(intervals):
    maxInterval = 0
    if len(intervals) == 0:
        return maxInterval
    if len(intervals) == 1:
        return intervals[0]
    maxInterval = max(intervals[0], longestInterval(intervals[1:]))
    return maxInterval


x = int(input())
y = int(input())

primeList = primeNumbers(x, y)

intervals = primeIntervals(primeList)
maxInterval = longestInterval(intervals)

print(maxInterval)
