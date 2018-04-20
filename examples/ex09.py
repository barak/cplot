import time
import random

# simulate the value of the sum of three dice rolls (needs 16 bins)

def roll(nsides):
    return random.randint(1, nsides)

while True:
    r1, r2, r3 = roll(6), roll(6), roll(6)
    print('SumOfThreeDice:', r1 + r2 + r3, flush=True)
    time.sleep(0.001)
