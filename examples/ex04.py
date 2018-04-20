import random
import time


x, y = 0, 0
while True:
    print('chart:', x, y, flush=True)
    if random.choice([False,True]):
        x += random.choice([-1, 1])
    else:
        y += random.choice([-1, 1])

    time.sleep(0.001)
