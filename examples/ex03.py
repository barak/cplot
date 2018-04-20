import random
import itertools
import time

# give cplot some time to boot up
time.sleep(0.1)

y = 0
for i in itertools.count():
    print('chart:', i, y, flush=True)
    y += random.randint(-1, 1)
    time.sleep(0.0001)

