import time
import itertools

time.sleep(0.1)

for x in itertools.count():
    print('chart:', x + 1, x + 1, flush=True)
    time.sleep(0.001)

