import time
import numpy as np


time.sleep(0.1)

while True:
    ts = np.linspace(-10, 10, 1000)
    x = lambda t: t - 1.6 * np.cos(24 * t)
    y = lambda t: t - 1.6 * np.sin(25 * t)

    for t in ts:
        print('chart:', x(t), y(t), flush=True)
        time.sleep(0.01)
