import time
import math

for i in range(5000):
    print(i/200, math.sin(i/200), end='\n', flush=True)
    #time.sleep(0.001)
