import time
import numpy as np


def main():
  for x in np.random.normal(0, 1, 10000000):
    print('chart:', x, flush=True)
    time.sleep(0.01)


if __name__ == '__main__':
  main()
