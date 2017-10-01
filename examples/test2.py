import numpy as np
import pexpect
import time
from scipy.stats import cosine

from tqdm import tqdm


def main():
  exe_filepath = \
      '../.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/bin/cplot'

  proc = pexpect.spawn(exe_filepath)
  proc.delaybeforesend = 0.001  # this is normally 0.05, which is too high

  # allow time for cplot to boot
  time.sleep(1)

  for i in tqdm(np.linspace(0, 10, 1000)):
    proc.sendline(str(i * 150) + ' ' + str((cosine.pdf(i)) * 350))

  time.sleep(5)


if __name__ == '__main__':
  main()
