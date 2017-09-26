import numpy as np
import pexpect
import time

from tqdm import tqdm


def main():
  exe_filepath = \
      '../.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/bin/cplot'

  proc = pexpect.spawn(exe_filepath)
  proc.delaybeforesend = 0.001  # this is normally 0.05, which is too high

  # allow time for cplot to boot
  time.sleep(1)

  for i in tqdm(np.linspace(-2 * np.pi, 2 * np.pi, 1000)):
    proc.sendline('1:' + str(i * 50) + ' ' + str(np.sin(i) * 50))

  for i in tqdm(np.linspace(-2 * np.pi, 2 * np.pi, 1000)):
    proc.sendline('2:' + str(i * 50) + ' ' + str(np.sin(i) * 50))

  for i in tqdm(np.linspace(-2 * np.pi, 2 * np.pi, 1000)):
    proc.sendline('3:' + str(i * 50) + ' ' + str(np.sin(i) * 50))

  for i in tqdm(np.linspace(-2 * np.pi, 2 * np.pi, 1000)):
    proc.sendline('4:' + str(i * 50) + ' ' + str(np.sin(i) * 50))

  time.sleep(5)


if __name__ == '__main__':
  main()
