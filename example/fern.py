import numpy as np
import pexpect
import time


def f1(v):
  return np.matrix([[0, 0], [0, 0.16]]) * v


def f2(v):
  return np.matrix([[0.85, 0.04], [-0.04, 0.85]]) * v + np.array([[0], [1.6]])


def f3(v):
  return np.matrix([[0.20, -0.26], [0.23, 0.22]]) * v + np.array([[0], [1.6]])


def f4(v):
  return np.matrix([[-0.15, 0.28], [0.26, 0.24]]) * v + np.array([[0], [0.44]])


def f(v):
  return np.random.choice([f1] * 1 +
                          [f2] * 85 +
                          [f3] * 7 +
                          [f4] * 7)(v)


def iterate(fn, x):
  while True:
    yield x
    x = fn(x)


def main():
  exe_fp = ('/home/conor/Projects/final-year-project/cplot/' +
            '.stack-work/install/x86_64-linux-nopie/lts-9.5/8.0.2/bin/cplot')
  proc = pexpect.spawn(exe_fp)
  proc.delaybeforesend = 0.001  # decrease/increase to speed up/slow down

  time.sleep(1)

  initial_point = np.array([[0], [0]])

  for i, point in enumerate(iterate(f, initial_point)):
    proc.sendline(str((point * 70)[0, 0]) + ' ' + str((point * 50)[1, 0]))

    if i >= 10000:
      break

  print('Press enter to exit')
  input()  # pause


if __name__ == '__main__':
  main()
