import wave
import numpy as np
import time
import sys

spf = wave.open('/home/conor/Dropbox/Personal Music/Voice0032.wav', 'r')
signal = np.fromstring(spf.readframes(-1), 'Int16')

channels = [[] for channel in range(spf.getnchannels())]
for index, datum in enumerate(signal):
    channels[index % len(channels)].append(datum)

fs = spf.getframerate()
Time = np.linspace(0, len(signal) / len(channels) / fs,
                      num=len(signal) / len(channels))

# only look at first channel for now
for t, c in zip(Time, channels[0][:100000]):
    print(t, c, flush=True)
    time.sleep(0.001)
