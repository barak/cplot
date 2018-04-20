# TODO

- [ ] Approximate streaming statistics (no idea which would be useful to show)
- [ ] Histograms! These would require an entirely different data structure to
      represent so I'm expecting a lot more refactoring.
- [ ] Need some way of communicating (at the type level) that parts of the data
      stream cannot be empty. Having redundant checks that we know aren't going
      to happen is extremely annoying.
- [ ] Not having to C-c twice to kill the pipeline.
