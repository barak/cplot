#include <iostream>
#include <csignal>
#include <cmath>
#include <chrono>

int main(int argc, char *argv[]) {
  // Ensure SIGPIPE is ignored
  (void) std::signal(SIGPIPE, SIG_IGN);

  std::chrono::high_resolution_clock::time_point start = std::chrono::high_resolution_clock::now();

  unsigned int i = 0;

  do {
    double _i = (double) i++ / 100000;
    std::cout << "chart: "
	      << _i << " "
	      << sin(_i) * cos(2 * _i)
	      << std::endl;
  } while (std::cout);

  std::chrono::high_resolution_clock::time_point end = std::chrono::high_resolution_clock::now();
  auto duration = std::chrono::duration_cast<std::chrono::seconds>(end - start).count();

  std::cerr << "Total number of points processed: " << i << std::endl;
  std::cerr << "Avg. points processed per second: " << (double) i / (double) duration << std::endl;
}
