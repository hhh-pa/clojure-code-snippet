"""Scaled-input generators for complexity-slope measurement (T20).

Each generator produces (input_bytes, expected_bytes, fit_n) for
one benchmark's stdin contract at requested scales. Expected
outputs keep every scaled run correctness-checked through the
existing measure_case diff path.
"""

from generators.sort_arrays import SortArrayGenerator

# Benchmark name (label/stem, as in BMF) -> generator with scales.
# Scales are per-pilot: solve time must clear JVM-startup noise
# (see docs/t20/specification.md calibration).
GENERATORS = {
    "sort/insertion_sort/solution": SortArrayGenerator(
        scales=[20000, 40000, 80000, 160000]),
    "sort/quick_sort/solution": SortArrayGenerator(
        scales=[1000000, 2000000, 4000000, 8000000]),
}
