"""Scaled-input generators for complexity-slope measurement (T20).

Each generator produces (input_bytes, expected_bytes, fit_n) for
one benchmark's stdin contract at requested scales. Expected
outputs keep every scaled run correctness-checked through the
existing measure_case diff path.
"""

from generators.compiler import (CykParserGenerator,
                                 DfaMinimizationGenerator,
                                 EarleyParserGenerator, GvnGenerator,
                                 LexerRegexNfaGenerator,
                                 LinearScanRegallocGenerator,
                                 Ll1ParserGenerator, LrParserGenerator,
                                 PrattParserGenerator,
                                 RegisterAllocationGenerator,
                                 SsaDominanceGenerator,
                                 SubsetConstructionGenerator)
from generators.geo_dist import (
    BentleyOttmannGenerator, ByzantineEigGenerator, ChandyLamportGenerator,
    ChanHullGenerator, ClosestPairGenerator, ConvexHullGenerator,
    GhsMstGenerator, KdtreeGenerator, LeaderElectionGenerator,
    PaxosGenerator, PolygonTriangulationGenerator, RaftGenerator,
    VectorClocksGenerator)
from generators.data_structure import (
    AvlTreeGenerator, BstLcaGenerator, BTreeGenerator,
    DirectConnectionsGenerator, DisjointSetGenerator,
    FibonacciHeapGenerator, LazySegmentTreeGenerator,
    RangeMinimumQueryGenerator, RunningMedianGenerator,
    SkipListGenerator, SplayTreeGenerator, TreapGenerator)
from generators.dp_math import (CrtGenerator, CoinChangeGenerator,
                                EditDistanceGenerator, Knapsack01Generator,
                                LcsGenerator, LisGenerator,
                                MaxSubarrayGenerator,
                                MillerRabinPollardGenerator,
                                UnboundedKnapsackGenerator)
from generators.graph import (BellmanFordGenerator, BfsShortReachGenerator,
                              BoruvkaGenerator, DijkstraShortReachGenerator,
                              DinicGenerator, EvenTreeGenerator,
                              FloydGenerator, HierholzerGenerator,
                              HopcroftKarpGenerator, HungarianGenerator,
                              JohnsonGenerator, KosarajuGenerator,
                              KruskalMstGenerator, MakingMakeGenerator,
                              PrimsMstGenerator, PushRelabelGenerator,
                              TarjanSccGenerator)
from generators.search import (BfsShortestReachGenerator, DfsRegionGenerator,
                               PacmanAstarGenerator, PairsGenerator)
from generators.string_related import (
    BoyerMooreSubstringGenerator, CommonChildGenerator, DnaHealthGenerator,
    HirschbergGenerator, KmpSubstringGenerator, ManacherGenerator,
    NoPrefixSetGenerator, SequenceAlignmentGenerator,
    SherlockAnagramsGenerator, SuffixArrayFunctionGenerator,
    SuffixAutomatonGenerator, SuffixTreeFunctionGenerator,
    UkkonenGenerator, ZFunctionSubstringGenerator)
from generators.sort_arrays import QuickselectGenerator, SortArrayGenerator

# Benchmark name (label/stem, as in BMF) -> generator with scales.
# Scales are per-pilot: solve time must clear JVM-startup noise
# (see docs/t20/specification.md calibration).
#
# O(n log n) sorts share the 1M..8M ladder; linear sorts use the
# same ladder (solve stays well under the timeout); the quadratic
# insertion sort uses a lower ladder (see comment below).
_NLOGN_SCALES = [1000000, 2000000, 4000000, 8000000]
GENERATORS = {
    # compiler: files sit directly under src/compiler/, so BMF
    # keys double the stem (see compiler.py).
    "compiler/cyk_parser/cyk_parser": CykParserGenerator(
        scales=[60, 120, 240, 480]),
    "compiler/dfa_minimization/dfa_minimization":
        DfaMinimizationGenerator(scales=[2000, 4000, 8000, 16000]),
    "compiler/earley_parser/earley_parser": EarleyParserGenerator(
        scales=[60, 120, 240, 480]),
    "compiler/gvn/gvn": GvnGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "compiler/lexer_regex_nfa/lexer_regex_nfa":
        LexerRegexNfaGenerator(scales=[800, 1600, 3200, 6400]),
    "compiler/linear_scan_regalloc/linear_scan_regalloc":
        LinearScanRegallocGenerator(scales=[2000, 4000, 8000, 16000]),
    "compiler/ll1_parser/ll1_parser": Ll1ParserGenerator(
        scales=[800000, 1600000, 3200000, 6400000]),
    "compiler/lr_parser/lr_parser": LrParserGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "compiler/pratt_parser/pratt_parser": PrattParserGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "compiler/register_allocation/register_allocation":
        RegisterAllocationGenerator(scales=[20000, 40000, 80000, 160000]),
    "compiler/ssa_dominance/ssa_dominance": SsaDominanceGenerator(
        scales=[2000, 4000, 8000, 16000]),
    "compiler/subset_construction/subset_construction":
        SubsetConstructionGenerator(scales=[4000, 8000, 16000, 32000]),
    "sort/bucket_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/counting_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/heap_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/insertion_sort/solution": SortArrayGenerator(
        # Pure-Clojure transient-vector insertion (no Java
        # interop) is ~20x slower per op than the pre-PR-29
        # version the old ladder was calibrated on, and scales
        # ~n^2.3 here (5x per doubling: 20k->8s, 40k->33s,
        # 80k->167s solo wall); 120k exceeds the 300 s timeout.
        # 5k..40k keeps an 8x range with the top near ~31 s.
        scales=[5000, 10000, 20000, 40000]),
    "sort/intro_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/merge_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/quick_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/radix_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/shell_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/tournament_sort/solution": SortArrayGenerator(
        scales=_NLOGN_SCALES),
    "sort/quickselect/find_the_median": QuickselectGenerator(
        scales=_NLOGN_SCALES),
    # computational_geometry + distributed (see geo_dist.py).
    # delaunay/voronoi are NOT covered: the JVM solves in <1s at
    # every scale where the O(n^4) reference is feasible, so no
    # ladder can feed a fit (verified flat 3.0s -> 3.3s, n=10
    # to 200, while the reference already needs 21s at n=200).
    "computational_geometry/closest_pair/closest_pair":
        ClosestPairGenerator(scales=[50000, 100000, 200000, 400000]),
    "computational_geometry/convex_hull/chan_hull": ChanHullGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "computational_geometry/convex_hull/convex_hull": ConvexHullGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "computational_geometry/spatial/kdtree": KdtreeGenerator(
        scales=[10000, 20000, 40000, 80000]),
    "computational_geometry/sweep_line/bentley_ottmann":
        BentleyOttmannGenerator(scales=[125, 250, 500, 1000]),
    "computational_geometry/triangulation/polygon_triangulation":
        PolygonTriangulationGenerator(scales=[1000, 2000, 4000, 8000]),
    "distributed/byzantine/byzantine_eig": ByzantineEigGenerator(
        scales=[375000, 750000, 1500000, 3000000]),
    "distributed/clocks/vector_clocks": VectorClocksGenerator(
        scales=[50000, 100000, 200000, 400000]),
    "distributed/consensus/paxos_consensus": PaxosGenerator(
        scales=[20000, 40000, 80000, 160000]),
    "distributed/consensus/raft_consensus": RaftGenerator(
        scales=[20000, 40000, 80000, 160000]),
    "distributed/election/leader_election": LeaderElectionGenerator(
        scales=[250000, 500000, 1000000, 2000000]),
    "distributed/mst/ghs_mst": GhsMstGenerator(
        scales=[12500, 25000, 50000, 100000]),
    "distributed/snapshot/chandy_lamport": ChandyLamportGenerator(
        scales=[100000, 200000, 400000, 800000]),
    # data_structure: n is inserts / node count / op count /
    # case size; dsu fits on pair count, fenwick on 3n.
    "data_structure/avl_tree/self_balancing_tree": AvlTreeGenerator(
        scales=[37500, 75000, 150000, 300000]),
    "data_structure/bst_lca/binary_search_tree_lowest_common_ancestor":
        BstLcaGenerator(scales=[187500, 375000, 750000, 1500000]),
    "data_structure/b_tree/geek_for_geeks": BTreeGenerator(
        scales=[37500, 75000, 150000, 300000]),
    "data_structure/disjoint_set/connected_component":
        DisjointSetGenerator(scales=[50000, 100000, 200000, 400000]),
    "data_structure/fenwick_tree/direct_connections":
        DirectConnectionsGenerator(scales=[20000, 40000, 80000, 160000]),
    "data_structure/heap/fibonacci_heap": FibonacciHeapGenerator(
        scales=[93750, 187500, 375000, 750000]),
    "data_structure/heap/find_the_running_median": RunningMedianGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "data_structure/segment_tree/range_minimum_query":
        RangeMinimumQueryGenerator(scales=[50000, 100000, 200000, 400000]),
    "data_structure/segment_tree/segment_tree_lazy":
        LazySegmentTreeGenerator(scales=[20000, 40000, 80000, 160000]),
    "data_structure/skip_list/skip_list": SkipListGenerator(
        scales=[5000, 10000, 20000, 40000]),
    "data_structure/splay_tree/splay_tree": SplayTreeGenerator(
        scales=[12500, 25000, 50000, 100000]),
    "data_structure/treap/treap": TreapGenerator(
        scales=[15000, 30000, 60000, 120000]),
    # dp: n is amount / string len / cap=items / run len; knapsack
    # cases bundle t=2, max_subarray t=4 (see dp_math.py).
    "dp/coin_change/solution": CoinChangeGenerator(
        scales=[80000, 160000, 320000, 640000]),
    "dp/edit_distance/solution": EditDistanceGenerator(
        scales=[750, 1500, 3000, 6000]),
    "dp/knapsack/solution": Knapsack01Generator(
        scales=[1250, 2500, 5000, 10000]),
    "dp/knapsack/unbounded_knapsack": UnboundedKnapsackGenerator(
        scales=[1250, 2500, 5000, 10000]),
    "dp/lcs/solution": LcsGenerator(
        scales=[1000, 2000, 4000, 8000]),
    "dp/lis/longest_increasing_subsequent": LisGenerator(
        scales=[500000, 1000000, 2000000, 4000000]),
    "dp/max_subarray/maxsubarray": MaxSubarrayGenerator(
        scales=[1000000, 2000000, 4000000, 8000000]),
    # math: crt fits on k (512-bit moduli); miller_rabin scales
    # are bit-widths, fit on max N (ratio 2^24).
    "math/number_theory/crt": CrtGenerator(
        scales=[250, 500, 1000, 2000]),
    "math/number_theory/miller_rabin_pollard": MillerRabinPollardGenerator(
        scales=[54, 62, 70, 78]),
    # graph: construction-known answers throughout (see graph.py).
    # kosaraju/tarjan scales are pre-rounded to 3-cycle cover
    # sizes; making_make is O(V^2) via `last`, hence 20k top.
    "graph/all_pairs_shortest_path/floyd_city_of_blinding_lights":
        FloydGenerator(scales=[50, 100, 200, 400]),
    "graph/all_pairs_shortest_path/johnson": JohnsonGenerator(
        scales=[400, 800, 1600, 3200]),
    "graph/bridges/even_tree": EvenTreeGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/eulerian/hierholzer": HierholzerGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/flow/dinic": DinicGenerator(
        scales=[80000, 160000, 320000, 640000]),
    "graph/flow/push_relabel": PushRelabelGenerator(
        scales=[80000, 160000, 320000, 640000]),
    "graph/matching/hopcroft_karp": HopcroftKarpGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/matching/hungarian": HungarianGenerator(
        scales=[50, 100, 200, 400]),
    "graph/minimum_spanning_tree/boruvka": BoruvkaGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/minimum_spanning_tree/kruskalmstrsub": KruskalMstGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/minimum_spanning_tree/primsmstsub": PrimsMstGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/shortest_path/bellman_ford": BellmanFordGenerator(
        scales=[2000, 4000, 8000, 16000]),
    "graph/shortest_path/bfsshortreach": BfsShortReachGenerator(
        scales=[125000, 250000, 500000, 1000000]),
    "graph/shortest_path/dijkstrashortreach":
        DijkstraShortReachGenerator(
            scales=[125000, 250000, 500000, 1000000]),
    "graph/strongly_connected_component/kosaraju": KosarajuGenerator(
        scales=[124998, 249999, 499998, 999999]),
    "graph/strongly_connected_component/tarjan_scc": TarjanSccGenerator(
        scales=[124998, 249999, 499998, 999999]),
    "graph/topological_sort/making_make": MakingMakeGenerator(
        scales=[2500, 5000, 10000, 20000]),
    # string_related: construction-known answers (see
    # string_related.py). suffix_tree top is 1.25M (not 5M):
    # the 5M scale needs 5.7 GB rss, unsafe under --jobs 2.
    "string_related/aho_corasick/determining_dna_health":
        DnaHealthGenerator(scales=[2000, 4000, 8000, 16000]),
    "string_related/anagrams/sherlock_and_anagrams":
        SherlockAnagramsGenerator(scales=[10000, 20000, 40000, 80000]),
    "string_related/boyer_moore/substring_searching":
        BoyerMooreSubstringGenerator(
            scales=[10000000, 20000000, 40000000, 80000000]),
    "string_related/kmp/substring_searching": KmpSubstringGenerator(
        scales=[2000000, 4000000, 8000000, 16000000]),
    "string_related/z_function/substring_searching":
        ZFunctionSubstringGenerator(
            scales=[500000, 1000000, 2000000, 4000000]),
    "string_related/common_child/common_child": CommonChildGenerator(
        scales=[2000, 4000, 8000, 16000]),
    "string_related/hirschberg/hirschberg": HirschbergGenerator(
        scales=[1000, 2000, 4000, 8000]),
    "string_related/sequence_alignment/sequence_alignment":
        SequenceAlignmentGenerator(scales=[1000, 2000, 4000, 8000]),
    "string_related/manacher/manacher": ManacherGenerator(
        scales=[500000, 1000000, 2000000, 4000000]),
    "string_related/suffix_array/string_function_calculation":
        SuffixArrayFunctionGenerator(
            scales=[200000, 400000, 800000, 1600000]),
    "string_related/suffix_tree/string_function_calculation":
        SuffixTreeFunctionGenerator(
            scales=[156250, 312500, 625000, 1250000]),
    "string_related/suffix_automaton/suffix_automaton":
        SuffixAutomatonGenerator(
            scales=[200000, 400000, 800000, 1600000]),
    "string_related/trie/no_prefix_set": NoPrefixSetGenerator(
        scales=[250000, 500000, 1000000, 2000000]),
    "string_related/ukkonen/ukkonen": UkkonenGenerator(
        scales=[2500000, 5000000, 10000000, 20000000]),
    # search: astar corridors are (odd) side lengths, fit on path
    # cells; dfs sides fit on cell counts (see search.py).
    "search/astar/pacman_astar": PacmanAstarGenerator(
        scales=[251, 501, 1001, 2001]),
    "search/bfs/ctci_bfs_shortest_reach": BfsShortestReachGenerator(
        scales=[40000, 80000, 160000, 320000]),
    "search/dfs/solution": DfsRegionGenerator(
        scales=[250, 500, 1000, 2000]),
    "search/two_pointers/pairs": PairsGenerator(
        scales=[500000, 1000000, 2000000, 4000000]),
}
