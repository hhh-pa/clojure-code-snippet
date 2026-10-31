// Benchmark name (label/stem, as in BMF) -> generator with scales.
// Keys and scales are identical to tools/generators/__init__.py.
package generators

// nlognScales is the shared 1M..8M ladder for O(n log n) and linear
// sorts alike; see the Python registry for calibration notes.
var nlognScales = []int64{1000000, 2000000, 4000000, 8000000}

// GENERATORS maps benchmark names to their input generators.
var GENERATORS = map[string]Generator{
	// compiler: files sit directly under src/compiler/, so keys
	// double the stem.
	"compiler/cyk_parser/cyk_parser": &CykParserGenerator{
		scales: []int64{60, 120, 240, 480}},
	"compiler/dfa_minimization/dfa_minimization": &DfaMinimizationGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"compiler/earley_parser/earley_parser": &EarleyParserGenerator{
		scales: []int64{60, 120, 240, 480}},
	"compiler/gvn/gvn": &GvnGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"compiler/lexer_regex_nfa/lexer_regex_nfa": &LexerRegexNfaGenerator{
		scales: []int64{800, 1600, 3200, 6400}},
	"compiler/linear_scan_regalloc/linear_scan_regalloc": &LinearScanRegallocGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"compiler/ll1_parser/ll1_parser": &Ll1ParserGenerator{
		scales: []int64{800000, 1600000, 3200000, 6400000}},
	"compiler/lr_parser/lr_parser": &LrParserGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"compiler/pratt_parser/pratt_parser": &PrattParserGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"compiler/register_allocation/register_allocation": &RegisterAllocationGenerator{
		scales: []int64{20000, 40000, 80000, 160000}},
	"compiler/ssa_dominance/ssa_dominance": &SsaDominanceGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"compiler/subset_construction/subset_construction": &SubsetConstructionGenerator{
		scales: []int64{4000, 8000, 16000, 32000}},
	// sort: insertion sort uses a lower ladder (see Python registry).
	"sort/bucket_sort/solution":   &SortArrayGenerator{scales: nlognScales},
	"sort/counting_sort/solution": &SortArrayGenerator{scales: nlognScales},
	"sort/heap_sort/solution":     &SortArrayGenerator{scales: nlognScales},
	"sort/insertion_sort/solution": &SortArrayGenerator{
		scales: []int64{5000, 10000, 20000, 40000}},
	"sort/intro_sort/solution":      &SortArrayGenerator{scales: nlognScales},
	"sort/merge_sort/solution":      &SortArrayGenerator{scales: nlognScales},
	"sort/quick_sort/solution":      &SortArrayGenerator{scales: nlognScales},
	"sort/radix_sort/solution":      &SortArrayGenerator{scales: nlognScales},
	"sort/shell_sort/solution":      &SortArrayGenerator{scales: nlognScales},
	"sort/tournament_sort/solution": &SortArrayGenerator{scales: nlognScales},
	"sort/quickselect/find_the_median": &QuickselectGenerator{
		scales: nlognScales},
	// computational_geometry + distributed. delaunay/voronoi are
	// NOT covered (see Python registry).
	"computational_geometry/closest_pair/closest_pair": &ClosestPairGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"computational_geometry/convex_hull/chan_hull": &ChanHullGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"computational_geometry/convex_hull/convex_hull": &ConvexHullGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"computational_geometry/spatial/kdtree": &KdtreeGenerator{
		scales: []int64{10000, 20000, 40000, 80000}},
	"computational_geometry/sweep_line/bentley_ottmann": &BentleyOttmannGenerator{
		scales: []int64{125, 250, 500, 1000}},
	"computational_geometry/triangulation/polygon_triangulation": &PolygonTriangulationGenerator{
		scales: []int64{1000, 2000, 4000, 8000}},
	"distributed/byzantine/byzantine_eig": &ByzantineEigGenerator{
		scales: []int64{375000, 750000, 1500000, 3000000}},
	"distributed/clocks/vector_clocks": &VectorClocksGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"distributed/consensus/paxos_consensus": &PaxosGenerator{
		scales: []int64{20000, 40000, 80000, 160000}},
	"distributed/consensus/raft_consensus": &RaftGenerator{
		scales: []int64{20000, 40000, 80000, 160000}},
	"distributed/election/leader_election": &LeaderElectionGenerator{
		scales: []int64{250000, 500000, 1000000, 2000000}},
	"distributed/mst/ghs_mst": &GhsMstGenerator{
		scales: []int64{12500, 25000, 50000, 100000}},
	"distributed/snapshot/chandy_lamport": &ChandyLamportGenerator{
		scales: []int64{100000, 200000, 400000, 800000}},
	// data_structure: n is inserts / node count / op count / case
	// size; dsu fits on pair count, fenwick on 3n.
	"data_structure/avl_tree/self_balancing_tree": &AvlTreeGenerator{
		scales: []int64{37500, 75000, 150000, 300000}},
	"data_structure/bst_lca/binary_search_tree_lowest_common_ancestor": &BstLcaGenerator{
		scales: []int64{187500, 375000, 750000, 1500000}},
	"data_structure/b_tree/geek_for_geeks": &BTreeGenerator{
		scales: []int64{37500, 75000, 150000, 300000}},
	"data_structure/disjoint_set/connected_component": &DisjointSetGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"data_structure/fenwick_tree/direct_connections": &DirectConnectionsGenerator{
		scales: []int64{20000, 40000, 80000, 160000}},
	"data_structure/heap/fibonacci_heap": &FibonacciHeapGenerator{
		scales: []int64{93750, 187500, 375000, 750000}},
	"data_structure/heap/find_the_running_median": &RunningMedianGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"data_structure/segment_tree/range_minimum_query": &RangeMinimumQueryGenerator{
		scales: []int64{50000, 100000, 200000, 400000}},
	"data_structure/segment_tree/segment_tree_lazy": &LazySegmentTreeGenerator{
		scales: []int64{20000, 40000, 80000, 160000}},
	"data_structure/skip_list/skip_list": &SkipListGenerator{
		scales: []int64{5000, 10000, 20000, 40000}},
	"data_structure/splay_tree/splay_tree": &SplayTreeGenerator{
		scales: []int64{12500, 25000, 50000, 100000}},
	"data_structure/treap/treap": &TreapGenerator{
		scales: []int64{15000, 30000, 60000, 120000}},
	// dp: n is amount / string len / cap=items / run len; knapsack
	// cases bundle t=2, max_subarray t=4.
	"dp/coin_change/solution": &CoinChangeGenerator{
		scales: []int64{80000, 160000, 320000, 640000}},
	"dp/edit_distance/solution": &EditDistanceGenerator{
		scales: []int64{750, 1500, 3000, 6000}},
	"dp/knapsack/solution": &Knapsack01Generator{
		scales: []int64{1250, 2500, 5000, 10000}},
	"dp/knapsack/unbounded_knapsack": &UnboundedKnapsackGenerator{
		scales: []int64{1250, 2500, 5000, 10000}},
	"dp/lcs/solution": &LcsGenerator{
		scales: []int64{1000, 2000, 4000, 8000}},
	"dp/lis/longest_increasing_subsequent": &LisGenerator{
		scales: []int64{500000, 1000000, 2000000, 4000000}},
	"dp/max_subarray/maxsubarray": &MaxSubarrayGenerator{
		scales: []int64{1000000, 2000000, 4000000, 8000000}},
	// math: crt fits on k (512-bit moduli); miller_rabin scales
	// are bit-widths, fit on max N (ratio 2^24).
	"math/number_theory/crt": &CrtGenerator{
		scales: []int64{250, 500, 1000, 2000}},
	"math/number_theory/miller_rabin_pollard": &MillerRabinPollardGenerator{
		scales: []int64{54, 62, 70, 78}},
	// graph: construction-known answers throughout.
	// kosaraju/tarjan scales are pre-rounded to 3-cycle cover
	// sizes; making_make is O(V^2) via `last`, hence 20k top.
	"graph/all_pairs_shortest_path/floyd_city_of_blinding_lights": &FloydGenerator{
		scales: []int64{50, 100, 200, 400}},
	"graph/all_pairs_shortest_path/johnson": &JohnsonGenerator{
		scales: []int64{400, 800, 1600, 3200}},
	"graph/bridges/even_tree": &EvenTreeGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/eulerian/hierholzer": &HierholzerGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/flow/dinic": &DinicGenerator{
		scales: []int64{80000, 160000, 320000, 640000}},
	"graph/flow/push_relabel": &PushRelabelGenerator{
		scales: []int64{80000, 160000, 320000, 640000}},
	"graph/matching/hopcroft_karp": &HopcroftKarpGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/matching/hungarian": &HungarianGenerator{
		scales: []int64{50, 100, 200, 400}},
	"graph/minimum_spanning_tree/boruvka": &BoruvkaGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/minimum_spanning_tree/kruskalmstrsub": &KruskalMstGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/minimum_spanning_tree/primsmstsub": &PrimsMstGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/shortest_path/bellman_ford": &BellmanFordGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"graph/shortest_path/bfsshortreach": &BfsShortReachGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/shortest_path/dijkstrashortreach": &DijkstraShortReachGenerator{
		scales: []int64{125000, 250000, 500000, 1000000}},
	"graph/strongly_connected_component/kosaraju": &KosarajuGenerator{
		scales: []int64{124998, 249999, 499998, 999999}},
	"graph/strongly_connected_component/tarjan_scc": &TarjanSccGenerator{
		scales: []int64{124998, 249999, 499998, 999999}},
	"graph/topological_sort/making_make": &MakingMakeGenerator{
		scales: []int64{2500, 5000, 10000, 20000}},
	// string_related: construction-known answers. suffix_tree top
	// is 1.25M (not 5M): the 5M scale needs 5.7 GB rss, unsafe
	// under --jobs 2.
	"string_related/aho_corasick/determining_dna_health": &DnaHealthGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"string_related/anagrams/sherlock_and_anagrams": &SherlockAnagramsGenerator{
		scales: []int64{10000, 20000, 40000, 80000}},
	"string_related/boyer_moore/substring_searching": &BoyerMooreSubstringGenerator{
		scales: []int64{10000000, 20000000, 40000000, 80000000}},
	"string_related/kmp/substring_searching": &KmpSubstringGenerator{
		scales: []int64{2000000, 4000000, 8000000, 16000000}},
	"string_related/z_function/substring_searching": &ZFunctionSubstringGenerator{
		scales: []int64{500000, 1000000, 2000000, 4000000}},
	"string_related/common_child/common_child": &CommonChildGenerator{
		scales: []int64{2000, 4000, 8000, 16000}},
	"string_related/hirschberg/hirschberg": &HirschbergGenerator{
		scales: []int64{1000, 2000, 4000, 8000}},
	"string_related/sequence_alignment/sequence_alignment": &SequenceAlignmentGenerator{
		scales: []int64{1000, 2000, 4000, 8000}},
	"string_related/manacher/manacher": &ManacherGenerator{
		scales: []int64{500000, 1000000, 2000000, 4000000}},
	"string_related/suffix_array/string_function_calculation": &SuffixArrayFunctionGenerator{
		scales: []int64{200000, 400000, 800000, 1600000}},
	"string_related/suffix_tree/string_function_calculation": &SuffixTreeFunctionGenerator{
		scales: []int64{156250, 312500, 625000, 1250000}},
	"string_related/suffix_automaton/suffix_automaton": &SuffixAutomatonGenerator{
		scales: []int64{200000, 400000, 800000, 1600000}},
	"string_related/trie/no_prefix_set": &NoPrefixSetGenerator{
		scales: []int64{250000, 500000, 1000000, 2000000}},
	"string_related/ukkonen/ukkonen": &UkkonenGenerator{
		scales: []int64{2500000, 5000000, 10000000, 20000000}},
	// search: astar corridors are (odd) side lengths, fit on path
	// cells; dfs sides fit on cell counts.
	"search/astar/pacman_astar": &PacmanAstarGenerator{
		scales: []int64{251, 501, 1001, 2001}},
	"search/bfs/ctci_bfs_shortest_reach": &BfsShortestReachGenerator{
		scales: []int64{40000, 80000, 160000, 320000}},
	"search/dfs/solution": &DfsRegionGenerator{
		scales: []int64{250, 500, 1000, 2000}},
	"search/two_pointers/pairs": &PairsGenerator{
		scales: []int64{500000, 1000000, 2000000, 4000000}},
}
