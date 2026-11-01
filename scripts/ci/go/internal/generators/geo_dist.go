// Slope input generators for computational geometry + distributed
// benchmarks, ported from tools/generators/geo_dist.py. Geometry
// inputs use integer coordinates so printed values are exact, and the
// expected outputs mirror the solutions' renderers bit for bit.
// Distributed sims use unbounded ints with zero float I/O. All
// generation is a pure function of (n, seed).
package generators

import (
	"bytes"
	"math"
	"math/big"
	"sort"
	"strconv"
	"strings"

	mtrand "github.com/hhh-pa/clojure/scripts/ci/go/internal/mtrand"
)

// geoDistEps mirrors geo_dist._EPS.
const geoDistEps = 1e-9

// geoDistFloatBarrier is an opaque float identity. Go's ARM64 backend
// fuses a multiply feeding an add/subtract into a single-rounding FMA,
// but Python (and the reference renderers) round every intermediate
// operation to float64. Wrapping such multiplies keeps each rounding
// point the compiler cannot see through, so results match bit for bit.
// The noinline attribute is essential: inlining would re-expose the
// fusion pattern.
//
//go:noinline
func geoDistFloatBarrier(x float64) float64 { return x }

// geoDistGrow preallocates est bytes, capped so absurd scales cannot
// trigger a huge allocation up front.
func geoDistGrow(buf *bytes.Buffer, est int64) {
	if est <= 0 {
		return
	}
	const maxPrealloc = 64 << 20
	if est > maxPrealloc {
		est = maxPrealloc
	}
	buf.Grow(int(est))
}

// geoDistTok appends one space-separated token to an input block.
func geoDistTok(buf *bytes.Buffer, first *bool, s string) {
	if !*first {
		buf.WriteByte(' ')
	}
	*first = false
	buf.WriteString(s)
}

// geoDistInt appends one space-separated integer token to an input block.
func geoDistInt(buf *bytes.Buffer, first *bool, v int64) {
	geoDistTok(buf, first, strconv.FormatInt(v, 10))
}

// geoDistLine appends one newline-terminated output line.
func geoDistLine(buf *bytes.Buffer, nlines *int, parts ...string) {
	for _, p := range parts {
		buf.WriteString(p)
	}
	buf.WriteByte('\n')
	*nlines++
}

// geoDistEndLines terminates an expected block. Python emits
// "\n".join(out) + "\n", which is a lone "\n" when out is empty.
func geoDistEndLines(buf *bytes.Buffer, nlines int) {
	if nlines == 0 {
		buf.WriteByte('\n')
	}
}

// geoDistNewtonSqrt is Newton's square root mirroring the Clojure
// solutions (30 iterations).
func geoDistNewtonSqrt(x float64) float64 {
	if x == 0.0 {
		return x
	}
	if math.IsNaN(x) {
		return x
	}
	if x == x*2.0 {
		return x
	}
	if x < 0.0 {
		return math.NaN()
	}
	m := x
	adj := 1.0
	for m > 4.0 {
		m /= 4.0
		adj *= 2.0
	}
	for m < 0.25 {
		m *= 4.0
		adj /= 2.0
	}
	g := 1.0
	if m > 1.0 {
		g = m
	}
	for i := 0; i < 30; i++ {
		g = (g + m/g) / 2.0
	}
	return g * adj
}

// geoDistDoubleToBigintStr renders the exact integer value of an
// integral double beyond int64 range (mirrors _double_to_bigint_str).
func geoDistDoubleToBigintStr(v float64) string {
	q := v
	var k uint
	for q >= 9007199254740992.0 {
		q /= 2.0
		k++
	}
	iv := big.NewInt(int64(q))
	iv.Lsh(iv, k)
	return iv.String()
}

// geoDistFormatFixed renders x with exactly k decimals, halves up
// (mirrors the Clojure renderer via _format_fixed).
func geoDistFormatFixed(x float64, k int) string {
	if math.IsNaN(x) {
		return "NaN"
	}
	if x != 0.0 && x == x*2.0 {
		if x < 0.0 {
			return "-Infinity"
		}
		return "Infinity"
	}
	neg := math.Signbit(x)
	a := x
	if neg {
		a = -x
	}
	if a >= 9.0e18 {
		s := geoDistDoubleToBigintStr(a) + "." + strings.Repeat("0", k)
		if neg {
			return "-" + s
		}
		return s
	}
	pow10k := 1.0
	for i := 0; i < k; i++ {
		pow10k *= 10.0
	}
	ip := int64(a)
	frac := a - float64(ip)
	r := int64(geoDistFloatBarrier(frac*pow10k) + 0.5)
	if r >= int64(pow10k) {
		ip++
		r = 0
	}
	fs := strconv.FormatInt(r, 10)
	var b strings.Builder
	if neg {
		b.WriteByte('-')
	}
	b.WriteString(strconv.FormatInt(ip, 10))
	b.WriteByte('.')
	for i := len(fs); i < k; i++ {
		b.WriteByte('0')
	}
	b.WriteString(fs)
	return b.String()
}

// geoDistFormatFixedEven1 renders x with exactly one decimal, ties to
// even (mirrors the Clojure renderer via _format_fixed_even_1).
func geoDistFormatFixedEven1(x float64) string {
	if math.IsNaN(x) {
		return "NaN"
	}
	if x != 0.0 && x == x*2.0 {
		if x < 0.0 {
			return "-Infinity"
		}
		return "Infinity"
	}
	neg := math.Signbit(x)
	a := x
	if neg {
		a = -x
	}
	if a >= 9.0e18 {
		if neg {
			return "-" + geoDistDoubleToBigintStr(a) + ".0"
		}
		return geoDistDoubleToBigintStr(a) + ".0"
	}
	ip := int64(a)
	frac := a - float64(ip)
	scaled := geoDistFloatBarrier(frac * 10.0)
	fl := int64(scaled)
	f := scaled - float64(fl)
	var r int64
	switch {
	case f > 0.5:
		r = fl + 1
	case f < 0.5:
		r = fl
	case fl%2 == 0:
		r = fl
	default:
		r = fl + 1
	}
	if r == 10 {
		ip++
		r = 0
	}
	var b strings.Builder
	if neg {
		b.WriteByte('-')
	}
	b.WriteString(strconv.FormatInt(ip, 10))
	b.WriteByte('.')
	b.WriteString(strconv.FormatInt(r, 10))
	return b.String()
}

// geoDistCross is twice the signed area of triangle o-a-b (exact
// integer math, mirrors _cross).
func geoDistCross(o, a, b [2]int64) int64 {
	return (a[0]-o[0])*(b[1]-o[1]) - (a[1]-o[1])*(b[0]-o[0])
}

// geoDistSortPoints orders points by x then y.
func geoDistSortPoints(pts [][2]int64) {
	sort.Slice(pts, func(i, j int) bool {
		if pts[i][0] != pts[j][0] {
			return pts[i][0] < pts[j][0]
		}
		return pts[i][1] < pts[j][1]
	})
}

// geoDistMonotoneChain is Andrew's chain mirroring graham-scan: CCW
// order with no collinear middle points.
func geoDistMonotoneChain(pts [][2]int64) [][2]int64 {
	seen := make(map[[2]int64]bool, len(pts))
	uniq := make([][2]int64, 0, len(pts))
	for _, p := range pts {
		if !seen[p] {
			seen[p] = true
			uniq = append(uniq, p)
		}
	}
	geoDistSortPoints(uniq)
	if len(uniq) <= 2 {
		return append([][2]int64(nil), uniq...)
	}
	var lower [][2]int64
	for _, p := range uniq {
		for len(lower) >= 2 && geoDistCross(lower[len(lower)-2], lower[len(lower)-1], p) <= 0 {
			lower = lower[:len(lower)-1]
		}
		lower = append(lower, p)
	}
	var upper [][2]int64
	for i := len(uniq) - 1; i >= 0; i-- {
		p := uniq[i]
		for len(upper) >= 2 && geoDistCross(upper[len(upper)-2], upper[len(upper)-1], p) <= 0 {
			upper = upper[:len(upper)-1]
		}
		upper = append(upper, p)
	}
	return append(lower[:len(lower)-1], upper[:len(upper)-1]...)
}

// geoDistHullPerimeter is the cyclic hull perimeter mirroring the
// convex-hull solution.
func geoDistHullPerimeter(hull [][2]int64) float64 {
	acc := 0.0
	m := len(hull)
	for i := 0; i < m; i++ {
		x1, y1 := hull[i][0], hull[i][1]
		x2, y2 := hull[(i+1)%m][0], hull[(i+1)%m][1]
		dx := float64(x1 - x2)
		dy := float64(y1 - y2)
		acc = acc + geoDistNewtonSqrt(geoDistFloatBarrier(dx*dx)+geoDistFloatBarrier(dy*dy))
	}
	return acc
}

// geoDistCircumcircle returns the circumcenter and squared radius of
// triangle a-b-c, or ok=false when |d| <= EPS (mirrors _circumcircle).
func geoDistCircumcircle(a, b, c [2]float64) (ux, uy, rsq float64, ok bool) {
	ax, ay := a[0], a[1]
	bx, by := b[0], b[1]
	cx, cy := c[0], c[1]
	// Every product feeding an add/subtract is barriered so each
	// Python rounding point survives ARM64 FMA fusion.
	d1 := geoDistFloatBarrier(ax * (by - cy))
	d2 := geoDistFloatBarrier(bx * (cy - ay))
	d3 := geoDistFloatBarrier(cx * (ay - by))
	d := 2.0 * (d1 + d2 + d3)
	if math.Abs(d) <= geoDistEps {
		return 0, 0, 0, false
	}
	n1 := geoDistFloatBarrier(ax*ax) + geoDistFloatBarrier(ay*ay)
	n2 := geoDistFloatBarrier(bx*bx) + geoDistFloatBarrier(by*by)
	n3 := geoDistFloatBarrier(cx*cx) + geoDistFloatBarrier(cy*cy)
	u1 := geoDistFloatBarrier(n1 * (by - cy))
	u2 := geoDistFloatBarrier(n2 * (cy - ay))
	u3 := geoDistFloatBarrier(n3 * (ay - by))
	ux = (u1 + u2 + u3) / d
	v1 := geoDistFloatBarrier(n1 * (cx - bx))
	v2 := geoDistFloatBarrier(n2 * (ax - cx))
	v3 := geoDistFloatBarrier(n3 * (bx - ax))
	uy = (v1 + v2 + v3) / d
	rsq = geoDistFloatBarrier((ax-ux)*(ax-ux)) + geoDistFloatBarrier((ay-uy)*(ay-uy))
	return ux, uy, rsq, true
}

// geoDistEmptyCircle reports whether triple i<j<k has an empty
// circumcircle over all points (mirrors _empty_circle).
func geoDistEmptyCircle(raw [][2]float64, i, j, k int) bool {
	ux, uy, rsq, ok := geoDistCircumcircle(raw[i], raw[j], raw[k])
	if !ok {
		return false
	}
	for m := range raw {
		if m == i || m == j || m == k {
			continue
		}
		dx := raw[m][0] - ux
		dy := raw[m][1] - uy
		if geoDistFloatBarrier(dx*dx)+geoDistFloatBarrier(dy*dy) < rsq-geoDistEps {
			return false
		}
	}
	return true
}

// geoDistPointCmp orders points by x then y within tolerance
// (mirrors _point_cmp).
func geoDistPointCmp(p1, p2 [2]float64) int {
	if math.Abs(p1[0]-p2[0]) > geoDistEps {
		if p1[0] < p2[0] {
			return -1
		}
		return 1
	}
	if math.Abs(p1[1]-p2[1]) > geoDistEps {
		if p1[1] < p2[1] {
			return -1
		}
		return 1
	}
	return 0
}

// geoDistNextGreaterDists gives, for each ring position, the circular
// distance to the next strictly greater element, or n when there is
// none (mirrors _next_greater_dists).
func geoDistNextGreaterDists(ring []int64) []int64 {
	n := len(ring)
	d := make([]int64, n)
	for i := range d {
		d[i] = int64(n)
	}
	var stack []int
	for j := 2*n - 1; j >= 0; j-- {
		val := ring[j%n]
		for len(stack) > 0 && ring[stack[len(stack)-1]%n] <= val {
			stack = stack[:len(stack)-1]
		}
		if j < n && len(stack) > 0 {
			d[j] = int64(stack[len(stack)-1] - j)
		}
		stack = append(stack, j)
	}
	return d
}

// geoDistBisectRight mirrors bisect.bisect_right over sorted ints.
func geoDistBisectRight(a []int64, x int64) int {
	lo, hi := 0, len(a)
	for lo < hi {
		mid := (lo + hi) / 2
		if x < a[mid] {
			hi = mid
		} else {
			lo = mid + 1
		}
	}
	return lo
}

// ClosestPairGenerator emits collinear unit-spaced points; the minimum
// distance is exactly 1.0.
type ClosestPairGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *ClosestPairGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits n collinear unit-spaced points; the expected minimum
// distance is 1.0 (1e18 rendered when n < 2).
func (g *ClosestPairGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	_ = seed
	var in bytes.Buffer
	geoDistGrow(&in, n*4+16)
	first := true
	geoDistInt(&in, &first, n)
	for i := int64(0); i < n; i++ {
		geoDistInt(&in, &first, i)
		geoDistInt(&in, &first, 0)
	}
	var exp bytes.Buffer
	nlines := 0
	if n < 2 {
		geoDistLine(&exp, &nlines, "1000000000000000000.000000")
	} else {
		geoDistLine(&exp, &nlines, "1.000000")
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// ChanHullGenerator emits random int64 points; the hull is the monotone
// chain rotated to start at the lexicographically smallest point.
type ChanHullGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *ChanHullGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits n random points in [-1e6, 1e6]^2 and the rotated hull.
func (g *ChanHullGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	pts := make([][2]int64, 0, n)
	for i := int64(0); i < n; i++ {
		x := rng.RandInt(-1000000, 1000000)
		y := rng.RandInt(-1000000, 1000000)
		pts = append(pts, [2]int64{x, y})
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*16+16)
	first := true
	geoDistInt(&in, &first, n)
	for _, p := range pts {
		geoDistInt(&in, &first, p[0])
		geoDistInt(&in, &first, p[1])
	}
	hull := geoDistMonotoneChain(pts)
	if len(hull) > 0 {
		mi := 0
		for i := 1; i < len(hull); i++ {
			if hull[i][0] < hull[mi][0] || (hull[i][0] == hull[mi][0] && hull[i][1] < hull[mi][1]) {
				mi = i
			}
		}
		rot := make([][2]int64, 0, len(hull))
		rot = append(rot, hull[mi:]...)
		rot = append(rot, hull[:mi]...)
		hull = rot
	}
	var exp bytes.Buffer
	geoDistGrow(&exp, int64(len(hull))*16+16)
	nlines := 0
	geoDistLine(&exp, &nlines, strconv.FormatInt(int64(len(hull)), 10))
	for _, p := range hull {
		geoDistLine(&exp, &nlines, strconv.FormatInt(p[0], 10), " ", strconv.FormatInt(p[1], 10))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// ConvexHullGenerator emits an axis-aligned power-of-two rectangle plus
// strict-interior points.
type ConvexHullGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *ConvexHullGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the rectangle corners plus n-4 interior points; the
// expected value is the hull perimeter rendered ties-to-even.
func (g *ConvexHullGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	const w, h = int64(1024), int64(2048)
	corners := [][2]int64{{0, 0}, {w, 0}, {w, h}, {0, h}}
	var pts [][2]int64
	if n <= 4 {
		for i := int64(0); i < n; i++ {
			pts = append(pts, corners[i])
		}
	} else {
		pts = append(pts, corners...)
		for i := int64(4); i < n; i++ {
			x := rng.RandInt(1, w-1)
			y := rng.RandInt(1, h-1)
			pts = append(pts, [2]int64{x, y})
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*12+16)
	first := true
	geoDistInt(&in, &first, n)
	for _, p := range pts {
		geoDistInt(&in, &first, p[0])
		geoDistInt(&in, &first, p[1])
	}
	seen := make(map[[2]int64]bool, len(pts))
	uniq := make([][2]int64, 0, len(pts))
	for _, p := range pts {
		if !seen[p] {
			seen[p] = true
			uniq = append(uniq, p)
		}
	}
	geoDistSortPoints(uniq)
	var exp bytes.Buffer
	nlines := 0
	if len(uniq) < 2 {
		geoDistLine(&exp, &nlines, "0.0")
	} else {
		geoDistLine(&exp, &nlines, geoDistFormatFixedEven1(geoDistHullPerimeter(geoDistMonotoneChain(uniq))))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// KdtreeGenerator emits points (10i, 0); RANGE boxes on the lattice and
// unit-offset nearest queries.
type KdtreeGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *KdtreeGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits n lattice points plus q mixed NEAREST/RANGE queries
// with their exact answers.
func (g *KdtreeGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var q int64
	if n == 0 {
		q = 0
	} else {
		q = n / 10
		if q < 1 {
			q = 1
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*16+q*32+32)
	first := true
	geoDistInt(&in, &first, n)
	for i := int64(0); i < n; i++ {
		geoDistInt(&in, &first, 10*i)
		geoDistInt(&in, &first, 0)
	}
	geoDistInt(&in, &first, q)
	var exp bytes.Buffer
	geoDistGrow(&exp, q*24+8)
	nlines := 0
	for i := int64(0); i < q; i++ {
		if n > 0 && rng.Random() < 0.5 {
			pi := rng.RandInt(0, n-1)
			geoDistTok(&in, &first, "NEAREST")
			geoDistInt(&in, &first, 10*pi+1)
			geoDistInt(&in, &first, 0)
			geoDistLine(&exp, &nlines, strconv.FormatInt(10*pi, 10), ".00 0.00 1.00")
		} else {
			var lo, hi int64
			if n > 0 {
				a := rng.RandInt(0, n-1)
				b := rng.RandInt(0, n-1)
				lo, hi = a, b
				if b < a {
					lo, hi = b, a
				}
			} else {
				lo, hi = 0, -1
			}
			r := rng.Random()
			var y1, y2 int64
			if r < 0.6 {
				y1 = -rng.RandInt(0, 3)
				y2 = rng.RandInt(0, 3)
			} else if r < 0.8 {
				y1 = rng.RandInt(1, 4)
				y2 = y1 + rng.RandInt(0, 4)
			} else {
				y2 = -rng.RandInt(1, 4)
				y1 = y2 - rng.RandInt(0, 4)
			}
			geoDistTok(&in, &first, "RANGE")
			geoDistInt(&in, &first, 10*lo)
			geoDistInt(&in, &first, y1)
			geoDistInt(&in, &first, 10*hi)
			geoDistInt(&in, &first, y2)
			if n > 0 && y1 <= 0 && 0 <= y2 {
				geoDistLine(&exp, &nlines, strconv.FormatInt(hi-lo+1, 10))
			} else {
				geoDistLine(&exp, &nlines, "0")
			}
		}
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// BentleyOttmannGenerator emits an axis-aligned grid: v verticals by h
// horizontals with crossings at integer points.
type BentleyOttmannGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *BentleyOttmannGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the v+h grid segments (or disjoint unit diagonals when
// the grid would be degenerate) and the sorted crossings.
func (g *BentleyOttmannGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	_ = seed
	v := n / 2
	h := n - v
	var in bytes.Buffer
	geoDistGrow(&in, n*32+16)
	first := true
	geoDistInt(&in, &first, n)
	var exp bytes.Buffer
	nlines := 0
	if v >= 2 && h >= 2 {
		geoDistGrow(&exp, v*h*16+16)
		for i := int64(0); i < v; i++ {
			geoDistInt(&in, &first, i)
			geoDistInt(&in, &first, 0)
			geoDistInt(&in, &first, i)
			geoDistInt(&in, &first, h-1)
		}
		for j := int64(0); j < h; j++ {
			geoDistInt(&in, &first, 0)
			geoDistInt(&in, &first, j)
			geoDistInt(&in, &first, v-1)
			geoDistInt(&in, &first, j)
		}
		geoDistLine(&exp, &nlines, strconv.FormatInt(v*h, 10))
		for i := int64(0); i < v; i++ {
			si := strconv.FormatInt(i, 10)
			for j := int64(0); j < h; j++ {
				geoDistLine(&exp, &nlines, si, ".00 ", strconv.FormatInt(j, 10), ".00")
			}
		}
	} else {
		for i := int64(0); i < n; i++ {
			geoDistInt(&in, &first, 10*i)
			geoDistInt(&in, &first, 10*i)
			geoDistInt(&in, &first, 10*i+1)
			geoDistInt(&in, &first, 10*i+1)
		}
		geoDistLine(&exp, &nlines, "0")
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// DelaunayGenerator emits random distinct integer points; the reference
// is the O(n^4) brute-force empty-circle triples.
type DelaunayGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *DelaunayGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits n distinct points in [0, 199]^2 and the sorted
// Delaunay triples.
func (g *DelaunayGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	pts := make([][2]int64, 0, n)
	seen := make(map[[2]int64]bool)
	for int64(len(pts)) < n {
		x := rng.RandInt(0, 199)
		y := rng.RandInt(0, 199)
		p := [2]int64{x, y}
		if !seen[p] {
			seen[p] = true
			pts = append(pts, p)
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*12+16)
	first := true
	geoDistInt(&in, &first, n)
	for _, p := range pts {
		geoDistInt(&in, &first, p[0])
		geoDistInt(&in, &first, p[1])
	}
	raw := make([][2]float64, n)
	for i, p := range pts {
		raw[i] = [2]float64{float64(p[0]), float64(p[1])}
	}
	type tri struct{ i, j, k int64 }
	var tris []tri
	for i := int64(0); i < n; i++ {
		for j := i + 1; j < n; j++ {
			for k := j + 1; k < n; k++ {
				if geoDistEmptyCircle(raw, int(i), int(j), int(k)) {
					tris = append(tris, tri{i, j, k})
				}
			}
		}
	}
	// Generation order is already lexicographic, matching tris.sort().
	var exp bytes.Buffer
	geoDistGrow(&exp, int64(len(tris))*16+16)
	nlines := 0
	geoDistLine(&exp, &nlines, strconv.FormatInt(int64(len(tris)), 10))
	for _, t := range tris {
		geoDistLine(&exp, &nlines, strconv.FormatInt(t.i, 10), " ", strconv.FormatInt(t.j, 10), " ", strconv.FormatInt(t.k, 10))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// PolygonTriangulationGenerator emits a convex parabola polygon (i, i^2);
// the ear clip yields a fan at vertex n-1.
type PolygonTriangulationGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *PolygonTriangulationGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the parabola polygon and its fan triangulation.
func (g *PolygonTriangulationGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	_ = seed
	var in bytes.Buffer
	geoDistGrow(&in, n*16+16)
	first := true
	geoDistInt(&in, &first, n)
	for i := int64(0); i < n; i++ {
		geoDistInt(&in, &first, i)
		geoDistInt(&in, &first, i*i)
	}
	type tri struct{ i, j, k int64 }
	var tris []tri
	if n >= 3 {
		if n == 3 {
			tris = append(tris, tri{0, 1, 2})
		} else {
			for k := int64(0); k < n-2; k++ {
				tris = append(tris, tri{k, k + 1, n - 1})
			}
		}
	}
	var exp bytes.Buffer
	geoDistGrow(&exp, int64(len(tris))*16+16)
	nlines := 0
	geoDistLine(&exp, &nlines, strconv.FormatInt(int64(len(tris)), 10))
	for _, t := range tris {
		geoDistLine(&exp, &nlines, strconv.FormatInt(t.i, 10), " ", strconv.FormatInt(t.j, 10), " ", strconv.FormatInt(t.k, 10))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// VoronoiGenerator emits a two-row integer grid; the reference is the
// brute-force Delaunay circumcenters, EPS-deduped.
type VoronoiGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *VoronoiGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the grid points and the deduped Voronoi vertices
// rendered with two decimals, halves up.
func (g *VoronoiGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	_ = seed
	var cols int64
	if n > 1 {
		cols = (n + 1) / 2
		if cols < 2 {
			cols = 2
		}
	} else {
		cols = 1
	}
	pts := make([][2]int64, 0, n)
	for i := int64(0); i < n; i++ {
		pts = append(pts, [2]int64{i % cols, i / cols})
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*8+16)
	first := true
	geoDistInt(&in, &first, n)
	for _, p := range pts {
		geoDistInt(&in, &first, p[0])
		geoDistInt(&in, &first, p[1])
	}
	raw := make([][2]float64, n)
	for i, p := range pts {
		raw[i] = [2]float64{float64(p[0]), float64(p[1])}
	}
	var centers [][2]float64
	for i := int64(0); i < n; i++ {
		for j := i + 1; j < n; j++ {
			for k := j + 1; k < n; k++ {
				if geoDistEmptyCircle(raw, int(i), int(j), int(k)) {
					ux, uy, _, ok := geoDistCircumcircle(raw[int(i)], raw[int(j)], raw[int(k)])
					if !ok {
						continue
					}
					centers = append(centers, [2]float64{ux, uy})
				}
			}
		}
	}
	// Stable sort: Python's sort is stable, and dedup keeps the first
	// of each within-tolerance run.
	sort.SliceStable(centers, func(i, j int) bool {
		if centers[i][0] != centers[j][0] {
			return centers[i][0] < centers[j][0]
		}
		return centers[i][1] < centers[j][1]
	})
	var uniq [][2]float64
	for _, pt := range centers {
		if len(uniq) == 0 || geoDistPointCmp(uniq[len(uniq)-1], pt) != 0 {
			uniq = append(uniq, pt)
		}
	}
	var exp bytes.Buffer
	geoDistGrow(&exp, int64(len(uniq))*16+16)
	nlines := 0
	geoDistLine(&exp, &nlines, strconv.FormatInt(int64(len(uniq)), 10))
	for _, pt := range uniq {
		geoDistLine(&exp, &nlines, geoDistFormatFixed(pt[0], 2), " ", geoDistFormatFixed(pt[1], 2))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// ByzantineEigGenerator emits a random loyal/faulty split; the decision
// is 0 iff the commander is faulty.
type ByzantineEigGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *ByzantineEigGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the commander value plus faulty ids and the per-loyal
// decision lines.
func (g *ByzantineEigGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	nf := rng.RandInt(0, 3)
	cmd := rng.RandInt(-1000000000000000000, 1000000000000000000)
	var ids []int64
	if n > 0 && nf > 0 {
		pool := make([]int64, n)
		for i := range pool {
			pool[i] = int64(i)
		}
		rng.Shuffle(len(pool), func(i, j int) { pool[i], pool[j] = pool[j], pool[i] })
		k := nf
		if k > n {
			k = n
		}
		ids = append([]int64(nil), pool[:k]...)
		sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	}
	faulty := make(map[int64]bool, len(ids))
	for _, id := range ids {
		faulty[id] = true
	}
	decision := cmd
	if faulty[0] {
		decision = 0
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*4+64)
	first := true
	geoDistInt(&in, &first, n)
	geoDistInt(&in, &first, nf)
	geoDistInt(&in, &first, cmd)
	geoDistInt(&in, &first, int64(len(ids)))
	for _, id := range ids {
		geoDistInt(&in, &first, id)
	}
	var exp bytes.Buffer
	geoDistGrow(&exp, n*24+32)
	nlines := 0
	geoDistLine(&exp, &nlines, "DECISION: ", strconv.FormatInt(decision, 10))
	for i := int64(1); i < n; i++ {
		if !faulty[i] {
			geoDistLine(&exp, &nlines, "P", strconv.FormatInt(i, 10), ": ", strconv.FormatInt(decision, 10))
		}
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// VectorClocksGenerator emits random LOCAL/SEND/RECV ops plus causality
// queries over the events.
type VectorClocksGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *VectorClocksGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the op stream and queries plus the causality verdicts.
func (g *VectorClocksGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var procs int64
	if n > 0 {
		procs = n / 500
		if procs < 2 {
			procs = 2
		}
		if procs > 64 {
			procs = 64
		}
	} else {
		procs = 2
	}
	var nq int64
	if n > 0 {
		nq = n / 10
		if nq < 2 {
			nq = 2
		}
	} else {
		nq = 0
	}
	type vcOp struct {
		op   string
		p    int64
		name string
	}
	type vcQuery struct {
		e1, e2 string
	}
	var ops []vcOp
	var labels []string
	var sent []string
	var mctr int64
	for i := int64(0); i < n; i++ {
		p := rng.RandInt(0, procs-1)
		r := rng.Random()
		if r < 0.4 {
			name := "L" + strconv.FormatInt(i, 10)
			ops = append(ops, vcOp{"LOCAL", p, name})
			labels = append(labels, name)
		} else if r < 0.7 {
			mctr++
			name := "M" + strconv.FormatInt(mctr, 10)
			ops = append(ops, vcOp{"SEND", p, name})
			sent = append(sent, name)
			labels = append(labels, name+"_SEND")
		} else {
			var msg string
			if len(sent) > 0 && rng.Random() < 0.8 {
				msg = sent[rng.RandInt(0, int64(len(sent))-1)]
			} else {
				mctr++
				msg = "M" + strconv.FormatInt(mctr, 10)
			}
			ops = append(ops, vcOp{"RECV", p, msg})
			labels = append(labels, msg+"_RECV")
		}
	}
	var queries []vcQuery
	for t := int64(0); t < nq; t++ {
		var e1, e2 string
		if len(labels) > 0 && rng.Random() < 0.85 {
			e1 = labels[rng.RandInt(0, int64(len(labels))-1)]
		} else {
			e1 = "Z" + strconv.FormatInt(t, 10)
		}
		if len(labels) > 0 && rng.Random() < 0.85 {
			e2 = labels[rng.RandInt(0, int64(len(labels))-1)]
		} else {
			e2 = "Z" + strconv.FormatInt(t, 10) + "b"
		}
		queries = append(queries, vcQuery{e1, e2})
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*24+nq*24+32)
	first := true
	geoDistInt(&in, &first, procs)
	geoDistInt(&in, &first, n)
	for _, op := range ops {
		geoDistTok(&in, &first, op.op)
		geoDistInt(&in, &first, op.p)
		geoDistTok(&in, &first, op.name)
	}
	geoDistInt(&in, &first, nq)
	for _, q := range queries {
		geoDistTok(&in, &first, q.e1)
		geoDistTok(&in, &first, q.e2)
	}
	vc := make([][]int64, procs)
	for i := range vc {
		vc[i] = make([]int64, procs)
	}
	events := make(map[string][]int64)
	inflight := make(map[string][]int64)
	for _, op := range ops {
		switch op.op {
		case "LOCAL":
			vc[op.p][op.p]++
			events[op.name] = append([]int64(nil), vc[op.p]...)
		case "SEND":
			vc[op.p][op.p]++
			inflight[op.name] = append([]int64(nil), vc[op.p]...)
			events[op.name+"_SEND"] = append([]int64(nil), vc[op.p]...)
		default:
			cur := vc[op.p]
			got, ok := inflight[op.name]
			var merged []int64
			if ok {
				merged = make([]int64, procs)
				for k := range merged {
					if cur[k] >= got[k] {
						merged[k] = cur[k]
					} else {
						merged[k] = got[k]
					}
				}
			} else {
				merged = append([]int64(nil), cur...)
			}
			merged[op.p]++
			vc[op.p] = merged
			events[op.name+"_RECV"] = append([]int64(nil), merged...)
		}
	}
	zero := make([]int64, procs)
	var exp bytes.Buffer
	geoDistGrow(&exp, nq*32+8)
	nlines := 0
	for _, q := range queries {
		v1, ok := events[q.e1]
		if !ok {
			v1 = zero
		}
		v2, ok := events[q.e2]
		if !ok {
			v2 = zero
		}
		aleb, blea := true, true
		for k := int64(0); k < procs; k++ {
			if v1[k] > v2[k] {
				aleb = false
			}
			if v2[k] > v1[k] {
				blea = false
			}
		}
		switch {
		case aleb && blea:
			geoDistLine(&exp, &nlines, q.e1, " == ", q.e2)
		case aleb:
			geoDistLine(&exp, &nlines, q.e1, " -> ", q.e2)
		case blea:
			geoDistLine(&exp, &nlines, q.e2, " -> ", q.e1)
		default:
			geoDistLine(&exp, &nlines, q.e1, " || ", q.e2)
		}
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// PaxosGenerator emits a random PREPARE/PROPOSE stream over A acceptors;
// the reference is the per-acceptor simulation.
type PaxosGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *PaxosGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the op stream and the promise/chosen/rejected verdicts.
func (g *PaxosGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var acc int64
	if n > 0 {
		acc = n / 200
		if acc < 3 {
			acc = 3
		}
		if acc > 256 {
			acc = 256
		}
	} else {
		acc = 3
	}
	type paxOp struct {
		op     string
		num    int64
		val    string
		hasVal bool
	}
	var ops []paxOp
	var cur int64
	for i := int64(0); i < n; i++ {
		if rng.Random() < 0.5 {
			cur += rng.RandInt(1, 3)
			ops = append(ops, paxOp{"PREPARE", cur, "", false})
		} else {
			var num int64
			if rng.Random() < 0.7 {
				cur += rng.RandInt(1, 3)
				num = cur
			} else {
				num = rng.RandInt(0, cur)
			}
			ops = append(ops, paxOp{"PROPOSE", num, "v" + strconv.FormatInt(i, 10), true})
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*20+32)
	first := true
	geoDistInt(&in, &first, acc)
	geoDistInt(&in, &first, n)
	for _, op := range ops {
		geoDistTok(&in, &first, op.op)
		geoDistInt(&in, &first, op.num)
		if op.hasVal {
			geoDistTok(&in, &first, op.val)
		}
	}
	majority := 1 + acc/2
	promised := make([]int64, acc)
	accepted := make([]int64, acc)
	aval := make([]string, acc)
	var exp bytes.Buffer
	geoDistGrow(&exp, n*28+8)
	nlines := 0
	for _, op := range ops {
		if op.op == "PREPARE" {
			var cnt, topN int64
			var topV string
			for i := int64(0); i < acc; i++ {
				if promised[i] < op.num {
					promised[i] = op.num
					cnt++
					if accepted[i] > topN {
						topN = accepted[i]
						topV = aval[i]
					}
				}
			}
			if cnt >= majority {
				if topN > 0 {
					geoDistLine(&exp, &nlines, "PROMISED ", strconv.FormatInt(op.num, 10), " PREV ", strconv.FormatInt(topN, 10), " ", topV)
				} else {
					geoDistLine(&exp, &nlines, "PROMISED ", strconv.FormatInt(op.num, 10), " NONE")
				}
			} else {
				geoDistLine(&exp, &nlines, "PREPARE REJECTED ", strconv.FormatInt(op.num, 10))
			}
		} else {
			var cnt int64
			for i := int64(0); i < acc; i++ {
				if promised[i] <= op.num {
					promised[i] = op.num
					accepted[i] = op.num
					aval[i] = op.val
					cnt++
				}
			}
			if cnt >= majority {
				geoDistLine(&exp, &nlines, "CHOSEN ", strconv.FormatInt(op.num, 10), " ", op.val)
			} else {
				geoDistLine(&exp, &nlines, "PROPOSE REJECTED ", strconv.FormatInt(op.num, 10))
			}
		}
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// RaftGenerator emits an election/append/print event mix; the reference
// is the full cluster-state simulation.
type RaftGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *RaftGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the event stream and the resulting cluster log lines.
func (g *RaftGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var nodes int64
	if n > 0 {
		nodes = n / 200
		if nodes < 3 {
			nodes = 3
		}
		if nodes > 128 {
			nodes = 128
		}
	} else {
		nodes = 3
	}
	type raftEv struct {
		ev   string
		a    int64
		b    string
		hasB bool
	}
	var events []raftEv
	for i := int64(0); i < n; i++ {
		r := rng.Random()
		if r < 0.3 {
			events = append(events, raftEv{"START_ELECTION", rng.RandInt(0, nodes-1), "", false})
		} else if r < 0.7 {
			events = append(events, raftEv{"APPEND_ENTRY", rng.RandInt(0, nodes-1), "c" + strconv.FormatInt(i, 10), true})
		} else {
			events = append(events, raftEv{"PRINT_STATE", rng.RandInt(0, nodes-1), "", false})
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*28+32)
	first := true
	geoDistInt(&in, &first, nodes)
	geoDistInt(&in, &first, n)
	for _, e := range events {
		geoDistTok(&in, &first, e.ev)
		geoDistInt(&in, &first, e.a)
		if e.hasB {
			geoDistTok(&in, &first, e.b)
		}
	}
	majority := 1 + nodes/2
	roles := make([]string, nodes)
	terms := make([]int64, nodes)
	voted := make([]int64, nodes)
	commits := make([]int64, nodes)
	for i := range roles {
		roles[i] = "FOLLOWER"
		voted[i] = -1
		commits[i] = -1
	}
	var logsz int64
	var exp bytes.Buffer
	geoDistGrow(&exp, n*32+8)
	nlines := 0
	for _, e := range events {
		switch e.ev {
		case "START_ELECTION":
			c := e.a
			t := terms[c] + 1
			roles[c] = "CANDIDATE"
			terms[c] = t
			voted[c] = c
			votes := int64(1)
			for i := int64(0); i < nodes; i++ {
				if i != c && terms[i] < t {
					roles[i] = "FOLLOWER"
					terms[i] = t
					voted[i] = c
					votes++
				}
			}
			if votes >= majority {
				roles[c] = "LEADER"
				geoDistLine(&exp, &nlines, "LEADER ELECTED: ", strconv.FormatInt(c, 10), " TERM: ", strconv.FormatInt(t, 10))
			} else {
				roles[c] = "FOLLOWER"
				geoDistLine(&exp, &nlines, "ELECTION FAILED: ", strconv.FormatInt(c, 10))
			}
		case "APPEND_ENTRY":
			l := e.a
			cmd := e.b
			if roles[l] != "LEADER" {
				geoDistLine(&exp, &nlines, "REJECT NOT LEADER")
			} else {
				t := terms[l]
				sz := logsz + 1
				for i := int64(0); i < nodes; i++ {
					if i != l {
						roles[i] = "FOLLOWER"
						terms[i] = t
					}
				}
				logsz = sz
				if nodes >= majority {
					for i := range commits {
						commits[i] = sz - 1
					}
					geoDistLine(&exp, &nlines, "COMMITTED ", cmd, " AT INDEX ", strconv.FormatInt(sz-1, 10))
				}
			}
		default:
			p := e.a
			geoDistLine(&exp, &nlines, "NODE ", strconv.FormatInt(p, 10), " ROLE ", roles[p], " TERM ", strconv.FormatInt(terms[p], 10), " COMMITTED ", strconv.FormatInt(commits[p], 10))
		}
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// LeaderElectionGenerator emits a random bignum ring of ids; the
// reference is the LCR sum plus the HS closed form over phases.
type LeaderElectionGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *LeaderElectionGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the ring ids and the leader/LCR/HS message counts.
func (g *LeaderElectionGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var ids []int64
	for i := int64(0); i < n; i++ {
		if len(ids) > 0 && rng.Random() < 0.1 {
			ids = append(ids, ids[rng.RandInt(0, int64(len(ids))-1)])
		} else {
			ids = append(ids, rng.RandInt(-1000000000000000000, 1000000000000000000))
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, n*20+16)
	first := true
	geoDistInt(&in, &first, n)
	for _, v := range ids {
		geoDistInt(&in, &first, v)
	}
	dNext := geoDistNextGreaterDists(ids)
	rev := make([]int64, len(ids))
	for i, v := range ids {
		rev[len(ids)-1-i] = v
	}
	fwd := geoDistNextGreaterDists(rev)
	var dPrev []int64
	if n > 0 {
		dPrev = make([]int64, n)
		for i := int64(0); i < n; i++ {
			dPrev[i] = fwd[n-1-i]
		}
	}
	var lcr int64
	for _, d := range dNext {
		lcr += d
	}
	var hs int64
	if n <= 1 {
		if n == 0 {
			hs = 0
		} else {
			hs = 1
		}
	} else {
		var lims []int64
		for lim := int64(1); ; lim *= 2 {
			lims = append(lims, lim)
			if lim >= n/2 {
				break
			}
		}
		dall := append(append([]int64(nil), dNext...), dPrev...)
		sort.Slice(dall, func(i, j int) bool { return dall[i] < dall[j] })
		prefix := make([]int64, len(dall)+1)
		for i, dd := range dall {
			prefix[i+1] = prefix[i] + dd
		}
		total := int64(len(dall))
		for _, lim := range lims {
			k := geoDistBisectRight(dall, lim)
			hs += prefix[k] + 2*lim*(total-int64(k))
		}
	}
	leader := int64(-1)
	for _, v := range ids {
		if v > leader {
			leader = v
		}
	}
	var exp bytes.Buffer
	nlines := 0
	geoDistLine(&exp, &nlines, "LEADER: ", strconv.FormatInt(leader, 10))
	geoDistLine(&exp, &nlines, "LCR MESSAGES: ", strconv.FormatInt(lcr, 10))
	geoDistLine(&exp, &nlines, "HS MESSAGES: ", strconv.FormatInt(hs, 10))
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// GhsMstGenerator emits a random graph with distinct weights; the
// reference is Kruskal's MST with union by size.
type GhsMstGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *GhsMstGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the weighted edges and the MST weight plus edge list.
func (g *GhsMstGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	nodes := n
	if nodes < 2 {
		nodes = 2
	}
	m := 3 * nodes
	weights := make([]int64, m)
	for i := range weights {
		weights[i] = int64(i)
	}
	rng.Shuffle(len(weights), func(i, j int) { weights[i], weights[j] = weights[j], weights[i] })
	type edge struct{ u, v, w int64 }
	edges := make([]edge, 0, m)
	half := m / 2
	for i := int64(0); i < m; i++ {
		u := rng.RandInt(0, nodes-1)
		v := rng.RandInt(0, nodes-2)
		if v >= u {
			v++
		}
		edges = append(edges, edge{u, v, weights[i] - half})
	}
	var in bytes.Buffer
	geoDistGrow(&in, m*20+16)
	first := true
	geoDistInt(&in, &first, nodes)
	geoDistInt(&in, &first, m)
	for _, e := range edges {
		geoDistInt(&in, &first, e.u)
		geoDistInt(&in, &first, e.v)
		geoDistInt(&in, &first, e.w)
	}
	parent := make([]int64, nodes)
	size := make([]int64, nodes)
	for i := range parent {
		parent[i] = int64(i)
		size[i] = 1
	}
	find := func(x int64) int64 {
		var path []int64
		for parent[x] != x {
			path = append(path, x)
			x = parent[x]
		}
		for _, nd := range path {
			parent[nd] = x
		}
		return x
	}
	// Weights are distinct, so this stable sort is a total order.
	sort.SliceStable(edges, func(i, j int) bool { return edges[i].w < edges[j].w })
	var weight int64
	var pairs [][2]int64
	for _, e := range edges {
		ra := find(e.u)
		rb := find(e.v)
		if ra != rb {
			if size[ra] >= size[rb] {
				parent[rb] = ra
				size[ra] += size[rb]
			} else {
				parent[ra] = rb
				size[rb] += size[ra]
			}
			weight += e.w
			a, b := e.u, e.v
			if b < a {
				a, b = b, a
			}
			pairs = append(pairs, [2]int64{a, b})
		}
	}
	geoDistSortPoints(pairs)
	var exp bytes.Buffer
	geoDistGrow(&exp, int64(len(pairs))*12+48)
	nlines := 0
	geoDistLine(&exp, &nlines, "GHS MST Weight: ", strconv.FormatInt(weight, 10))
	geoDistLine(&exp, &nlines, "Edges: ", strconv.FormatInt(int64(len(pairs)), 10))
	for _, p := range pairs {
		geoDistLine(&exp, &nlines, strconv.FormatInt(p[0], 10), " ", strconv.FormatInt(p[1], 10))
	}
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}

// ChandyLamportGenerator emits a transfer/receive/snapshot mix; the
// reference is the FIFO-channel snapshot simulation.
type ChandyLamportGenerator struct {
	scales []int64
}

// Scales returns the measurement scales.
func (g *ChandyLamportGenerator) Scales() []int64 {
	return g.scales
}

// Generate emits the balances plus event stream and the recorded
// snapshot with its channel contents and total.
func (g *ChandyLamportGenerator) Generate(n int64, seed int64) ([]byte, []byte, FitN, error) {
	rng := mtrand.New(seed)
	var procs int64
	if n > 0 {
		procs = n / 500
		if procs < 2 {
			procs = 2
		}
		if procs > 64 {
			procs = 64
		}
	} else {
		procs = 2
	}
	bals := make([]int64, procs)
	for i := range bals {
		bals[i] = rng.RandInt(0, 1000)
	}
	type clEv struct {
		ev   string
		a, b int64
		c    int64
	}
	var events []clEv
	for i := int64(0); i < n; i++ {
		if i%10 == 9 {
			a := rng.RandInt(0, procs-1)
			events = append(events, clEv{"SNAPSHOT_INIT", a, 0, 0})
			continue
		}
		r := rng.Random()
		if r < 0.55 {
			a := rng.RandInt(0, procs-1)
			b := rng.RandInt(0, procs-1)
			c := rng.RandInt(1, 100)
			events = append(events, clEv{"TRANSFER", a, b, c})
		} else {
			a := rng.RandInt(0, procs-1)
			b := rng.RandInt(0, procs-1)
			events = append(events, clEv{"RECEIVE", a, b, 0})
		}
	}
	var in bytes.Buffer
	geoDistGrow(&in, procs*4+n*28+32)
	first := true
	geoDistInt(&in, &first, procs)
	geoDistInt(&in, &first, n)
	for _, b := range bals {
		geoDistInt(&in, &first, b)
	}
	for _, e := range events {
		geoDistTok(&in, &first, e.ev)
		geoDistInt(&in, &first, e.a)
		if e.ev == "TRANSFER" {
			geoDistInt(&in, &first, e.b)
			geoDistInt(&in, &first, e.c)
		} else if e.ev == "RECEIVE" {
			geoDistInt(&in, &first, e.b)
		}
	}
	balance := append([]int64(nil), bals...)
	recorded := make(map[int64]bool)
	recbal := make([]int64, procs)
	chans := make(map[[2]int64][]string)
	recchans := make(map[[2]int64][]string)
	recording := make(map[[2]int64]bool)
	push := func(chanmap map[[2]int64][]string, key [2]int64, msg string) {
		chanmap[key] = append(chanmap[key], msg)
	}
	onMarker := func(u, v int64) {
		if recorded[v] {
			delete(recording, [2]int64{u, v})
			return
		}
		recorded[v] = true
		recbal[v] = balance[v]
		for k := int64(0); k < procs; k++ {
			if k == v {
				continue
			}
			if k != u {
				recording[[2]int64{k, v}] = true
			}
			push(chans, [2]int64{v, k}, "MARKER")
		}
	}
	onSnapshotInit := func(p int64) {
		if recorded[p] {
			return
		}
		recorded[p] = true
		recbal[p] = balance[p]
		for k := int64(0); k < procs; k++ {
			if k == p {
				continue
			}
			recording[[2]int64{k, p}] = true
			push(chans, [2]int64{p, k}, "MARKER")
		}
	}
	chanOff := make(map[[2]int64]int)
	for _, e := range events {
		switch e.ev {
		case "TRANSFER":
			balance[e.a] -= e.c
			push(chans, [2]int64{e.a, e.b}, strconv.FormatInt(e.c, 10))
		case "RECEIVE":
			key := [2]int64{e.a, e.b}
			q := chans[key]
			off := chanOff[key]
			if q == nil || off >= len(q) {
				continue
			}
			msg := q[off]
			chanOff[key] = off + 1
			if msg == "MARKER" {
				onMarker(e.a, e.b)
			} else {
				mv, _ := strconv.ParseInt(msg, 10, 64)
				balance[e.b] += mv
				if recording[key] {
					push(recchans, key, msg)
				}
			}
		default:
			onSnapshotInit(e.a)
		}
	}
	var exp bytes.Buffer
	nlines := 0
	for i := int64(0); i < procs; i++ {
		geoDistLine(&exp, &nlines, "P", strconv.FormatInt(i, 10), ": ", strconv.FormatInt(recbal[i], 10))
	}
	var total int64
	for _, v := range recbal {
		total += v
	}
	for u := int64(0); u < procs; u++ {
		for v := int64(0); v < procs; v++ {
			for _, msg := range recchans[[2]int64{u, v}] {
				geoDistLine(&exp, &nlines, "CHANNEL ", strconv.FormatInt(u, 10), "->", strconv.FormatInt(v, 10), ": ", msg)
				mv, _ := strconv.ParseInt(msg, 10, 64)
				total += mv
			}
		}
	}
	geoDistLine(&exp, &nlines, "TOTAL: ", strconv.FormatInt(total, 10))
	geoDistEndLines(&exp, nlines)
	return in.Bytes(), exp.Bytes(), FitNFromInt64(n), nil
}
