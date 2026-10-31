// Package pyjson renders JSON exactly like Python's
// json.dump(obj, indent=2) plus the caller's trailing newline:
// 2-space indent, no HTML escaping, ensure_ascii (non-ASCII as
// lowercase \uXXXX with surrogate pairs), and Python float repr
// (shortest round-trip, .0 for integral floats, e+XX exponents).
// Key order follows the caller's structs; the sibling cmdtest
// package's golden strategy proves the bytes.
package pyjson

import (
	"bytes"
	"encoding/json"
	"strconv"
	"strings"
	"unicode/utf8"
)

// MarshalIndent2 renders v like json.dumps(v, indent=2) + "\n".
func MarshalIndent2(v any) ([]byte, error) {
	var buf bytes.Buffer
	enc := json.NewEncoder(&buf)
	enc.SetEscapeHTML(false)
	enc.SetIndent("", "  ")
	if err := enc.Encode(v); err != nil {
		return nil, err
	}
	return JSONEscape(buf.Bytes()), nil
}

// Num is a JSON number with Python int-or-float semantics: values
// that Python keeps integral (counts, odd-count int medians, int
// arithmetic) render as bare digits; everything else renders with
// Python float repr.
type Num struct {
	IsInt bool
	I     int64
	F     float64
}

// IntNum builds an integral number.
func IntNum(v int64) Num { return Num{IsInt: true, I: v} }

// FloatNum builds a floating number.
func FloatNum(v float64) Num { return Num{F: v} }

// Float64 returns the numeric value.
func (n Num) Float64() float64 {
	if n.IsInt {
		return float64(n.I)
	}
	return n.F
}

// Add mirrors Python + on ints/floats (int+int stays integral).
func (n Num) Add(o Num) Num {
	if n.IsInt && o.IsInt {
		return IntNum(n.I + o.I)
	}
	return FloatNum(n.Float64() + o.Float64())
}

// Mul mirrors Python * on ints/floats.
func (n Num) Mul(o Num) Num {
	if n.IsInt && o.IsInt {
		return IntNum(n.I * o.I)
	}
	return FloatNum(n.Float64() * o.Float64())
}

// Cmp compares numerically (-1, 0, +1).
func (n Num) Cmp(o Num) int {
	if n.IsInt && o.IsInt {
		switch {
		case n.I < o.I:
			return -1
		case n.I > o.I:
			return 1
		}
		return 0
	}
	a, b := n.Float64(), o.Float64()
	switch {
	case a < b:
		return -1
	case a > b:
		return 1
	}
	return 0
}

// Sum mirrors Python sum() (starts at integral 0).
func Sum(nums []Num) Num {
	acc := IntNum(0)
	for _, n := range nums {
		acc = acc.Add(n)
	}
	return acc
}

// Max mirrors Python max() (returns the winning element as-is).
func Max(nums []Num) Num {
	best := nums[0]
	for _, n := range nums[1:] {
		if n.Cmp(best) > 0 {
			best = n
		}
	}
	return best
}

// MedianInt64 mirrors statistics.median over sorted ints: the
// middle value stays integral for odd counts; even counts average
// to a float, exactly like (a+b)/2.
func MedianInt64(sorted []int64) Num {
	mid := len(sorted) / 2
	if len(sorted)%2 == 1 {
		return IntNum(sorted[mid])
	}
	return FloatNum((float64(sorted[mid-1]) + float64(sorted[mid])) / 2)
}

// MedianFloat64 mirrors statistics.median over sorted floats.
func MedianFloat64(sorted []float64) Num {
	mid := len(sorted) / 2
	if len(sorted)%2 == 1 {
		return FloatNum(sorted[mid])
	}
	return FloatNum((sorted[mid-1] + sorted[mid]) / 2)
}

// MarshalJSON renders the number with Python formatting.
func (n Num) MarshalJSON() ([]byte, error) {
	if n.IsInt {
		return []byte(strconv.FormatInt(n.I, 10)), nil
	}
	return []byte(FloatRepr(n.F)), nil
}

// FloatRepr formats f exactly like Python repr(float): shortest
// round-trip digits, ".0" for integral values, and e+XX exponents
// (signed, at least two digits) outside [1e-4, 1e16).
func FloatRepr(f float64) string {
	if f != f {
		return "NaN"
	}
	if f > 1.7976931348623157e308 {
		return "Infinity"
	}
	if f < -1.7976931348623157e308 {
		return "-Infinity"
	}
	if f == 0 {
		if 1/f < 0 {
			return "-0.0"
		}
		return "0.0"
	}
	// Shortest digits via scientific form: mantissa digits plus
	// decimal-point position.
	sci := strconv.FormatFloat(f, 'e', -1, 64)
	mant, expStr, _ := strings.Cut(sci, "e")
	exp, _ := strconv.Atoi(expStr)
	neg := false
	if strings.HasPrefix(mant, "-") {
		neg = true
		mant = mant[1:]
	}
	digits := strings.Replace(mant, ".", "", 1)
	decpt := exp + 1
	var out strings.Builder
	if neg {
		out.WriteByte('-')
	}
	useExp := decpt > 16 || decpt <= -4
	if !useExp {
		switch {
		case decpt <= 0:
			out.WriteString("0.")
			out.WriteString(strings.Repeat("0", -decpt))
			out.WriteString(digits)
		case decpt >= len(digits):
			out.WriteString(digits)
			out.WriteString(strings.Repeat("0", decpt-len(digits)))
			out.WriteString(".0")
		default:
			out.WriteString(digits[:decpt])
			out.WriteByte('.')
			out.WriteString(digits[decpt:])
		}
		return out.String()
	}
	out.WriteByte(digits[0])
	if len(digits) > 1 {
		out.WriteByte('.')
		out.WriteString(digits[1:])
	}
	out.WriteByte('e')
	point := decpt - 1
	if point < 0 {
		out.WriteByte('-')
		point = -point
	} else {
		out.WriteByte('+')
	}
	expDigits := strconv.Itoa(point)
	if len(expDigits) < 2 {
		out.WriteByte('0')
	}
	out.WriteString(expDigits)
	return out.String()
}

const hexd = "0123456789abcdef"

// JSONEscape rewrites encoder output the way ensure_ascii does,
// plus the two short escapes Go omits: every non-ASCII rune inside
// a string becomes lowercase \uXXXX (surrogate pairs above the
// BMP), and Go-emitted \u0008/\u000c become \b/\f. The scan is
// string- and backslash-aware, so literal "\u0008" source text
// (arriving double-escaped) is never rewritten.
func JSONEscape(in []byte) []byte {
	var out []byte
	inString := false
	for i := 0; i < len(in); {
		c := in[i]
		if !inString {
			if c == '"' {
				inString = true
			}
			out = append(out, c)
			i++
			continue
		}
		// Inside a string.
		switch {
		case c == '\\':
			if i+1 < len(in) && in[i+1] == 'u' && i+6 <= len(in) {
				seq := string(in[i+2 : i+6])
				if seq == "0008" {
					out = append(out, '\\', 'b')
					i += 6
					continue
				}
				if seq == "000c" {
					out = append(out, '\\', 'f')
					i += 6
					continue
				}
			}
			// Copy the escape (or lone backslash) verbatim;
			// a doubled backslash keeps the parity honest.
			out = append(out, c)
			if i+1 < len(in) {
				out = append(out, in[i+1])
				i++
			}
			i++
		case c == '"':
			inString = false
			out = append(out, c)
			i++
		case c < 0x80:
			out = append(out, c)
			i++
		default:
			r, w := utf8.DecodeRune(in[i:])
			if r == utf8.RuneError && w == 1 {
				r = 0xFFFD
			}
			if r > 0xFFFF {
				r -= 0x10000
				appendEscape(&out, 0xD800+r>>10)
				appendEscape(&out, 0xDC00+r&0x3FF)
			} else {
				appendEscape(&out, r)
			}
			i += w
		}
	}
	return out
}

func appendEscape(out *[]byte, r rune) {
	*out = append(*out, '\\', 'u',
		hexd[byte(r>>12)&0xF], hexd[byte(r>>8)&0xF],
		hexd[byte(r>>4)&0xF], hexd[byte(r)&0xF])
}
