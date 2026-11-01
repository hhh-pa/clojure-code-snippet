package snipcore

// Check is one named self-test assertion shared by the measure
// self-test and the slope check suite.
type Check struct {
	Name   string
	Ok     bool
	Detail string
}
