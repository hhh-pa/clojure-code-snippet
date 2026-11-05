/* snip Pin tool: dynamic instruction count.
 *
 * Output (-o file) is one line, the contract that
 * scripts/ci/go/internal/measure/parsers.go parses
 * (ParsePinCount takes the last Count line):
 *
 *   Count <total dynamic instructions, all threads>
 *
 * Instructions only: memory is benchmarked via RSS (GNU time
 * maxrss_kb around the Pin run), never traced here, so the tool
 * stays a plain basic-block counter. Addition commutes, so the
 * total is thread-count independent.
 */

#include <cstdio>
#include <iostream>
#include <string>

#include "pin.H"

KNOB<std::string> KnobOutputFile(KNOB_MODE_WRITEONCE, "pintool", "o", "inscount.out",
                                 "specify output file name");

static UINT64 icount = 0;

static FILE* out = 0;

// Hot path: one LOCK XADD per basic block.
VOID PIN_FAST_ANALYSIS_CALL CountIns(UINT32 c) { __sync_fetch_and_add(&icount, (UINT64)c); }

// Pin calls this function every time a new trace is encountered.
VOID Trace(TRACE trace, VOID* v)
{
    (void)v;
    for (BBL bbl = TRACE_BblHead(trace); BBL_Valid(bbl); bbl = BBL_Next(bbl))
    {
        BBL_InsertCall(bbl, IPOINT_ANYWHERE, MAKE_AFUNPTR(CountIns), IARG_FAST_ANALYSIS_CALL, IARG_UINT32,
                       BBL_NumIns(bbl), IARG_END);
    }
}

VOID Fini(INT32 code, VOID* v)
{
    (void)code;
    (void)v;
    if (out == 0) return;
    fprintf(out, "Count %llu\n", (unsigned long long)icount);
    fclose(out);
    out = 0;
}

INT32 Usage()
{
    std::cerr << "snip tool: counts dynamic instructions" << std::endl;
    std::cerr << std::endl << KNOB_BASE::StringKnobSummary() << std::endl;
    return -1;
}

int main(int argc, char* argv[])
{
    if (PIN_Init(argc, argv)) return Usage();

    out = fopen(KnobOutputFile.Value().c_str(), "w");

    TRACE_AddInstrumentFunction(Trace, 0);
    PIN_AddFiniFunction(Fini, 0);

    PIN_StartProgram();

    return 0;
}
