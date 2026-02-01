#!/bin/bash
# Quick test script for InCollege COBOL program
# Usage: ./run_tests.sh [--verbose] [--report]

# Note: Not using 'set -e' here because we want to continue running
# all test categories even if some fail, and collect overall results

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
VERBOSE=""
REPORT=""
EXECUTABLE="bin/main"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -v|--verbose)
            VERBOSE="--verbose"
            shift
            ;;
        -r|--report)
            REPORT="--report test-report.json"
            shift
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            echo "Usage: $0 [--verbose] [--report]"
            exit 1
            ;;
    esac
done

echo -e "${YELLOW}InCollege Test Runner${NC}"
echo "======================================"
echo ""

# Check if executable exists
if [ ! -f "$EXECUTABLE" ]; then
    echo -e "${YELLOW}Executable not found. Compiling...${NC}"
    mkdir -p bin
    cobc -x -free -o bin/main src/main.cob

    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Compilation successful${NC}"
    else
        echo -e "${RED}✗ Compilation failed${NC}"
        exit 1
    fi
    echo ""
fi

# Run tests
echo -e "${YELLOW}Running tests...${NC}"
echo ""

# Convert to absolute path
EXECUTABLE_PATH="$(cd "$(dirname "$EXECUTABLE")" && pwd)/$(basename "$EXECUTABLE")"

# Initialize counters
total_exit_code=0

# Find all test fixture directories
for test_dir in tests/fixtures/*/; do
    if [ -d "$test_dir" ]; then
        test_name=$(basename "$test_dir")
        echo ""
        echo -e "${YELLOW}═══════════════════════════════════════${NC}"
        echo -e "${YELLOW}Running ${test_name} tests...${NC}"
        echo -e "${YELLOW}═══════════════════════════════════════${NC}"

        python3 tests/test_runner.py "$EXECUTABLE_PATH" --test-root "$test_dir" $VERBOSE $REPORT
        exit_code=$?

        if [ $exit_code -ne 0 ]; then
            total_exit_code=1
        fi
    fi
done

echo ""
echo -e "${YELLOW}═══════════════════════════════════════${NC}"
if [ $total_exit_code -eq 0 ]; then
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo -e "${RED}✗ Some tests failed. See details above.${NC}"
fi
echo -e "${YELLOW}═══════════════════════════════════════${NC}"

exit $total_exit_code
