#!/bin/bash
# Quick test script for InCollege COBOL program
# Usage: ./run_tests.sh [--verbose] [--report]

set -e

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

python3 tests/test_runner.py $EXECUTABLE $VERBOSE $REPORT

exit_code=$?

if [ $exit_code -eq 0 ]; then
    echo ""
    echo -e "${GREEN}✓ All tests passed!${NC}"
else
    echo ""
    echo -e "${RED}✗ Some tests failed. See details above.${NC}"
fi

exit $exit_code
