RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
BLUE=$(tput setaf 4)

echo -e "${BLUE}Running scan test for keywords"
./bin/jcc --file test/scan/keywords.inp --emit-tokens > test/scan/keywords.out
if diff --color=always test/scan/keywords.out test/scan/keywords.exp; then
    echo -e "${GREEN}Scan test for keywords passed!"
else
    echo -e "${RED}Scan test for keywords failed!"
fi

echo -e "${BLUE}Running scan test for delimiters"
./bin/jcc --file test/scan/delimiters.inp --emit-tokens > test/scan/delimiters.out
if diff --color=always test/scan/delimiters.out test/scan/delimiters.exp; then
    echo -e "${GREEN}Scan test for delimiters passed!"
else
    echo -e "${RED}Scan test for delimiters failed!"
fi