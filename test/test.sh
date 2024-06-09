RED=$(tput setaf 1)
GREEN=$(tput setaf 2)
BLUE=$(tput setaf 4)

echo -e "${BLUE}Running scan test for keywords"
./bin/jcc --file test/scan/keywords.in > test/scan/keywords.out
if diff --color=always test/scan/keywords.out test/scan/keywords.expected; then
    echo -e "${GREEN}Scan test for keywords passed"
else
    echo -e "${RED}Scan test for keywords failed"
fi