
benchmark:
	stack build && stack exec -- liquid-benchmark -o benchmarks.html --csv benchmarks.csv
