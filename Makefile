EXECUTABLES = lic
SRCDIR = hackerrank/src/main/go

all: $(EXECUTABLES)

lic:
	go build -o bin/lic $(SRCDIR)/algorithms/dynamic-programming/longest-increasing-subsequence.go

clean:
	rm bin/*
