EXECUTABLES = algorithms/dynamic-programming/longest-common-subsequence \
	      algorithms/dynamic-programming/longest-increasing-subsequence

export GOPATH = $(CURDIR)/hackerrank

all: $(EXECUTABLES)

$(EXECUTABLES):
	go install main/go/$@

clean:
	rm -r $(GOPATH)/bin $(GOPATH)/pkg
