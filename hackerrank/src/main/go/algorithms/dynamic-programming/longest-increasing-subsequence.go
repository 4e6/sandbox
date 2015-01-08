package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
)

func readInput() []int {
	var n int
	bio := bufio.NewReader(os.Stdin)
	n = readInt(bio)

	xs := make([]int, n)

	for i := 0; i < n; i++ {
		xs[i] = readInt(bio)
	}

	return xs
}

func readInt(bio *bufio.Reader) int {
	b, _ := bio.ReadString('\n')
	if b[len(b)-1] == '\n' {
		b = b[:len(b)-1]
	}
	x, err := strconv.Atoi(b)
	if err != nil {
		fmt.Println("strconv.Atoi", err)
	}
	return x
}

// longest increasing subsequence
func lis(D []int) []int {
	L := make([][]int, len(D))

	L[0] = append(L[0], D[0])

	for i := 1; i < len(D); i++ {
		for j := 0; j <= i; j++ {
			if D[j] < D[i] && len(L[i]) < len(L[j]) {
				L[i] = L[j]
			}
		}
		L[i] = append(L[i], D[i])
	}

	//fmt.Println(L)

	lmax := L[0]
	for _, ls := range L {
		if len(ls) > len(lmax) {
			lmax = ls
		}
	}
	return lmax
}

func main() {
	//ints := []int{3, 2, 6, 4, 5, 1}

	//fmt.Println(lis(ints))

	xs := readInput()
	fmt.Println(len(lis(xs)))
}
