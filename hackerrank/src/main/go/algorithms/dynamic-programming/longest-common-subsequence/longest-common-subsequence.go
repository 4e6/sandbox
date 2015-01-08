package main

import (
	"main/go/utils"
	"bufio"
	"fmt"
	"os"
	"strings"
)

func readInput() ([]int, []int) {
	bio := bufio.NewReader(os.Stdin)
	utils.ReadString(bio, '\n')

	xs, _ := utils.ReadFieldsInt(bio)
	ys, _ := utils.ReadFieldsInt(bio)

	return xs, ys
}

func lcs(a, b []int) []int {
	as := []int(a)
	bs := []int(b)

	aLen := len(as)
	bLen := len(bs)

	ls := make([][]int, aLen+1)
	for i := 0; i <= aLen; i++ {
		ls[i] = make([]int, bLen+1)
	}

	for i := 0; i < aLen; i++ {
		for j := 0; j < bLen; j++ {
			if as[i] == bs[j] {
				ls[i+1][j+1] = ls[i][j] + 1
			} else if ls[i+1][j] > ls[i][j+1] {
				ls[i+1][j+1] = ls[i+1][j]
			} else {
				ls[i+1][j+1] = ls[i][j+1]
			}
		}
	}

	// read subseq
	ss := make([]int, 0, ls[aLen][bLen])
	for x, y := aLen, bLen; x != 0 && y != 0; {
		if ls[x][y] == ls[x-1][y] {
			x--
		} else if ls[x][y] == ls[x][y-1] {
			y--
		} else {
			ss = append(ss, as[x-1])
			x--
			y--
		}
	}
	// reverse subseq
	for i, j := 0, len(ss)-1; i < j; i, j = i+1, j-1 {
		ss[i], ss[j] = ss[j], ss[i]
	}
	return ss
}

func makeOutput(xs []int) string {
	return strings.Trim(fmt.Sprint(xs), "[]")
}

func main() {
	xs, ys := readInput()
	ss := lcs(xs, ys)
	fmt.Println(makeOutput(ss))
}
