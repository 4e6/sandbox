package main

import (
	"main/go/utils"
	"bufio"
	"fmt"
	"math"
	"os"
)

// O(n^2)
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

// O(n log n)
func lis1(X []int) (int, []int) {
	N := len(X)
	P := make([]int, N)
	M := make([]int, N+1)
	L := 0

	for i := 0; i < N; i++ {
		lo := 1
		hi := L
		for lo <= hi {
			mid := int(math.Ceil(float64(lo+hi) / 2))
			if X[M[mid]] < X[i] {
				lo = mid+1
			} else {
				hi = mid-1
			}
		}

		newL := lo

		P[i] = M[newL-1]
		M[newL] = i

		if newL > L {
			L = newL
		}
	}

	S := make([]int, L)
	k := M[L]
	for i := L-1; i >= 0; i-- {
		S[i] = X[k]
		k = P[k]
	}

	return L, S
}

func readInput() []int {
	bio := bufio.NewReader(os.Stdin)
	n, _ := utils.ReadInt(bio)

	xs := make([]int, n)

	for i := 0; i < n; i++ {
		xs[i], _ = utils.ReadInt(bio)
	}

	return xs
}

func main() {
	xs := readInput()
	n, _ := lis1(xs)
	fmt.Println(n)
}
