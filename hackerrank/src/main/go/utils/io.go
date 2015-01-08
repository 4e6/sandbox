package utils

import (
	"bufio"
	"io"
	"strconv"
	"strings"
)

func Pair(xs []int) (int, int) {
	return xs[0], xs[1]
}

func ReadFieldsInt(bio *bufio.Reader) ([]int, error) {
	var err error
	s, serr := ReadString(bio, '\n')
	if serr != nil {
		return []int{0}, serr
	}

	fs := strings.Fields(s)
	xs := make([]int, len(fs))

	for i, f := range fs {
		xs[i], err = strconv.Atoi(f)
		if err != nil {
			return xs, err
		}
	}
	return xs, err
}

func ReadInt(bio *bufio.Reader) (int, error) {
	s, serr := ReadString(bio, '\n')
	if serr != nil {
		return 0, serr
	}

	x, aerr := strconv.Atoi(s)
	if aerr != nil {
		return x, aerr
	}

	return x, aerr
}

func ReadString(bio *bufio.Reader, delim byte) (string, error) {
	s, err := bio.ReadString(delim)
	l := len(s) - 1
	if s[l] == delim {
		s = s[:l]
	}
	if err == io.EOF {
		err = nil
	}
	return s, err
}
