/* How to Compile
--- For mac:
- Install Go pkg
- In terminal where your .go file is you can
- run by: go run ./means.go

--- For Windows:
- Install Go
- cd to pathway of .go file
- run by: go run means.go

*/

// code from: https://rosettacode.org/wiki/Averages/Pythagorean_means#Go

// No inputs are need, already hardcoded in the loop
package main

import (
	"fmt"  // Package for printing
	"math" // Package for mathematical functions
)

func main() {
	sum, sumr, prod := 0., 0., 1. // Initializing sum, reciprocal sum, and product
	for n := 1.; n <= 10; n++ {   // Loop from 1-10
		sum += n      // Computing sum of numbers
		sumr += 1 / n // Computing the sum of reciprocal
		prod *= n     // Computing the product of numbers
	}

	// a calculates the arithmetic mean, g calculates geometric mena, and h calculates harmonic mean
	a, g, h := sum/10, math.Pow(prod, .1), 10/sumr
	fmt.Println("A:", a, "G:", g, "H:", h)        // Prints the computed means
	fmt.Println("A >= G >= H:", a >= g && g >= h) // Verify and print if the inequality A >= G >= H holds true
}
