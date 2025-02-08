(*Written by Braden Maillet*)
(*run in the terminal using "ocaml OcamlPmeans.ml*)
print_endline "please input the size of your array followed by the array, everything seperated by newlines";;
let n = read_int();;
let arr = Array.make n 0;;
let sum = ref 0;;
let gProd = ref 1;;
let hSum = ref 0.0;;

(*gather intput*)
for i = 0 to n-1 do
  arr.(i) <- read_int()
done;;

(*computations*)
for j = 0 to n-1 do
  sum := (!sum + arr.(j));
  gProd := !gProd * arr.(j);
  hSum := !hSum +. (1.0 /. (float_of_int arr.(j)));
done;;

(*calculate Arithmetic Mean*)
let mean = float_of_int !sum /. float_of_int n;;
print_string "arithmetic mean is: ";;
print_float (mean);;
print_endline "";;

(*Calculate Geometric Mean*)
let gmean = (float_of_int !gProd) ** (1.0 /. float_of_int n);;
print_string "geometric mean is: ";;
print_float (gmean);;
print_endline "";;

(*Calculate Harmonic Mean*)
let hmean = float_of_int n /. !hSum;;
print_string "harmoic mean is: ";;
print_float hmean;;
print_endline "";;