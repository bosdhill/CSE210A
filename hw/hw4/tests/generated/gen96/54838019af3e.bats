load ../../harness

@test "54838019af3e" {
  check 'if (false  ∧  l    *   x     < 25041678390694270206240146879735448144    +  137366141782248510867561872633953556971)    then  gF    :=  x   *    z  else     skip' '⇒ skip, {}'
}
