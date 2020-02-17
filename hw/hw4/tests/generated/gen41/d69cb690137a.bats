load ../../harness

@test "d69cb690137a" {
  check 'while false   ∧    y--4 <z  +    x  do x   :=-3    *  y  ' '⇒ skip, {}'
}
