load harness

@test "exp-assign" {
  check 'x := 3 ^ 3' '{x → 27}'
}

@test "exp-if-assign" {
  checkOr 'y := 2; if true then x := 4 ^ y else x := 0' '{x → 16, y → 2}' '{x → 2, y → 16}'
}

@test "exp-pemdas" {
  check 'x:=2^{3 * 2}' '{x → 64}'
}