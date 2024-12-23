fn mix(nums: &[i64], iterations: usize, decryption_key: i64) -> i64 {
  let nums = nums.iter().map(|x| x * decryption_key).collect::<Vec<_>>();
  let mut ans = (0..nums.len()).collect::<Vec<_>>();
  for _ in 0..iterations {
    for (i, &x) in nums.iter().enumerate() {
      let pos = ans.iter().position(|&y| y == i).unwrap();
      ans.remove(pos);
      let new_i = (pos as i64 + x).rem_euclid(ans.len() as i64) as usize;
      ans.insert(new_i, i);
    }
  }
  let orig_zero_i = nums.iter().position(|&i| i == 0).unwrap();
  let zero_i = ans.iter().position(|&i| i == orig_zero_i).unwrap();
  [1000, 2000, 3000].iter().map(|i| nums[ans[(zero_i + i) % ans.len()]]).sum()
}

#[aoc::main(20)]
fn main(input: &str) -> (i64, i64) {
  let nums = input.lines().map(|l| l.parse().unwrap()).collect::<Vec<_>>();
  (mix(&nums, 1, 1), mix(&nums, 10, 811589153))
}
