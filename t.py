for part in [2020,30000000]:
    one,nums = 4, { e:i+1 for i,e in enumerate([9,19,1,6,0,5]) }
    for turn in range(7, part):
        nums[one], one = turn,0 if one not in nums else turn - nums[one]
    print(one)
