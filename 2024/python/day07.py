import itertools


with open("data/day07.txt", "r") as f:
    lines = f.readlines()

op_map = {
    "+": lambda x, y: x + y,
    "*": lambda x, y: x * y,
    "||": lambda x, y: int(str(x) + str(y))
}

total_sum = 0

for line in lines:
    line = line.split(":")
    num = int(line[0])
    nums = [int(x) for x in line[1].strip().split(" ")]
    operators = itertools.product(op_map.keys(), repeat=len(nums) - 1)
    for combo in operators:
        result = nums[0]
        for i, op in zip(range(1, len(nums)), combo):
            result = op_map[op](result, nums[i])
            if result > num:
                break

        if result == num:
            total_sum += num
            break

print(total_sum)
