import Algorithms

struct Day11: AdventDay {
    var data: String

    var entities: [Int] {
        data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: " ").map { Int($0)! }
    }
    
    func mutate(_ stone: Int) -> [Int] {
        if stone == 0 { return [1] }
        else {
            let digits = String(stone)
            if digits.count % 2 == 0 {
                return [Int(digits.prefix(digits.count / 2))!, Int(digits.suffix(digits.count / 2))!]
            } else {
                return [2024 * stone]
            }
        }
    }

    func part1() -> Int {
        let blinkingTimes = 25
        var stones = entities
        for _ in 0..<blinkingTimes {
            stones = stones.flatMap { mutate($0) }
        }
        return stones.count
    }

    func part2() -> Int {
        let blinkingTimes = 75
        var stones: [Int: Int] = [:]
        stones.reserveCapacity(entities.count)
        for stone in entities {
            stones[stone] = 1
        }
        for _ in 0..<blinkingTimes {
            var newStones: [Int: Int] = [:]
            for (stone, cnt) in stones {
                for newStone in mutate(stone) {
                    newStones[newStone, default: 0] += cnt
                }
            }
            stones = newStones
        }
        return stones.values.reduce(0, +)
    }
}
