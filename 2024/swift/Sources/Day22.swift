import Parsing

extension Int {
    func mix(_ x: Int) -> Int {
        return x ^ self
    }

    func prune() -> Int {
        return self % 16777216
    }
}

struct Day22: AdventDay {
    var data: String

    func stepPRNG(_ key: Int) -> Int {
        var key = key.mix(key * 64).prune()
        key = key.mix(Int(key / 32)).prune()
        key = key.mix(key * 2048).prune()
        return key
    }

    var entities: [Int] {
        do {
            return try Many { Int.parser() } separator: { "\n" } terminator: { End() }
                .parse(data.trimmingCharacters(in: .whitespacesAndNewlines))
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }

    func part1() async -> Any {
        return await withTaskGroup(of: Int.self) { group in
            for number in entities {
                group.addTask {
                    var key = number
                    for _ in 0..<2000 {
                        key = stepPRNG(key)
                    }
                    return key
                }
            }
            return await group.reduce(0, +)
        }
    }

    struct Diff: Hashable {
        let diff0: Int
        let diff1: Int
        let diff2: Int
        let diff3: Int

        init(_ diff0: Int, _ diff1: Int, _ diff2: Int, _ diff3: Int) {
            self.diff0 = diff0
            self.diff1 = diff1
            self.diff2 = diff2
            self.diff3 = diff3
        }
    }

    func part2() -> Any {
        var bananas: [Diff: Int] = [:]
        for buyer in entities {
            var secret = buyer
            var oldPrice = secret % 10
            var deltas: [Int] = []
            var seen: Set<Diff> = []
            for i in 0..<2000 {
                secret = stepPRNG(secret)
                let price = secret % 10

                let delta = price - oldPrice
                deltas.append(delta)

                oldPrice = price

                if i < 4 { continue }
                let diff = Diff(deltas[i - 3], deltas[i - 2], deltas[i - 1], deltas[i])
                if seen.contains(diff) { continue }
                seen.insert(diff)
                bananas[diff, default: 0] += price
            }
        }
        return bananas.max(by: { $0.value < $1.value })!.value
    }
}
