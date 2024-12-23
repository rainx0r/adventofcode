import Algorithms

struct Day19: AdventDay {
    var data: String

    var entities: ([String], [String]) {
        let inputParts = data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n\n")
        let designs = inputParts[0].split(separator: ", ")
        let towels = inputParts[1].split(separator: "\n")
        return (designs.map { String($0) }, towels.map { String($0) })
    }

    func possibilities(cache: inout [String: Int], design: String, towels: [String]) -> Int {
        if cache.keys.contains(design) {
            return cache[design]!
        }

        if design.count == 0 {
            cache[design] = 1
            return 1
        }

        cache[design] = towels.filter({ design.hasPrefix($0) }).map { possibilities(cache: &cache, design: String(design.dropFirst($0.count)), towels: towels) }.reduce(0, +)
        return cache[design]!
    }

    func part1() -> Any {
        let (towels, designs) = entities
        var cache: [String: Int] = [:]
        return designs.filter { possibilities(cache: &cache, design: $0, towels: towels) > 0 }.count
    }

    func part2() -> Any {
        let (towels, designs) = entities
        var cache: [String: Int] = [:]
        return designs.map { possibilities(cache: &cache, design: $0, towels: towels) }.reduce(0, +)
    }
}
