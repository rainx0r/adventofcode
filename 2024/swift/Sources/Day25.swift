import Parsing

typealias Schematic = [Int]
typealias SchematicBlockRows = (String, String, String, String, String, String, String)

struct Day25: AdventDay {
    var data: String

    struct InputBlockParser: Parser {
        var body: some Parser<Substring, SchematicBlockRows> {
            PrefixUpTo("\n").map(String.init); "\n"
            PrefixUpTo("\n").map(String.init); "\n"
            PrefixUpTo("\n").map(String.init); "\n"
            PrefixUpTo("\n").map(String.init); "\n"
            PrefixUpTo("\n").map(String.init); "\n"
            PrefixUpTo("\n").map(String.init); "\n"
            Prefix { $0 != "\n" }.map(String.init)
        }
    }

    struct Schematics {
        let keys: [Schematic]
        let locks: [Schematic]

        init(_ blocks: [SchematicBlockRows]) {
            var _keys: [Schematic] = []
            var _locks: [Schematic] = []

            for block in blocks {
                let isLock = block.0 == "#####" && block.6 == "....."
                var heights = Array(repeating: -1, count: 5)
                var blockRows = [block.1, block.2, block.3, block.4, block.5]
                if !isLock {
                    blockRows = blockRows.reversed() + [block.0]
                } else {
                    blockRows += [block.6]
                }
                for (i, row) in blockRows.enumerated() {
                    for (j, col) in row.enumerated() {
                        if col == "." && heights[j] == -1 {
                            heights[j] = i
                        }
                    }
                }
                if !isLock { _keys.append(heights) } else { _locks.append(heights) }
            }

            self.keys = _keys
            self.locks = _locks
        }
        
        func findNonOverlapping() -> Int {
            var nonOverlapping = 0
            for key in keys {
                for lock in locks {
                    var isOverlapping = false
                    for i in 0..<key.count {
                        isOverlapping = key[i] + lock[i] > 5
                        if isOverlapping { break }
                    }
                    if !isOverlapping { nonOverlapping += 1}
                }
            }
            return nonOverlapping
        }
    }

    var entities: Schematics {
        do {
            let inputBlocks = try Many { InputBlockParser() } separator: { "\n\n" } terminator: { End() }
                .parse(data.trimmingCharacters(in: .whitespacesAndNewlines))
            return Schematics(inputBlocks)
        } catch {
            fatalError("Failed to parse input \(error)")
        }
    }

    func part1() -> Any {
        return entities.findNonOverlapping()
    }

    func part2() -> Any {
        0
    }
}
