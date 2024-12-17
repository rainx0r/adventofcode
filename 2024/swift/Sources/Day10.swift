import Algorithms
import Collections

struct Day10: AdventDay {
    var data: String
    
    struct Coord: Hashable {
        let x: Int
        let y: Int

        init(_ x: Int, _ y: Int) {
            self.x = x
            self.y = y
        }
        
        static func + (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x + rhs.x, lhs.y + rhs.y)
        }
        
        static func - (lhs: Coord, rhs: Coord) -> Coord {
            return Coord(lhs.x - rhs.x, lhs.y - rhs.y)
        }
    }

    var entities: ([Coord: Int], [Coord]) {
        var ret: [Coord: Int] = [:]
        var trailheads: [Coord] = []
        ret.reserveCapacity(data.trimmingCharacters(in: .whitespacesAndNewlines).count)
        for (j, line) in data.split(separator: "\n").enumerated() {
            for (i, height) in line.enumerated() {
                ret[Coord(i, j)] = height.wholeNumberValue
                if height.wholeNumberValue == 0 { trailheads.append(Coord(i,j)) }
            }
        }
        return (ret, trailheads)
    }
    
    func neighbours(_ coord: Coord, _ map: [Coord: Int]) -> [Coord] {
        [Coord(coord.x - 1, coord.y),
         Coord(coord.x + 1, coord.y),
         Coord(coord.x, coord.y - 1),
         Coord(coord.x, coord.y + 1)].filter(map.keys.contains)
    }
    
    func dfs(_ trailhead: Coord, _ map: [Coord: Int], skipVisited: Bool = true) -> Int {
        guard map[trailhead] != nil else { return 0 }
        
        var visited = Set<Coord>()
        var queue = Deque<Coord>([trailhead])
        var score = 0
        
        while !queue.isEmpty {
            let current = queue.popFirst()!
            if visited.contains(current) && skipVisited { continue }
            
            visited.insert(current)

            let height = map[current]!

            if height == 9 {
                score += 1
            } else {
                queue.prepend(contentsOf: neighbours(current, map).filter({ n in
                    map[n] != nil && map[n]! == height + 1
                }))
            }
        }
        return score
    }

    func part1() -> Int {
        let (map, trailheads) = self.entities
        return trailheads.map({ e in return dfs(e, map) }).reduce(0, +)
    }

    func part2() -> Int {
        let (map, trailheads) = self.entities
        return trailheads.map({ e in return dfs(e, map, skipVisited: false) }).reduce(0, +)
    }
}
