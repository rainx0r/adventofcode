import Algorithms
import Collections

struct Day12: AdventDay {
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
        
        static let NORTH = Coord(0, -1)
        static let SOUTH = Coord(0, 1)
        static let EAST = Coord(1, 0)
        static let WEST = Coord(-1, 0)
        
        func neighbours() -> [Coord] {
            return [self + Coord.NORTH, self + Coord.SOUTH, self + Coord.EAST, self + Coord.WEST]
        }
        
        func getEdges(_ map: [Coord: Character]) -> Int {
            let directions = [Coord.NORTH, Coord.EAST, Coord.SOUTH, Coord.WEST, Coord.NORTH]
            let directionPairs = zip(directions.dropLast(), directions.dropFirst())
            var ret = 0
            for pair in directionPairs {
                if (map[self] != map[self + pair.0] && map[self] != map[self + pair.1]) || (map[self] == map[self + pair.0] && map[self] == map[self + pair.1]) && map[self] != map[self + pair.0 + pair.1] {
                   ret += 1
                }
            }
            return ret
        }
    }

    var entities: [Coord: Character] {
        var ret: [Coord: Character] = [:]
        ret.reserveCapacity(data.trimmingCharacters(in: .whitespacesAndNewlines).count)
        for (j, line) in data.split(separator: "\n").enumerated() {
            for (i, char) in line.enumerated() {
                ret[Coord(i, j)] = char
            }
        }
        return ret
    }
    

    func getIslands(_ map: [Coord: Character]) -> [(Character, Set<Coord>)] {
        var islands: [(Character, Set<Coord>)] = []
        var visited = Set<Coord>()
        
        // dfs
        for node in map.keys {
            if visited.contains(node) { continue }
            var queue = Deque<Coord>([node])
            var island = (map[node]!, Set<Coord>())
            
            while !queue.isEmpty {
                let current = queue.popFirst()!
                if island.1.contains(current) { continue }
                
                let value = map[current]!
                if value == map[node]! {
                    island.1.insert(current)
                    visited.insert(current)
                }
                queue.prepend(contentsOf: current.neighbours().filter({ n in map[n] != nil && map[n]! == value }))
            }
            
            if !island.1.isEmpty { islands.append(island) }
        }
        return islands
    }
    
    func area(_ island: Set<Coord>) -> Int {
        return island.count
    }
    
    func perimeter(_ island: Set<Coord>) -> Int {
        var p = 0
        for coord in island {
            p += 4 - coord.neighbours().filter(island.contains).count
        }
        return p
    }
    
    func sides(_ island: Set<Coord>, _ map: [Coord: Character]) -> Int {
        island.map({ $0.getEdges(map) }).reduce(0, +)
    }

    func part1() -> Int {
        let islands = getIslands(entities)
        return islands.map({ area($0.1) * perimeter($0.1) }).reduce(0, +)
    }

    func part2() -> Int {
        let islands = getIslands(entities)
        return islands.map({ area($0.1) * sides($0.1, entities) }).reduce(0, +)
    }
}
