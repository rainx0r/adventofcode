import Collections
import Parsing

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
}

let BOUNDS = Coord(70, 70)

struct Day18: AdventDay {
    var data: String
    
    struct MemorySpace {
        let bytes: Set<Coord>
        
        struct Node: Comparable {
            let coord: Coord
            let cost: Int
            
            static func < (lhs: Node, rhs: Node) -> Bool {
                lhs.cost < rhs.cost
            }

            static func == (lhs: Node, rhs: Node) -> Bool {
                lhs.cost == rhs.cost
            }
            
            var neighbours: [Coord] {
                [Coord(-1, 0), Coord(1, 0), Coord(0, 1), Coord(0, -1)]
                    .map { $0 + coord }
                    .filter { 0...BOUNDS.x ~= $0.x && 0...BOUNDS.y ~= $0.y }
            }
        }
        
        func draw() {
            var drawStr = ""
            for y in 0...BOUNDS.y {
                for x in 0...BOUNDS.x {
                    drawStr.append(bytes.contains(Coord(x, y)) ? "#" : ".")
                }
                drawStr.append("\n")
            }
            print(drawStr)
        }
        
        func astar(start: [Coord], target: Coord) -> Int {
            func heuristic(_ coord: Coord) -> Int {
                return abs(coord.x - target.x) + abs(coord.y - target.y)
            }
            var costs: [Coord: Int] = Dictionary(uniqueKeysWithValues: start.map { ($0, 0) })
            costs.reserveCapacity(BOUNDS.x * BOUNDS.y)
            var heap: Heap<Node> = Heap(start.map { Node(coord: $0, cost: heuristic($0)) })
            
            while let node = heap.popMin() {
                if node.coord == target {
                    return costs[node.coord]!
                }
                
                let neighbours = node.neighbours.filter({ !(bytes.contains($0)) })
                for neighbour in neighbours {
                    let gCost = costs[node.coord]! + 1
                    let hCost = heuristic(neighbour)
                    if gCost < costs[neighbour, default: Int.max] {
                        let newNode = Node(coord: neighbour, cost: gCost + hCost)
                        heap.insert(newNode)
                        costs[neighbour] = gCost
                    }
                }
            }
            return -1
        }
    }

    var entities: [Coord] {
        do {
            let bytes = try Many {
                Parse { Coord($0, $1) } with: {
                        Int.parser(); ","; Int.parser()
                    }
                } separator: {
                    "\n"
                } terminator: {
                    End()
                }.parse(data.trimmingCharacters(in: .whitespacesAndNewlines))
            return bytes
        } catch {
            fatalError("Unable to parse data: \(error)")
        }
    }

    func part1() -> Any {
        let memorySpace = MemorySpace(bytes: Set(entities[..<1024]))
        return memorySpace.astar(start: [Coord(0,0)], target: BOUNDS)
    }

    func part2() -> Any {
        var l = 0, r = entities.count - 1
        while l < r {
            let m = (l + r) / 2
            let memorySpace = MemorySpace(bytes: Set(entities[..<m]))
            if memorySpace.astar(start: [Coord(0,0)], target: BOUNDS) != -1 {
                l = m + 1
            } else {
                r = m
            }
        }
        return entities[r - 1]
    }
}
