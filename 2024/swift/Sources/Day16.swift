import Collections

struct Day16: AdventDay {
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

        static func * (lhs: Int, rhs: Coord) -> Coord {
            return Coord(lhs * rhs.x, lhs * rhs.y)
        }
    }

    enum Direction: Character, CaseIterable {
        case north = "^"
        case south = "v"
        case east = ">"
        case west = "<"

        func coord() -> Coord {
            return switch self {
            case .north: Coord(0, -1)
            case .south: Coord(0, 1)
            case .east: Coord(1, 0)
            case .west: Coord(-1, 0)
            }
        }

        func opposite() -> Direction {
            return switch self {
            case .north: .south
            case .south: .north
            case .east: .west
            case .west: .east
            }
        }
    }

    struct PartialNode: Hashable {
        let coord: Coord
        let direction: Direction

        init(_ coord: Coord, _ direction: Direction) {
            self.coord = coord
            self.direction = direction
        }
    }

    struct Node: Comparable {
        let cost: Double
        let coord: Coord
        let direction: Direction

        static func < (lhs: Node, rhs: Node) -> Bool {
            lhs.cost < rhs.cost
        }

        static func == (lhs: Node, rhs: Node) -> Bool {
            lhs.cost == rhs.cost
        }

        func toPartial() -> PartialNode {
            PartialNode(self.coord, self.direction)
        }
    }

    struct ReindeerMaze {
        let map: [Coord: Character]
        let startPos: Coord
        let endPos: Coord

        func heuristic(_ coord: Coord) -> Double {
//            return Double((coord.x - self.endPos.x) ^ 2 + (coord.y - self.endPos.y) ^ 2).squareRoot()
            return Double(abs(coord.x - self.endPos.x) + abs(coord.y - self.endPos.y))
        }

        func validDirections(_ direction: Direction) -> [(direction: Direction, isTurn: Bool)] {
            // Can only move in the same direction or turn 90
            var ret: [(Direction, Bool)] = []
            ret.reserveCapacity(3)
            ret.append((direction, false))
            switch direction {
            case .north, .south:
                ret.append(contentsOf: [(Direction.east, true), (Direction.west, true)])
            case .east, .west:
                ret.append(contentsOf: [(Direction.north, true), (Direction.south, true)])
            }
            return ret
        }

        func _alphaStar(costs: inout [PartialNode: Int], heap: inout Heap<Node>, target: Coord, backwards: Bool = false) -> Int {
            while let node = heap.popMin() {
                if node.coord == target {
                    return costs[node.toPartial()]!
                }

                for (direction, isTurn) in self.validDirections(node.direction) {
                    let nextCoord = isTurn ? node.coord : node.coord + (backwards ? -1 : 1) * direction.coord()
                    let nextNode = PartialNode(nextCoord, direction)
                    switch self.map[nextCoord]! {
                    case "#":
                        continue
                    default:
                        let gCost = costs[node.toPartial()]! + (isTurn ? 1000 : 1)
                        if gCost >= costs[nextNode, default: Int.max] {
                            continue
                        } else {
                            let hCost = self.heuristic(nextCoord)
                            let fCost = Double(gCost) + hCost
                            costs[nextNode] = gCost
                            heap.insert(Node(cost: fCost, coord: nextCoord, direction: direction))
                        }
                    }
                }
            }
            preconditionFailure("A* did not find the end position")
        }

        func solve(part2: Bool = false) async -> Int {
            async let forwardPass = Task {
                var costs: [PartialNode: Int] = [:]
                costs.reserveCapacity(self.map.filter { $0.value == "." || $0.value == "E" || $0.value == "S" }.count)

                let startNode = Node(cost: heuristic(self.startPos), coord: self.startPos, direction: .east)
                var heap: Heap<Node> = [startNode]
                costs[startNode.toPartial()] = 0

                return (self._alphaStar(costs: &costs, heap: &heap, target: self.endPos), costs)
            }.value

            if !part2 {
                return (await forwardPass).0
            }

            async let backwardPass = Task {
                var costs: [PartialNode: Int] = [:]
                costs.reserveCapacity(self.map.filter { $0.value == "." || $0.value == "E" || $0.value == "S" }.count)

                var backwardHeap: Heap<Node> = []
                for direction in Direction.allCases {
                    let startNode = Node(cost: heuristic(self.endPos), coord: self.endPos, direction: direction)
                    backwardHeap.insert(startNode)
                    costs[startNode.toPartial()] = 0
                }
                let _ = self._alphaStar(costs: &costs, heap: &backwardHeap, target: self.startPos, backwards: true)
                return costs
            }.value

            let (minPathCost, forwardCosts) = await forwardPass
            let backwardCosts = await backwardPass
            return forwardCosts.filter { key, value in backwardCosts[key, default: Int.max - value] + value == minPathCost }.uniqued(on: { $0.key.coord }).count
        }
    }

    var entities: ReindeerMaze {
        let inputLines = self.data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")
        var map: [Coord: Character] = [:], startPos: Coord? = nil, endPos: Coord? = nil
        map.reserveCapacity(inputLines.count * inputLines.first!.count)
        for (y, line) in inputLines.enumerated() {
            xIter: for (x, char) in line.enumerated() {
                let coord = Coord(x, y)
                map[coord] = char
                switch char {
                case "S":
                    startPos = coord
                case "E":
                    endPos = coord
                default:
                    continue xIter
                }
            }
        }
        return ReindeerMaze(map: map, startPos: startPos!, endPos: endPos!)
    }

    func part1() async -> Int {
        return await self.entities.solve()
    }

    func part2() async -> Int {
        return await self.entities.solve(part2: true)
    }
}
