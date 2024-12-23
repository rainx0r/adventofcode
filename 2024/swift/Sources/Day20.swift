import Algorithms


struct Day20: AdventDay {
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

    struct Racetrack {
        let map: [Coord: Character]
        let startPos: Coord
        let endPos: Coord
    }

    var entities: Racetrack {
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
        return Racetrack(map: map, startPos: startPos!, endPos: endPos!)
    }

    func part1() -> Any {
        0
    }

    func part2() -> Any {
        0
    }
}
