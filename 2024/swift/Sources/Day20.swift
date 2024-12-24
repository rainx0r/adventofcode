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

        func manhattan(_ other: Coord) -> Int {
            return abs(x - other.x) + abs(y - other.y)
        }
    }
    
    struct CoordPair: Hashable {
        let coord1: Coord
        let coord2: Coord
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
        let bounds: Coord
        let startPos: Coord
        let endPos: Coord

        func getTrack() -> (track: [Coord], coordIdx: [Coord: Int]) {
            var _track: [Coord] = [startPos]
            var pos = 0
            var coordIdx: [Coord: Int] = [startPos: pos]
            var current = startPos
            var previous: Coord? = nil
            while current != endPos {
                var next: Coord? = nil
                for direction in Direction.allCases {
                    let nextCoord = current + direction.coord()
                    if nextCoord != previous && map[nextCoord] != nil && map[nextCoord] != "#" {
                        next = current + direction.coord()
                        break
                    }
                }
                if next != nil {
                    _track.append(next!)
                    previous = current
                    current = next!
                    pos += 1
                    coordIdx[next!] = pos
                }
            }
            return (_track, coordIdx)
        }

        func getNumCheats(track: (track: [Coord], coordIdx: [Coord: Int]), cheatDist: Int, saveAtLeast: Int) -> Int {
            var visited = Set<CoordPair>()
            var count = 0
            for (time1, coord1) in track.track.enumerated() {
                for yOffset in -cheatDist...cheatDist {
                    if !(coord1.y + yOffset >= 0 && coord1.y + yOffset < bounds.y) {
                       continue
                    }
                    let xBound = cheatDist - abs(yOffset)
                    for xOffset in -xBound...xBound {
                        if xOffset == 0 && yOffset == 0 { continue }
                        if !(coord1.x + xOffset >= 0 && coord1.x + xOffset < bounds.y) {
                           continue
                        }
                        let coord2 = coord1 + Coord(xOffset, yOffset)
                        if map[coord2] != "#" && !visited.contains(CoordPair(coord1: coord1, coord2: coord2)) {
                            visited.insert(CoordPair(coord1: coord1, coord2: coord2))
                            if track.coordIdx[coord2]! - time1 - coord2.manhattan(coord1) >= saveAtLeast {
                                count += 1
                            }
                        }
                    }
                }
            }
            return count
        }
    }

    var entities: (rt: Racetrack, track: ([Coord], [Coord: Int])) {
        let inputLines = data.trimmingCharacters(in: .whitespacesAndNewlines).split(separator: "\n")
        var map: [Coord: Character] = [:], startPos: Coord? = nil, endPos: Coord? = nil
        map.reserveCapacity(inputLines.count * inputLines.first!.count)
        let bounds = Coord(inputLines.first!.count, inputLines.count)
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
        let rt = Racetrack(map: map, bounds: bounds, startPos: startPos!, endPos: endPos!)
        return (rt, rt.getTrack())
    }

    func part1() -> Any {
        return entities.rt.getNumCheats(track: entities.track, cheatDist: 2, saveAtLeast: 100)
    }

    func part2() -> Any {
        return entities.rt.getNumCheats(track: entities.track, cheatDist: 20, saveAtLeast: 100)
    }
}
