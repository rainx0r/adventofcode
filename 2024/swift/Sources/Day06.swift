import Algorithms
import Foundation
import Parsing

struct Day06: AdventDay {
    // Save your data in a corresponding text file in the `Data` directory.
    var data: String

    struct Coord: Hashable {
        let x: Int
        let y: Int

        init(_ x: Int, _ y: Int) {
            self.x = x
            self.y = y
        }
    }

    struct GuardGallivant {
        let startingPos: Coord
        var directions: [Coord: Character]
        var direction: Character
        var rowObstacles: [Int: [Int]]
        var colObstacles: [Int: [Int]]
        var positionOfGuard: Coord
        let bounds: Coord
        var finished: Bool
        var looping: Bool

        init(_ rowObstacles: [Int: [Int]], _ colObstacles: [Int: [Int]], _ positionOfGuard: Coord, _ bounds: Coord) {
            self.rowObstacles = rowObstacles
            self.colObstacles = colObstacles
            self.directions = [:]
            self.directions.reserveCapacity(bounds.x * bounds.y)
            self.bounds = bounds
            self.positionOfGuard = positionOfGuard
            self.startingPos = positionOfGuard
            self.finished = false
            self.looping = false
            self.direction = "^"
        }

        func rotate() -> Character {
            return switch self.direction {
            case "^":
                ">"
            case ">":
                "v"
            case "v":
                "<"
            case "<":
                "^"
            default:
                fatalError("Unexpected character position value")
            }
        }

        func getNextPosition(_ pos: Coord, _ direction: Character) -> Coord? {
            switch direction {
            case "^":
                if let nextObstacleY = self.colObstacles[pos.x]?.reversed().first(where: { $0 < pos.y }) {
                    Coord(pos.x, nextObstacleY + 1)
                } else {
                    nil
                }
            case ">":
                if let nextObstacleX = self.rowObstacles[pos.y]?.first(where: { $0 > pos.x }) {
                    Coord(nextObstacleX - 1, pos.y)
                } else {
                    nil
                }
            case "v":
                if let nextObstacleY = self.colObstacles[pos.x]?.first(where: { $0 > pos.y }) {
                    Coord(pos.x, nextObstacleY - 1)
                } else {
                    nil
                }
            case "<":
                if let nextObstacleX = self.rowObstacles[pos.y]?.reversed().first(where: { $0 < pos.x }) {
                    Coord(nextObstacleX + 1, pos.y)
                } else {
                    nil
                }
            default:
                fatalError("Unexpected character position value")
            }
        }

        mutating func step() {
            let nextPosition: Coord? = self.getNextPosition(self.positionOfGuard, self.direction)
            if nextPosition == nil || nextPosition! != self.positionOfGuard {
                if let existingDirection = self.directions[self.positionOfGuard] {
                    self.looping = existingDirection == self.direction
                } else {
                    self.directions[self.positionOfGuard] = self.direction
                }
            }

            if let pos = nextPosition {
                self.direction = self.rotate()
                self.positionOfGuard = pos
            } else {
                self.finished = true
            }
        }

        func visited() -> Set<Coord> {
            var ret = Set<Coord>()

            for (stepCoord, stepDirection) in self.directions {
                if let nextPosition = getNextPosition(stepCoord, stepDirection) {
                    switch stepDirection {
                    case "^":
                        for j in nextPosition.y...stepCoord.y {
                            ret.insert(Coord(stepCoord.x, j))
                        }
                    case ">":
                        for i in stepCoord.x...nextPosition.x {
                            ret.insert(Coord(i, stepCoord.y))
                        }
                    case "v":
                        for j in stepCoord.y...nextPosition.y {
                            ret.insert(Coord(stepCoord.x, j))
                        }
                    case "<":
                        for i in nextPosition.x...stepCoord.x {
                            ret.insert(Coord(i, stepCoord.y))
                        }
                    default:
                        fatalError("Unexpected character position value")
                    }
                } else {
                    switch stepDirection {
                    case "^":
                        for j in 0...stepCoord.y {
                            ret.insert(Coord(stepCoord.x, j))
                        }
                    case ">":
                        for i in stepCoord.x...(self.bounds.x - 1) {
                            ret.insert(Coord(i, stepCoord.y))
                        }
                    case "v":
                        for j in stepCoord.y...(self.bounds.y - 1) {
                            ret.insert(Coord(stepCoord.x, j))
                        }
                    case "<":
                        for i in 0...(stepCoord.x) {
                            ret.insert(Coord(i, stepCoord.y))
                        }
                    default:
                        fatalError("Unexpected character position value")
                    }
                }
            }

            return ret
        }
    }

    // Splits input data into its component parts and convert from string.
    var entities: GuardGallivant {
        let lines = self.data.split(separator: "\n")
        let firstLine = lines.first!
        var rowObstacles: [Int: [Int]] = [:]
        rowObstacles.reserveCapacity(lines.count)
        var colObstacles: [Int: [Int]] = [:]
        colObstacles.reserveCapacity(firstLine.count)
        var positionOfCharacter: Coord? = nil

        for (j, line) in self.data.split(separator: "\n").enumerated() {
            for (i, char) in line.enumerated() {
                if char == "^" {
                    positionOfCharacter = Coord(i, j)
                } else if char == "#" {
                    if !rowObstacles.keys.contains(j) {
                        rowObstacles[j] = []
                        rowObstacles[j]!.reserveCapacity(firstLine.count)
                    }
                    if !colObstacles.keys.contains(i) {
                        colObstacles[i] = []
                        colObstacles[i]!.reserveCapacity(lines.count)
                    }
                    rowObstacles[j]!.append(i)
                    colObstacles[i]!.append(j)
                }
            }
        }

        for key in rowObstacles.keys {
            rowObstacles[key]!.sort()
        }

        for key in colObstacles.keys {
            colObstacles[key]!.sort()
        }

        return GuardGallivant(rowObstacles, colObstacles, positionOfCharacter!, Coord(lines.count, firstLine.count))
    }

    // Replace this with your solution for the first part of the day's challenge.
    func part1() async throws -> Int {
        var gg = self.entities

        while !gg.finished {
            gg.step()
        }

        return gg.visited().count
    }

    func simulateObstacle(_ gg: GuardGallivant, _ obstaclePos: Coord) -> Bool {
        var gg2 = gg
        if !gg2.rowObstacles.keys.contains(obstaclePos.y) {
            gg2.rowObstacles[obstaclePos.y] = []
        }
        if !gg2.colObstacles.keys.contains(obstaclePos.x) {
            gg2.colObstacles[obstaclePos.x] = []
        }
        gg2.rowObstacles[obstaclePos.y]!.append(obstaclePos.x)
        gg2.rowObstacles[obstaclePos.y]!.sort()

        gg2.colObstacles[obstaclePos.x]!.append(obstaclePos.y)
        gg2.colObstacles[obstaclePos.x]!.sort()
        while !gg2.finished && !gg2.looping {
            gg2.step()
        }
        return gg2.looping
    }

    // Replace this with your solution for the second part of the day's challenge.
    func part2() async throws -> Int {
        let start = Date()
        let gg = self.entities

        var unrolledGG = gg
        while !unrolledGG.finished {
            unrolledGG.step()
        }

        let possibleObstacleCount = await withTaskGroup(of: Bool.self) { group in
            for candidatePosition in unrolledGG.visited() {
                if candidatePosition != gg.startingPos {
                    group.addTask {
                        self.simulateObstacle(gg, candidatePosition)
                    }
                }
            }

            return await group.map { $0 ? 1 : 0 }.reduce(0, +)
        }

        let end = Date()

        print("Time elapsed: \(end.timeIntervalSince(start))s.")

        return possibleObstacleCount
    }
}
