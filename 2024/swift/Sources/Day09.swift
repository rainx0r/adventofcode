import Algorithms

struct Day09: AdventDay {
    var data: String

    var entities: ([String], [Int], [Int]) {
        // Simply expand the input string. Might be intractable for ultra large inputs in the limit I guess...
        var ret: [String] = []
        var fileSizes: [Int] = []
        var fileOffsets: [Int] = []
        var id = 0
        for (i, character) in data.trimmingCharacters(in: .whitespacesAndNewlines).enumerated() {
            let upperBound = character.wholeNumberValue! - 1
            if i % 2 == 0 { // File
                fileOffsets.append(ret.count)
                fileSizes.append(character.wholeNumberValue!)
                for _ in 0 ... upperBound {
                    ret.append(String(id))
                }
                id += 1
            } else { // Free space
                if upperBound >= 0 {
                    for _ in 0 ... upperBound {
                        ret.append(".")
                    }
                }
            }
        }
        return (ret, fileSizes, fileOffsets)
    }

    func updateLifoCounter(_ input: [String], _ currentCounter: Int? = nil) -> Int {
        var lifoCounter = currentCounter ?? input.count - 1
        for i in stride(from: lifoCounter, to: 0, by: -1) {
            if input[i] != "." {
                lifoCounter = i
                break
            }
        }
        return lifoCounter
    }

    func moveFiles(_ data: [String]) -> [String] {
        var workingCopy = data
        var lifoCounter = updateLifoCounter(data)

        for i in 0 ..< workingCopy.count {
            if workingCopy[i] == "." {
                workingCopy[i] = workingCopy[lifoCounter]
                workingCopy[lifoCounter] = "."
                lifoCounter = updateLifoCounter(workingCopy, lifoCounter)
            }
            if (i + 1) > lifoCounter {
                break
            }
        }
        return workingCopy
    }

    func getSpace(_ data: [String], _ start: Int) -> Range<Int> {
        for j in start ..< data.count {
            if data[j] != "." {
                return start ..< j
            }
        }
        return start ..< data.count
    }

    func moveWholeFiles(_ data: [String], _ fileSizes: [Int], _ fileOffsets: [Int]) -> [String] {
        var workingCopy = data
        
        for file in stride(from: fileOffsets.count - 1, to: 0, by: -1) {
            var i = 0
            var done = false
            while !done {
                if i >= fileOffsets[file] {
                    done = true
                } else {
                    while workingCopy[i] != "." && i < fileOffsets[file] {
                        i += 1
                    }
                    let space = getSpace(workingCopy, i)
                    if space.count >= fileSizes[file] {
                        for j in i ..< i + fileSizes[file] {
                            workingCopy[j] = String(file)
                        }
                        for j in fileOffsets[file] ..< fileOffsets[file] + fileSizes[file] {
                            workingCopy[j] = "."
                        }
                        done = true
                    } else {
                        // Point i to the end of the space. Upon iterating this will make i find the next available space
                        i = space.endIndex
                    }
                }
            }
        }
        return workingCopy
    }

    func computeChecksum(_ data: [String]) -> Int {
        var checksum = 0
        for i in 0 ..< data.count {
            if data[i] != "." {
                checksum += i * Int(data[i])!
            }
        }
        return checksum
    }

    func part1() -> Any {
        var (data, _, _) = entities
        data = moveFiles(data)
        return computeChecksum(data)
    }

    func part2() -> Any {
        var (data, fileSizes, fileOffsets) = entities
        data = moveWholeFiles(data, fileSizes, fileOffsets)
        return computeChecksum(data)
    }
}
