#include <iostream>
#include <cassert>
#include <numeric>
#include <fstream>
#include <algorithm>
#include <string>
#include <map>
#include <string_view>
#include <vector>

class File
{
public:
    std::string name;
    std::size_t size = 0;
};

class Directory
{
public:
    std::string name;
    std::map<std::string, Directory> directories;
    std::map<std::string, File> files;
    Directory *parent = nullptr;
    std::size_t size = 0;
};

auto calculate_directory_sizes(Directory &directory) -> std::size_t
{
    const auto sizeOfFiles = std::accumulate(directory.files.cbegin(), directory.files.cend(), 0ULL, [](const auto &currentSum, const auto &currentFileKvp)
                                             { return currentSum + currentFileKvp.second.size; });
    const auto sizeOfDirectories = std::accumulate(directory.directories.begin(), directory.directories.end(), 0ULL, [](const auto &currentSum, auto &currentDirectoryKvp)
                                                   { return currentSum + calculate_directory_sizes(currentDirectoryKvp.second); });
    directory.size = sizeOfFiles + sizeOfDirectories;
    return directory.size;
}

auto parse_input(const std::string &filename) -> Directory
{
    auto inputFile = std::ifstream(filename);

    Directory root;
    Directory *currentDirectory = &root;

    std::string line;
    while (std::getline(inputFile, line))
    {
        if (line.substr(0, 6) == "$ cd /")
        {
            currentDirectory = &root;
        }
        else if (line.substr(0, 7) == "$ cd ..")
        {
            currentDirectory = currentDirectory->parent;
        }
        else if (line.substr(0, 5) == "$ cd ")
        {
            const auto newDirectoryName = line.substr(5);
            auto &newDirectory = currentDirectory->directories[newDirectoryName];
            newDirectory.parent = currentDirectory;
            newDirectory.name = newDirectoryName;
            currentDirectory = &currentDirectory->directories[newDirectoryName];
        }
        else if (line.substr(0, 4) == "$ ls")
        {
            // do nothing.
        }
        else if (line.substr(0, 4) == "dir ")
        {
            // do nothing.
        }
        else
        {
            const auto splitIndex = line.find_first_of(' ');
            const auto size = std::stoull(line);
            const auto directoryName = line.substr(splitIndex + 1);
            currentDirectory->files[directoryName] = File{directoryName, size};
        }
    }

    calculate_directory_sizes(root);

    return root;
}

auto part1(const Directory &directory)
{
    const auto initialSum = directory.size <= 100'000 ? directory.size : 0;

    const auto sizeOfSmallDirectories = std::accumulate(directory.directories.cbegin(), directory.directories.cend(), 0ULL, [](const auto &currentSum, auto &currentDirectoryKvp)
                                                        { return currentSum + part1(currentDirectoryKvp.second); });
    return initialSum + sizeOfSmallDirectories;
}

auto part2(const Directory &directory, const std::size_t rootSize, std::size_t currentBest) -> std::size_t
{
    const auto availableSpace = 70'000'000 - rootSize;
    const auto spaceToFreeUp = 30'000'000 - availableSpace;

    if (directory.size >= spaceToFreeUp && directory.size < currentBest)
    {
        currentBest = directory.size;
    }

    for (const auto &childDir : directory.directories)
    {
        currentBest = part2(childDir.second, rootSize, currentBest);
    }

    return currentBest;
}

auto main() -> int
{
    const auto testInputFileName = "testinput.txt";
    const auto inputFile = "input.txt";

    const auto testParsed = parse_input(testInputFileName);
    const auto parsed = parse_input(inputFile);

    const auto part1TestAnswer = part1(testParsed);
    const auto part1Answer = part1(parsed);
    assert(part1TestAnswer == 95437);
    assert(part1Answer == 1390824);
    std::cout << part1TestAnswer << '\n';
    std::cout << part1Answer << '\n';

    const auto part2TestAnswer = part2(testParsed, testParsed.size, testParsed.size);
    const auto part2Answer = part2(parsed, parsed.size, parsed.size);
    assert(part2TestAnswer == 24933642);
    assert(part2Answer == 7490863);

    std::cout << part2TestAnswer << '\n';
    std::cout << part2Answer << '\n';
}