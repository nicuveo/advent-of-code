#include <vector>
#include <iostream>
#include <algorithm>

#include "aoc.hh"

const std::vector<std::string> example =
  { "1000",
    "2000",
    "3000",
    "",
    "4000",
    "",
    "5000",
    "6000",
    "",
    "7000",
    "8000",
    "9000",
    "",
    "10000"
  };

void part1(const std::vector<std::string>& lines)
{
  std::vector<int> calories;
  int current = 0;

  for (auto const& line : lines)
  {
    if (line.empty())
    {
      calories.push_back(current);
      current = 0;
      continue;
    }
    current += std::stoi(line);
  }
  calories.push_back(current);

  std::cout << "part1: " << *std::max_element(calories.begin(), calories.end()) << std::endl;
}

int main()
{
  auto input = aoc::get_input_lines(1);
  part1(example);
  part1(input);
}
