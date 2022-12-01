#include <fstream>
#include <sstream>
#include <iomanip>

#include "aoc.hh"

namespace aoc
{

  std::ifstream
  get_file_stream(int day)
  {
    std::stringstream filename;
    filename << "../input/" << std::setw(2) << std::setfill('0') << day << ".in";
    return std::ifstream(filename.str());
  }

  std::string
  get_input_string(int day)
  {
    std::ifstream content = get_file_stream(day);
    std::stringstream buffer;

    buffer << content.rdbuf();
    return buffer.str();
  }

  std::vector<std::string>
  get_input_lines(int day)
  {
    std::ifstream content = get_file_stream(day);
    std::string line;
    std::vector<std::string> result;

    while (std::getline(content, line))
      result.push_back(line);

    return result;
  }

}
