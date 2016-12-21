#include "media-service.h"
#include <iostream>

using namespace std;

int main()
{
  cout << "Home System Media Server" << endl;
  try
  {
    home_system::media::media_service srv;

    cout << "Enter q to quit..." << endl;
    std::string input_line;
    while (std::getline(std::cin, input_line))
    {
      if (input_line == "q" || input_line == "quit")
      {
        break;
      }
    }
  }  catch (const exception& e)
  {
    cout << "Exception: " << e.what() << endl;
  }  catch (...)
  {
    cout << "Unknown Exception" << endl;
  }
  return 0;
}

