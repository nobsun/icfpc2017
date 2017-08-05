#pragma once
#include <fstream>
#include <iostream>
#include <thread>
#include <string>
#include <vector>

#include "json.hpp"

using namespace std;
// for convenience
using json = nlohmann::json;

json recvMessage();
void sendMessage(json json);
