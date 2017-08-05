#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <random>
#include <memory>

#include "json.hpp"

#include "Game.hpp"

using namespace std;
// for convenience
using json = nlohmann::json;

std::mt19937 mt;

void initPlayer() {
  std::random_device rnd;
  mt.seed(rnd());
}

JMove genmove(const Game& game, int player) {
  vector<int> openRivers;
  for (size_t i = 0; i < game.map.rivers; i++) {
    if (game.owner[i] < 0)
      openRivers.push_back(i);
  }
  if (openRivers.size() == 0) {
    return JMove::pass(player);
  } else {
    uniform_int_distribution<int> dist(0, openRivers.size() - 1);
    int n = openRivers[dist(mt)];
    auto& r = game.game.map.rivers[n];
    return JMove::claim(player, r.source, r.target);
  }
}
