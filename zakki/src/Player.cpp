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

JMove genmove(Game& game, int player) {
  vector<int> openRivers;
  for (size_t i = 0; i < game.map.rivers; i++) {
    if (game.owner[i] < 0)
      openRivers.push_back(i);
  }
  int score0 = game.score(player);
  cerr << "SCORE:" << score0 << endl;
  for (int p = 0; p < game.game.punters; p++) {
    int score = game.score(p);
    cerr << "SCORE(" << p << "):" << score << endl;
  }
  int maxScore = score0;
  int maxMove = 0;
  for (size_t i = 0; i < openRivers.size(); i++) {
    Game g2(game);
    auto& r = game.game.map.rivers[openRivers[i]];
    JMove m = JMove::claim(player, r.source, r.target);
    vector<JMove> ms;
    ms.push_back(m);
    g2.update(ms);
    //cerr << "OWNER:" << json(g2.owner) << endl;
    int score = g2.score(player);
    cerr << m.source << "->" << m.target << " SCORE:" << score << endl;
    if (score >= maxScore) {
      maxScore = score;
      maxMove = openRivers[i];
    }
  }

  if (openRivers.size() == 0) {
    return JMove::pass(player);
  } else {
    uniform_int_distribution<int> dist(0, openRivers.size() - 1);
    //int n = openRivers[dist(mt)];
    int n = maxMove;
    auto& r = game.game.map.rivers[n];
    return JMove::claim(player, r.source, r.target);
  }
}
