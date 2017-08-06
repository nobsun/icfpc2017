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

struct Child {
  int river;
};

struct Node {
  vector<shared_ptr<Child>> children;
  int win;
  int move_count;
};

void initPlayer() {
  std::random_device rnd;
  mt.seed(rnd());
}

Node expandNode(Game& game, int player) {
  vector<int> openRivers;
  for (size_t i = 0; i < game.map.rivers; i++) {
    if (game.owner[i] < 0)
      openRivers.push_back(i);
  }
  Node node;
  node.children.reserve(openRivers.size());
  for (int river : openRivers) {
    auto c = make_shared<Child>();
    c->river = river;
    node.children.push_back(c);
  }
  return node;
}

void claimRiver(Game& game, int river, int player) {
    auto& r = game.game.map.rivers[river];
    JMove m = JMove::claim(player, r.source, r.target);
    vector<JMove> ms;
    ms.push_back(m);
    game.update(ms);
}

int search(Game& game, Node& node, int player) {
  int maxScore = -1;
  int maxMove = -1;
  for (size_t i = 0; i < node.children.size(); i++) {
    auto c = node.children[i];
    Game g = game;
    claimRiver(g, c->river, player);
    int score = g.score(player);
    if (score > maxScore) {
      maxScore = score;
      maxMove = c->river;
    }
  }
  return maxMove;
}

JMove genmove(Game& game, int player) {
  Node root = expandNode(game, player);
  int score0 = game.score(player);
  cerr << "SCORE:" << score0 << endl;
  for (int p = 0; p < game.game.punters; p++) {
    int score = game.score(p);
    cerr << "SCORE(" << p << "):" << score << endl;
  }
  int maxScore = score0;
  int maxMove = search(game, root, player);

  if (maxMove < 0) {
    return JMove::pass(player);
  } else {
    //uniform_int_distribution<int> dist(0, openRivers.size() - 1);
    //int n = openRivers[dist(mt)];
    int n = maxMove;
    auto& r = game.game.map.rivers[n];
    return JMove::claim(player, r.source, r.target);
  }
}
