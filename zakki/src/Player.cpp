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

struct Node;

struct Child {
  int river;
  int win;
  int move_count;
  bool open;
  shared_ptr<Node> node;

  explicit Child(int r) :
    river(r), win(0), move_count(0), open(false) {
  }
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
  auto& map = game.map;
  vector<int> openRivers;
  for (size_t i = 0; i < game.map.rivers; i++) {
    if (game.owner[i] < 0) {
      openRivers.push_back(i);
      // for (int mine : map.mines) {
      //   for (int site : map.sites) {
      //     if (map.connection(mine, site)) {
      //       int rid = map.riverId(mine, target);
      //     }
      //   }
      // }
    }
  }
  Node node;
  node.children.reserve(openRivers.size());
  for (int river : openRivers) {
    auto c = make_shared<Child>(river);
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

shared_ptr<Child> maxChild(Node& node) {
  double sum = node.move_count;
  shared_ptr<Child> max_child;
  double max_value = -1;
  for (auto c : node.children) {
    // if (!c.open) continue;
    double ucb_value;
    if (c->move_count == 0) {
      ucb_value = 10.0;
    } else {
      double p = (double) c->win / c->move_count;
      ucb_value = p + sqrt(2 * log(sum) / c->move_count);
    }
    if (ucb_value > max_value) {
      max_value = ucb_value;
      max_child = c;
    }
  }
  return max_child;
}

int search(Game& game, Node& node, int player) {
  int maxScore = -1;
  int maxMove = -1;
  for (size_t i = 0; i < node.children.size(); i++) {
    auto c = node.children[i];
    Game g = game;
    claimRiver(g, c->river, player);
    int score = g.score(player);
    cerr << c->river << " SCORE:" << score << endl;
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
