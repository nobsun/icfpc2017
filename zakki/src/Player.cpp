#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <random>
#include <memory>
#include <chrono>

#include "json.hpp"

#include "Game.hpp"

using namespace std;
// for convenience
using json = nlohmann::json;
using my_clock =std::chrono::high_resolution_clock;

std::mt19937 mt;

struct Node;

struct Child {
  int river;
  int rate;
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

void sortChild(Game& game, int punter, shared_ptr<Node>& node) {
  auto& map = game.map;
  for (auto& c : node->children) {
    c->rate = 0;
  }
  for (auto& c : node->children) {
    auto& r = game.game.map.rivers[c->river];
    for (int mine : map.mines) {
      if (r.source == mine || r.target == mine) {
        c->rate += 1;
      }
    }
    for (int rid = 0; rid < game.owner.size(); rid++) {
      if (game.owner[rid] != punter)
        continue;
      auto& r2 = game.game.map.rivers[rid];
      if (r2.source == r.source || r2.source == r.target
          || r2.target == r.source || r2.target == r.target) {
        c->rate += 1;
        break;
      }
    }
  }
/*
      for (int i = 0; i < 2; i++) {
        int source = i == 0 ? r.source : r.target;
        for (int site : map.sites) {
          if (source == site)
            continue;
          if (map.connected(source, site)) {
            int rid = map.riverId(source, site);
            if (game.owner[rid] == punter) {
              c->rate += 1;
            }
          }
        }
      }
*/
  sort(node->children.begin(), node->children.end(), [](shared_ptr<Child>& a, shared_ptr<Child>& b) {
      return a->rate > b->rate;
    });
  // cerr << "sort" << endl;
  // for (auto& c : node.children) {
  //   cerr << c->river << ":" << c->rate << endl;
  // }
}

shared_ptr<Node> expandNode(Game& game, int punter) {
  vector<int> openRivers;
  for (size_t i = 0; i < game.map.rivers; i++) {
    if (game.owner[i] < 0) {
      openRivers.push_back(i);
    }
  }
  auto node = make_shared<Node>();
  node->children.reserve(openRivers.size());
  for (int river : openRivers) {
    auto c = make_shared<Child>(river);
    node->children.push_back(c);
  }
  sortChild(game, punter, node);
  return node;
}

void claimRiver(Game& game, int river, int punter) {
  auto& r = game.game.map.rivers[river];
  JMove m = JMove::claim(punter, r.source, r.target);
  vector<JMove> ms;
  ms.push_back(m);
  game.update(ms);
}

shared_ptr<Child> maxChild(shared_ptr<Node>& node) {
  double sum = node->move_count;
  shared_ptr<Child> max_child;
  double max_value = -1;
  for (auto c : node->children) {
     if (!c->open) continue;
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

void simulate(Game& game, int punter, std::vector<int>& rank) {
  const int punters = game.game.punters;
  //size_t end_turn = min(game.turn + punters * 4, (int) game.map.rivers);
  size_t end_turn = game.map.rivers;
  cerr << "SIMULATE " << game.turn << "/" << end_turn<< endl;
  for (size_t turn = game.turn; turn < end_turn; turn++) {
    //cerr << "TURN#" << turn << endl;
    auto node = expandNode(game, punter);
    if (node->children.size() == 0)
      break;
    int sum = 0;
    for (auto c : node->children) {
      sum += c->rate + 1;
    }
    uniform_int_distribution<int> dist(1, sum + 1);
    int rnd = dist(mt);

    for (auto c : node->children) {
      rnd -= c->rate + 1;
      if (rnd <= 0) {
        //cerr << punter << ":" << c->river << endl;
        claimRiver(game, c->river, punter);
        break;
      }
    }

    punter = (punter + 1) % punters;
  }

  vector<int> score;
  for (int i = 0; i < punters; i++) {
    score.push_back(game.score(i));
    rank[i] = i;
  }
  vector<int> sorted(score);
  sort(sorted.begin(), sorted.end());
  // sort(rank.begin(), rank.end(), [&](int a, int b) {
  //     return score[rank[a]] < score[rank[b]];
  //   });
  for (int i = 0; i < punters; i++) {
    rank[i] = find(sorted.begin(), sorted.end(), score[i]) - sorted.begin();
  }
  for (int i = 0; i < punters; i++) {
    cerr << "PUNTER#" << i << " SCORE:" << score[i] << " RANK:" << rank[i] << endl;
  }
}

void search(Game& game, shared_ptr<Node>& node, int punter, std::vector<int>& rank) {
  //int maxScore = -1;
  //int maxMove = -1;
  if (node->move_count % 128 == 0) {
    int pw = log((node->move_count) / 40+ 1) + 2;
    cerr << "open " << pw << "/" << node->children.size() << endl;
    if (pw > node->children.size())
      pw = node->children.size();
    for (size_t i = 0; i < pw; i++) {
      node->children[i]->open = true;
    }
  }
  auto c = maxChild(node);
  if (!c) {
    int next_punter = (punter + 1) % game.game.punters;
    simulate(game, next_punter, rank);
    node->move_count++;
    node->win += rank[punter];
    return;
  }

  claimRiver(game, c->river, punter);
  int next_punter = (punter + 1) % game.game.punters;

  bool end_of_game = game.turn >= game.map.rivers + game.game.punters;
  if (c->move_count < 10 || end_of_game) {
    simulate(game, next_punter, rank);
  } else {
    if (!c->node) {
      c->node = expandNode(game, next_punter);
    }
    search(game, c->node, next_punter, rank);
  }
  node->move_count++;
  node->win += rank[punter];
  c->move_count++;
  c->win += rank[punter];
/*
  for (size_t i = 0; i < node.children.size(); i++) {
    auto c = node.children[i];
    Game g = game;
    claimRiver(g, c->river, punter);
    int score = g.score(punter);
    cerr << c->river << " SCORE:" << score << endl;
    if (score > maxScore) {
      maxScore = score;
      maxMove = c->river;
    }
  }
*/
}

JMove genmove(Game& game, int player) {
  auto start_time = my_clock::now();
  auto root = expandNode(game, player);
  const int punters = game.game.punters;
  int score0 = game.score(player);
  cerr << "SCORE:" << score0 << endl;
  for (int p = 0; p < punters; p++) {
    int score = game.score(p);
    cerr << "SCORE(" << p << "):" << score << endl;
  }
  //int maxScore = score0;
  for (size_t i = 0; i < 100000; i++) {
    std::vector<int> rank(punters, -1);
    Game g(game);
    search(g, root, player, rank);
    auto t = chrono::duration_cast<chrono::milliseconds>(my_clock::now() - start_time).count();
    if (t > 500) {
      cerr << i << "PO " << (i * 1000.0 / t) << "PO/sec" << endl;
      break;
    }
  }

  int maxCount = -1;
  int maxMove = -1;
  cerr << "DONE" << endl;
  for (auto c : root->children) {
    cerr << c->river << ":" << (double)c->win / c->move_count << "/" << c->move_count
         << " " << c->open
         << endl;
    if (c->move_count > maxCount) {
      maxCount = c->move_count;
      maxMove = c->river;
    }
  }

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
