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



struct River {
  int source;
  int target;
  River(int s, int t) : source(s), target(t) {
  }
};

struct JMap {
  vector<int> sites;
  vector<River> rivers;
  vector<int> mines;
};

struct JMove {
  int punter;
  int source;
  int target;
  JMove(int p, int s, int t)
    : punter(p), source(s), target(t) {
  }

  bool isPass() const {
    return source < 0;
  }

  static JMove pass(int p) {
    return JMove(p, -1, -1);
  }

  static JMove claim(int p, int s, int t) {
    return JMove(p, s, t);
  }
};

struct JGame {
  JMap map;
  int punter;
  int punters;
};

const int no_route = numeric_limits<int>::max();

class Map {
public:
  const vector<int> sites;
  const vector<int> mines;
  vector<int> riverIndex;
  vector<bool> connection;
  mutable vector<int> distanceCache;
  const size_t rivers;

  explicit Map(const JMap& m);

  int riverId(int source, int target) const {
    int size = sites[sites.size() - 1] + 1;
    int index = riverIndex[source * size + target];
    if (index < 0) throw invalid_argument("bad river");
    return index;
  }

  bool connected(int from, int to) const {
    int size = sites[sites.size() - 1] + 1;
    return connection[from * size + to];
  }

  int distance(int from, int to) const;
};

class Game {
public:
  const JGame game;
  const Map map;
  vector<int> owner;

  explicit Game(const JGame& g) :
    game(g), map(g.map) {
    //cerr << "new Game" << endl;
    //cerr << map.rivers << endl;
    owner.resize(map.rivers);
    fill(owner.begin(), owner.end(), -1);
    // for (size_t i = 0; i < map.rivers; i++) {
    //   cerr << i << endl;
    //   owner.push_back(-1);
    // }
    //cerr << "ok" << endl;
  }

  void update(const vector<JMove>& moves) {
    for (auto m : moves) {
      if (m.isPass()) {
        //cerr << "PASS " << m.punter << endl;
      } else {
        int id = map.riverId(m.source, m.target);
        //cerr << "CLAIM " << m.punter << ":" << m.source << "-" << m.target << ":" << id << endl;
        if (owner[id] >= 0 && owner[id] != m.punter)
          cerr << "??? OWNED " << owner[id] << "<>" << m.punter << endl;
        owner[id] = m.punter;
      }
    }
  }
  
  bool hasRoute(int from, int to, int punter);
  int score(int punter);
  vector<int> route(int from, int punter);
};

