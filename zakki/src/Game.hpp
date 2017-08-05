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

class Map {
public:
  const vector<int> sites;
  const vector<int> mines;
  vector<int> riverIndex;
  vector<bool> connection;
  const size_t rivers;

  Map(const JMap& m)
    : sites(m.sites),
      mines(m.mines),
      rivers(m.rivers.size()) {
    int size = sites.size();
    connection.resize(size * size);
    riverIndex.resize(size * size);
    fill(riverIndex.begin(), riverIndex.end(), -1);
    for (size_t i = 0; i < m.rivers.size(); i++) {
      auto r = m.rivers[i];
      connection[r.source * size  + r.target] = true;
      connection[r.target * size  + r.source] = true;
      riverIndex[r.source * size  + r.target] = i;
      riverIndex[r.target * size  + r.source] = i;
    }
  }

  int riverId(int source, int target) const {
    return riverIndex[source * sites.size() + target];
  }

  bool connected(int from, int to) const {
    return connection[from * sites.size() + to];
  }

  int distance(int from, int to) const {
    const int no_route = numeric_limits<int>::max();
    const int size = sites.size();
    vector<int> dist(sites.size(), no_route);
    dist[from] = 0;
    for (int n = 0; n < size; n++) {
      for (int i = 0; i < size; i++) {
        if (dist[i] == no_route)
          continue;
        for (int j = 0; j < size; j++) {
          if (i == j || dist[j] < no_route)
            continue;
          if (connected(i, j)) {
            int d = dist[i] + 1;
            //cerr << "distance:" << i << "->" << j << ":" << d << endl;
            if (j == to)
              return d;
            if (dist[j] > d) {
              dist[j] = d;
            }
          }
        }
      }
    }
    if (dist[to] != no_route)
      return dist[to];
    return 0;
  }
};

class Game {
public:
  const JGame game;
  const Map map;
  vector<int> owner;

  Game(const JGame& g) :
    game(g), map(g.map) {
    for (size_t i = 0; i < map.rivers; i++) {
      owner.push_back(-1);
    }
  }

  void update(const vector<JMove>& moves) {
    for (auto m : moves) {
      if (m.isPass()) {
        cerr << "PASS " << m.punter << endl;
      } else {
        int id = map.riverId(m.source, m.target);
        cerr << "CLAIM " << m.punter << ":" << id << endl;
        if (owner[id] >= 0 && owner[id] != m.punter)
          cerr << "??? OWNED " << owner[id] << "<>" << m.punter << endl;
        owner[id] = m.punter;
      }
    }
  }
  
  bool hasRoute(int from, int to, int punter);
  int score(int punter);
};

