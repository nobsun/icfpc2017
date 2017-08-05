#include <fstream>
#include <iostream>
#include <thread>
#include <string>
#include <vector>
#include "json.hpp"

#include "Game.hpp"

using namespace std;
// for convenience
using json = nlohmann::json;

json recvMessage() {
  int n;
  char c;
  cin >> n >> c;
  cerr << n << c << endl;
  if (c != ':')
    throw invalid_argument("bad prefix");
  std::string buf(n + 1, '\0');
  cin.read(&buf[0], n);
  return json::parse(buf);
}

void sendMessage(json json) {
  string str = json.dump();
  int n = str.size();
  cout << n << ":" << str << endl;
}

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

  int distance(int from, int to) {
    const int no_route = numeric_limits<int>::max();
    const int size = sites.size();
    vector<int> dist(sites.size(), no_route);
    dist[from] = 0;
    for (int n = 0; n < size; n++) {
      for (int i = 0; i < size; i++) {
        if (dist[i] == no_route)
          continue;
        for (int j = 0; j < size; j++) {
          if (connected(i, j)) {
            int d = dist[i] + 1;
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
};

JMap parseMap(json map) {
  JMap m;

  auto sites = map.at("sites");
  cerr << sites << endl;
  assert(sites.is_array());

  for (auto it = sites.begin(); it != sites.end(); it++) {
    int id = it->at("id");
    cerr << "site " << id << " " << m.sites.size() << endl;
    m.sites.push_back(id);
  }
  sort(m.sites.begin(), m.sites.end());
  cerr << json(m.sites) << endl;

  auto rivers = map.at("rivers");
  cerr << rivers << endl;
  assert(rivers.is_array());
  for (auto it = rivers.begin(); it != rivers.end(); it++) {
    int s = it->at("source");
    int t = it->at("target");
    cerr << "river " << s << " -> " << t << endl;
    m.rivers.push_back(River(s, t));
  }

  auto mines = map.at("mines");
  cerr << mines << endl;
  assert(mines.is_array());
  m.mines = mines.get<std::vector<int>>();
  cerr << json(m.mines) << endl;

  return m;
}

JMove parseMove(json jmove) {
  auto pass = jmove.find("pass");
  if (pass != jmove.end()) {
    int punter = pass->at("punter");
    return JMove::pass(punter);
  }
  auto claim = jmove.find("claim");
  if (claim != jmove.end()) {
    int punter = claim->at("punter");
    int source = claim->at("source");
    int target = claim->at("target");
    return JMove::claim(punter, source, target);
  }
  throw invalid_argument("bad move");
}

vector<JMove> parseMoves(json jmove) {
  vector<JMove> moves;
  auto jmoves = jmove.at("moves");
  cerr << jmoves << endl;
  for (auto it = jmoves.begin(); it != jmoves.end(); ++it) {
    cerr << *it << endl;
    moves.push_back(parseMove(*it));
  }
  return moves;
}

JGame parseGame(json jsgame) {
  JGame game;
  auto jsmap = jsgame.at("map");
  cerr << jsmap.dump() << endl;
  game.map = parseMap(jsmap);
  cerr << "map" << endl;
  game.punter = jsgame.at("punter");
  cerr << game.punter << endl;
  game.punters = jsgame.at("punters");
  cerr << game.punters << endl;
  return game;
}

void handshake()
{  json startMsg;
  startMsg["me"] = "SamBob";
  sendMessage(startMsg);
}

void loop() {
  json msg;

  msg = recvMessage();
  cerr << msg << endl;
  msg = recvMessage();
  JGame jgame = parseGame(msg);
  Game game(jgame);

  json readyMsg;
  readyMsg["ready"] = game.game.punter;
  sendMessage(readyMsg);

  while (true) {
    json msg = recvMessage();
    cerr << msg << endl;
    auto jmove = msg.find("move");
    if (jmove != msg.end()) {
      auto ms = parseMoves(*jmove);
      cerr << "moves " << ms.size() << endl;
      game.update(ms);
      continue;
    }
    auto jstop = msg.find("stop");
    if (jstop != msg.end()) {
      break;
    }
  }
}

int main() {
#if 1
  try {
    handshake();
    loop();
  } catch (exception ex) {
    cerr << ex.what() << endl;
  }
  return 0;
#else
  string game_str = "{\"punter\":0,"
    "\"punters\":2,"
    "\"map\":{\"sites\":[{\"id\":4},{\"id\":1},{\"id\":3},{\"id\":6},{\"id\":5},{\"id\":0},{\"id\":7},{\"id\":2}],"
    "\"rivers\":[{\"source\":3,\"target\":4},{\"source\":0,\"target\":1},{\"source\":2,\"target\":3},"
    "{\"source\":1,\"target\":3},{\"source\":5,\"target\":6},{\"source\":4,\"target\":5},"
    "{\"source\":3,\"target\":5},{\"source\":6,\"target\":7},{\"source\":5,\"target\":7},"
    "{\"source\":1,\"target\":7},{\"source\":0,\"target\":7},{\"source\":1,\"target\":2}],"
    "\"mines\":[1,5]}}";
  cerr << game_str << endl;
  auto jsgame = json::parse(game_str);
  cerr << jsgame.dump() << endl;
  auto jsmap = jsgame["map"];
  cerr << jsmap.dump() << endl;
  auto jmap = parseMap(jsmap);

  string move_str = "{\"move\":{\"moves\":[{\"pass\":{\"punter\":0}},{\"pass\":{\"punter\":1}}]}}";
  auto jmove = json::parse(move_str);
  cerr << jmove << endl;
  string move_str2 = "{\"move\":{\"moves\":[{\"claim\":{\"punter\":0,\"source\":0,\"target\":1}},{\"claim\":{\"punter\":1,\"source\":1,\"target\":2}}]}}";
  auto jmove2 = json::parse(move_str2);
  cerr << jmove2 << endl;

  Map m{jmap};
  cerr << m.distance(1, 5) << endl;
#endif
}
