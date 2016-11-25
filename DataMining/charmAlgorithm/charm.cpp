#include <iostream>
#include <algorithm>
#include <vector>
#include <set>
#include <map>
#include <string>
#include <sstream>
#include <cstdio>

using namespace std;

#define mp make_pair
#define pb push_back

map<string, int> stringToInt;
map<int, string> intToString;
vector<pair<set<int>, set<int> > > close;

int minsup = 3;

void charm(vector<pair<int, pair<set<int>, set<int> > > > pVector){
    sort(pVector.begin(), pVector.end());
    for(int i=0; i<pVector.size(); i++){
        vector<pair<int, pair<set<int>, set<int> > > > pi;
        for(int j=i+1; j<pVector.size(); j++){
            vector<int> comXVector;
            set_union(pVector[i].second.first.begin(), pVector[i].second.first.end(),
                            pVector[j].second.first.begin(), pVector[j].second.first.end(), back_inserter(comXVector));
            set<int> comX(comXVector.begin(), comXVector.end());

            vector<int> comTVector;
            set_intersection(pVector[i].second.second.begin(), pVector[i].second.second.end(),
                            pVector[j].second.second.begin(), pVector[j].second.second.end(), back_inserter(comTVector));
            set<int> comT(comTVector.begin(), comTVector.end());

            if(comT.size() >= minsup){
                if(pVector[i].second.second == pVector[j].second.second){
                    // replace Xi in P
                    set<int> Xi = pVector[i].second.first;
                    for(int r=0; r<pVector.size(); r++){
                        if(pVector[r].second.first == Xi){
                            pVector[r].second.first = comX;
                        }
                    }
                    for(int r=0; r<pi.size(); r++){
                        if(pi[r].second.first == Xi){
                            pi[r].second.first = comX;
                        }
                    }

                    // remove from pVector
                    pVector.erase(pVector.begin() + j);
					j--;
                }
                else{
                    set<int> txJ = pVector[j].second.second;
                    set<int> tXi = pVector[i].second.second;
                    if(includes(txJ.begin(), txJ.end(), tXi.begin(), tXi.end()) && !(tXi != txJ)){
                        set<int> Xi = pVector[i].second.first;
                        for(int r=0; r<pVector.size(); r++){
                            if(pVector[r].second.first == Xi){
                                pVector[r].second.first = comX;
                            }
                        }
                        for(int r=0; r<pi.size(); r++){
                            if(pi[r].second.first == Xi){
                                pi[r].second.first = comX;
                            }
                        }
                    }
                    else{
                        pi.pb(mp(comT.size(), mp(comX, comT)));
                    }
                }
            }
        }

        if(pi.size() > 0){
            charm(pi);
        }

        bool ok = true;
        for(int r=0; r<close.size(); r++){
            set<int> z = close[r].first;
            set<int> xi = pVector[i].second.first;
            if(includes(z.begin(), z.end(), xi.begin(), xi.end())){
                if(pVector[i].second.second == close[r].second){
                    ok = false;
                    break;
                }
            }
        }
        if(ok){
            close.pb(mp(pVector[i].second.first, pVector[i].second.second));
        }
    }
}

int main(){
    freopen("test.txt", "r", stdin);
    freopen("test_out.txt", "w", stdout);

    map<string, set<int> > string2Int;
    map<string, bool> used;

    string s;
    int ind = 1;
    while(getline(cin, s)){
        istringstream iss(s);
        vector<string> tokens;
        copy(istream_iterator<string>(iss),
             istream_iterator<string>(),
             back_inserter(tokens));
        for(int i=0; i<tokens.size(); i++){
            if(!used[tokens[i]]){
                used[tokens[i]] = true;
                stringToInt[tokens[i]] = stringToInt.size();
                intToString[stringToInt[tokens[i]]] = tokens[i];
            }
            string2Int[tokens[i]].insert(ind);
        }
        ind++;
    }

    vector<pair<int, pair<set<int>, set<int> > > > pVector;
    for(auto i: string2Int){
        if(i.second.size() < minsup) continue;
        set<int> tmp;
        tmp.insert(stringToInt[i.first]);
        pVector.pb(mp(i.second.size(), mp(tmp, i.second)));
    }

    charm(pVector);

    for(auto i:close){
        for(auto j:i.first){
            cout << intToString[j] << " ";
        }
        cout << " - ";
        for(auto j:i.second){
            cout << j << " ";
        }
        cout << endl;
    }

    return 0;
}
