#include <list>
#include <string>
#include <unordered_map>
inline std::list<std::string> Values;
inline std::unordered_map<std::string, std::string*> Index_Map;
class LightStrings{
    
    public:
    static std::string& get(std::string key){
      if(Index_Map.find(key) != Index_Map.end()){
        return *Index_Map[key];
      } else {
        Values.push_back(key);
        Index_Map[key] = &Values.back();
        return Values.back();
      }
    }
};