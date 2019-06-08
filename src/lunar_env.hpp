#ifndef LUNAR_ENV
#define LUNAR_ENV

#include <string>
#include <vector>

#include <boost/filesystem.hpp>

namespace lunar {

namespace fs = boost::filesystem;

class lunar_env {
  public:
    lunar_env() {}
    lunar_env(const lunar_env &env) : m_paths(env.m_paths) {}
    virtual ~lunar_env() {}

    void add(const char *path);
    void add(const fs::path &path) { m_paths.push_back(path); }

    // translate a module name to the corresponding absolute path
    // e.g.
    //   If "name" is foo.bar then
    //   "/home/user/foo/bar.lunar" is returned
    //   only if m_paths includes "/home/user" and the file exists.
    //   if there is no such file, empty path is returned
    fs::path get_module_path(std::string &name);

  private:
    std::vector<fs::path> m_paths;
};

} // namespace lunar

#endif // LUNAR_ENV