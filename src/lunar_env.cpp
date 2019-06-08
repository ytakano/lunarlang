#include "lunar_env.hpp"

#include <boost/algorithm/string.hpp>

namespace lunar {

void lunar_env::add(const char *path) {
    auto p = fs::path(path);
    p.remove_filename();
    p = fs::absolute(p);
    m_paths.push_back(p);
}

fs::path lunar_env::get_module_path(std::string &name) {
    std::vector<std::string> strs;
    boost::split(strs, name, boost::is_any_of("."));

    strs[strs.size() - 1] += ".lunar";

    fs::path p;
    for (auto &s : strs) {
        p /= fs::path(s.c_str());
    }

    for (auto &fp : m_paths) {
        auto path = fp / p;
        if (fs::exists(path)) {
            auto st = fs::status(path);
            if (st.type() == fs::regular_file || st.type() == fs::symlink_file)
                return path;
        }
    }

    return fs::path();
}

} // namespace lunar