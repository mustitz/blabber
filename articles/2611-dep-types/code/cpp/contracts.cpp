#include <algorithm>
#include <string>
#include <vector>

static bool pre_failed = false;
static bool post_failed = false;

int nth(const std::vector<int>& v, size_t n)
    pre(n < v.size())
{
    return v[n];
}

void sort(std::vector<int>& v)
    post(std::is_sorted(v.begin(), v.end()))
{
    std::sort(v.begin(), v.end());
    if (post_failed) {
        std::swap(v.front(), v.back());
    }
}

int main(int argc, char** argv) {
    std::string mode = std::string(argc > 1 ? argv[1] : "ok");
    pre_failed = mode == "pre";
    post_failed = mode == "post";

    std::vector<int> v{5, 3, 1, 4, 2};
    sort(v);

    size_t n = pre_failed ? 7 : 2;
    return nth(v, n);
}
