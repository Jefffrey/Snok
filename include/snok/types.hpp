#pragma once

#include <aid/matrix.hpp>

namespace snok {

    template<typename Type, std::size_t N>
    using vector = aid::col_vector<Type, N>;
    using size = vector<int, 2>;

}
