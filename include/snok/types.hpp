#pragma once

#include <boost/numeric/ublas/vector.hpp>

namespace snok {

    namespace {
        namespace ublas = boost::numeric::ublas;
    }

    template<typename Type, std::size_t N>
    using vector = ublas::c_vector<Type, N>;

    using size = vector<int, 2>;

}
