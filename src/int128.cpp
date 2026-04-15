#include "int128.hpp"
#include <algorithm>
#include <cmath>

namespace {
    constexpr double POW2_32 = 4294967296.0;
    constexpr double POW2_64 = 18446744073709551616.0;
    constexpr double POW2_96 = 79228162514264337593543950336.0;
}

Int128::Int128(uint64_t low, int64_t high) : low_(low), high_(high) {}

Int128::Int128() : low_(0), high_(0) {}

Int128::Int128(int64_t value) : low_(static_cast<uint64_t>(value)), high_(value < 0 ? -1 : 0) {}

Int128::Int128(std::string_view str) : low_(0), high_(0) {
    bool negative = false;
    size_t start = 0;

    if (str[0] == '-') {
        negative = true;
        start = 1;
    }

    for (size_t i = start; i < str.size(); ++i) {
        *this = *this * Int128(10) + Int128(static_cast<int64_t>(str[i] - '0'));
    }

    if (negative) {
        *this = -(*this);
    }
}

Int128::operator int64_t() const {
    return static_cast<int64_t>(low_);
}

Int128::operator double() const {
    if (high_ >= 0) {
        uint32_t high_high = static_cast<uint32_t>(static_cast<uint64_t>(high_) >> 32);
        uint32_t high_low = static_cast<uint32_t>(static_cast<uint64_t>(high_) & 0xFFFFFFFF);
        uint32_t low_high = static_cast<uint32_t>(low_ >> 32);
        uint32_t low_low = static_cast<uint32_t>(low_ & 0xFFFFFFFF);

        double result = 0.0;
        result += static_cast<double>(high_high) * POW2_96;
        result += static_cast<double>(high_low) * POW2_64;
        result += static_cast<double>(low_high) * POW2_32;
        result += static_cast<double>(low_low);
        return result;
    } else {
        return -static_cast<double>(-*this);
    }
}

std::string Int128::str() const {
    if (high_ == 0 && low_ == 0) return "0";

    uint64_t temp_low = low_;
    int64_t temp_high = high_;
    bool negative = temp_high < 0;

    if (negative) {
        if (temp_low == 0) {
            temp_high = -temp_high;
        } else {
            temp_high = ~temp_high;
            temp_low = ~temp_low + 1;
        }
    }

    std::string result;
    uint32_t parts[4];
    parts[0] = static_cast<uint32_t>(temp_low & 0xFFFFFFFF);
    parts[1] = static_cast<uint32_t>(temp_low >> 32);
    parts[2] = static_cast<uint32_t>(static_cast<uint64_t>(temp_high) & 0xFFFFFFFF);
    parts[3] = static_cast<uint32_t>(static_cast<uint64_t>(temp_high) >> 32);

    while (parts[3] != 0 || parts[2] != 0 || parts[1] != 0 || parts[0] != 0) {
        uint64_t remainder = 0;
        for (int i = 3; i >= 0; --i) {
            uint64_t current = (remainder << 32) | parts[i];
            parts[i] = static_cast<uint32_t>(current / 10);
            remainder = current % 10;
        }
        result.push_back('0' + static_cast<char>(remainder));
    }

    std::reverse(result.begin(), result.end());
    if (negative) result = "-" + result;
    return result;
}

Int128 Int128::operator-() const {
    if (low_ == 0 && high_ == 0) return *this;
    uint64_t new_low = ~low_ + 1;
    int64_t new_high = ~high_;
    if (new_low == 0) new_high++;
    return Int128(new_low, new_high);
}

Int128 Int128::operator+(const Int128& other) const {
    uint64_t new_low = low_ + other.low_;
    int64_t new_high = high_ + other.high_;
    if (new_low < low_) new_high++;
    return Int128(new_low, new_high);
}

Int128& Int128::operator+=(const Int128& other) {
    *this = *this + other;
    return *this;
}

Int128 Int128::operator-(const Int128& other) const {
    return *this + (-other);
}

Int128& Int128::operator-=(const Int128& other) {
    *this = *this - other;
    return *this;
}

// Операторы сдвига
Int128 Int128::operator<<(int shift) const {
    if (shift == 0) return *this;
    if (shift >= 128) return Int128(0);
    
    uint64_t new_low = low_ << shift;
    uint64_t new_high = (static_cast<uint64_t>(high_) << shift) | (low_ >> (64 - shift));
    
    return Int128(new_low, static_cast<int64_t>(new_high));
}

Int128 Int128::operator>>(int shift) const {
    if (shift == 0) return *this;
    if (shift >= 128) {
        if (high_ < 0) return Int128(-1);
        return Int128(0);
    }
    
    uint64_t new_high = static_cast<uint64_t>(high_) >> shift;
    uint64_t new_low = (low_ >> shift) | (static_cast<uint64_t>(high_) << (64 - shift));
    
    if (high_ < 0 && shift > 0) {
        new_high |= (~0ULL) << (64 - shift);
    }
    
    return Int128(new_low, static_cast<int64_t>(new_high));
}

Int128 Int128::operator*(const Int128& other) const {
    bool negative = (high_ < 0) ^ (other.high_ < 0);
    Int128 a = high_ < 0 ? -*this : *this;
    Int128 b = other.high_ < 0 ? -other : other;

    uint32_t a0 = static_cast<uint32_t>(a.low_ & 0xFFFFFFFF);
    uint32_t a1 = static_cast<uint32_t>(a.low_ >> 32);
    uint32_t a2 = static_cast<uint32_t>(static_cast<uint64_t>(a.high_) & 0xFFFFFFFF);
    uint32_t a3 = static_cast<uint32_t>(static_cast<uint64_t>(a.high_) >> 32);

    uint32_t b0 = static_cast<uint32_t>(b.low_ & 0xFFFFFFFF);
    uint32_t b1 = static_cast<uint32_t>(b.low_ >> 32);
    uint32_t b2 = static_cast<uint32_t>(static_cast<uint64_t>(b.high_) & 0xFFFFFFFF);
    uint32_t b3 = static_cast<uint32_t>(static_cast<uint64_t>(b.high_) >> 32);

    uint64_t res[8] = {0};

    res[0] += static_cast<uint64_t>(a0) * b0;
    res[1] += static_cast<uint64_t>(a0) * b1;
    res[2] += static_cast<uint64_t>(a0) * b2;
    res[3] += static_cast<uint64_t>(a0) * b3;

    res[1] += static_cast<uint64_t>(a1) * b0;
    res[2] += static_cast<uint64_t>(a1) * b1;
    res[3] += static_cast<uint64_t>(a1) * b2;
    res[4] += static_cast<uint64_t>(a1) * b3;

    res[2] += static_cast<uint64_t>(a2) * b0;
    res[3] += static_cast<uint64_t>(a2) * b1;
    res[4] += static_cast<uint64_t>(a2) * b2;
    res[5] += static_cast<uint64_t>(a2) * b3;

    res[3] += static_cast<uint64_t>(a3) * b0;
    res[4] += static_cast<uint64_t>(a3) * b1;
    res[5] += static_cast<uint64_t>(a3) * b2;
    res[6] += static_cast<uint64_t>(a3) * b3;

    for (int i = 0; i < 7; ++i) {
        res[i + 1] += res[i] >> 32;
        res[i] &= 0xFFFFFFFF;
    }

    uint64_t new_low = res[0] | (res[1] << 32);
    int64_t new_high = static_cast<int64_t>(res[2] | (res[3] << 32));
    Int128 result(new_low, new_high);
    return negative ? -result : result;
}

Int128& Int128::operator*=(const Int128& other) {
    *this = *this * other;
    return *this;
}

Int128 Int128::operator/(const Int128& other) const {
    if (other.high_ == 0 && other.low_ == 0) return Int128(0);
    
    bool negative = (high_ < 0) ^ (other.high_ < 0);
    Int128 dividend = high_ < 0 ? -*this : *this;
    Int128 divisor = other.high_ < 0 ? -other : other;
    
    if (dividend < divisor) return Int128(0);
    
    Int128 quotient(0);
    Int128 remainder(0);
    
    for (int i = 127; i >= 0; --i) {
        remainder = remainder << 1;
        
        if (i < 64) {
            remainder.low_ |= (dividend.low_ >> i) & 1;
        } else {
            remainder.low_ |= (static_cast<uint64_t>(dividend.high_) >> (i - 64)) & 1;
        }
        
        if (remainder >= divisor) {
            remainder = remainder - divisor;
            if (i < 64) {
                quotient.low_ |= (1ULL << i);
            } else {
                quotient.high_ |= (1LL << (i - 64));
            }
        }
    }
    
    return negative ? -quotient : quotient;
}

Int128& Int128::operator/=(const Int128& other) {
    *this = *this / other;
    return *this;
}

bool Int128::operator==(const Int128& other) const {
    return low_ == other.low_ && high_ == other.high_;
}

bool Int128::operator!=(const Int128& other) const {
    return !(*this == other);
}

bool Int128::operator<(const Int128& other) const {
    if (high_ != other.high_) return high_ < other.high_;
    return low_ < other.low_;
}

bool Int128::operator>(const Int128& other) const {
    return other < *this;
}

bool Int128::operator<=(const Int128& other) const {
    return !(other < *this);
}

bool Int128::operator>=(const Int128& other) const {
    return !(*this < other);
}

std::ostream& operator<<(std::ostream& os, const Int128& value) {
    os << value.str();
    return os;
}