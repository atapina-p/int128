#ifndef EXPRESSION_HPP
#define EXPRESSION_HPP

#include "int128.hpp"
#include <map>
#include <memory>
#include <string>
#include <iostream>

class Expression {
public:
    virtual ~Expression() = default;
    virtual Int128 eval(const std::map<std::string, Int128>& vars) const = 0;
    virtual std::unique_ptr<Expression> clone() const = 0;
    virtual std::string to_string() const = 0;
};

class Const : public Expression {
private:
    Int128 value_;
public:
    Const(const Int128& value);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Variable : public Expression {
private:
    std::string name_;
public:
    Variable(const std::string& name);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Negate : public Expression {
private:
    std::unique_ptr<Expression> expr_;
public:
    Negate(std::unique_ptr<Expression> expr);
    Negate(const Expression& expr);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Add : public Expression {
private:
    std::unique_ptr<Expression> left_;
    std::unique_ptr<Expression> right_;
public:
    Add(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right);
    Add(const Expression& left, const Expression& right);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Subtract : public Expression {
private:
    std::unique_ptr<Expression> left_;
    std::unique_ptr<Expression> right_;
public:
    Subtract(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right);
    Subtract(const Expression& left, const Expression& right);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Multiply : public Expression {
private:
    std::unique_ptr<Expression> left_;
    std::unique_ptr<Expression> right_;
public:
    Multiply(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right);
    Multiply(const Expression& left, const Expression& right);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

class Divide : public Expression {
private:
    std::unique_ptr<Expression> left_;
    std::unique_ptr<Expression> right_;
public:
    Divide(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right);
    Divide(const Expression& left, const Expression& right);
    Int128 eval(const std::map<std::string, Int128>& vars) const override;
    std::unique_ptr<Expression> clone() const override;
    std::string to_string() const override;
};

// Операторы для Expression (возвращают unique_ptr)
std::unique_ptr<Expression> operator+(const Expression& left, const Expression& right);
std::unique_ptr<Expression> operator-(const Expression& left, const Expression& right);
std::unique_ptr<Expression> operator*(const Expression& left, const Expression& right);
std::unique_ptr<Expression> operator/(const Expression& left, const Expression& right);
std::unique_ptr<Expression> operator-(const Expression& expr);

// Перегрузки для unique_ptr с Expression
inline std::unique_ptr<Expression> operator+(std::unique_ptr<Expression> left, const Expression& right) {
    return std::make_unique<Add>(std::move(left), right.clone());
}

inline std::unique_ptr<Expression> operator+(const Expression& left, std::unique_ptr<Expression> right) {
    return std::make_unique<Add>(left.clone(), std::move(right));
}

inline std::unique_ptr<Expression> operator+(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) {
    return std::make_unique<Add>(std::move(left), std::move(right));
}

inline std::unique_ptr<Expression> operator-(std::unique_ptr<Expression> left, const Expression& right) {
    return std::make_unique<Subtract>(std::move(left), right.clone());
}

inline std::unique_ptr<Expression> operator-(const Expression& left, std::unique_ptr<Expression> right) {
    return std::make_unique<Subtract>(left.clone(), std::move(right));
}

inline std::unique_ptr<Expression> operator-(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) {
    return std::make_unique<Subtract>(std::move(left), std::move(right));
}

inline std::unique_ptr<Expression> operator*(std::unique_ptr<Expression> left, const Expression& right) {
    return std::make_unique<Multiply>(std::move(left), right.clone());
}

inline std::unique_ptr<Expression> operator*(const Expression& left, std::unique_ptr<Expression> right) {
    return std::make_unique<Multiply>(left.clone(), std::move(right));
}

inline std::unique_ptr<Expression> operator*(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) {
    return std::make_unique<Multiply>(std::move(left), std::move(right));
}

inline std::unique_ptr<Expression> operator/(std::unique_ptr<Expression> left, const Expression& right) {
    return std::make_unique<Divide>(std::move(left), right.clone());
}

inline std::unique_ptr<Expression> operator/(const Expression& left, std::unique_ptr<Expression> right) {
    return std::make_unique<Divide>(left.clone(), std::move(right));
}

inline std::unique_ptr<Expression> operator/(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right) {
    return std::make_unique<Divide>(std::move(left), std::move(right));
}

// Вывод в поток
std::ostream& operator<<(std::ostream& os, const Expression& expr);

#endif