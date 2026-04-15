#include "expression.hpp"
#include <iostream>

// Const
Const::Const(const Int128& value) : value_(value) {}

Int128 Const::eval(const std::map<std::string, Int128>&) const {
    return value_;
}

std::unique_ptr<Expression> Const::clone() const {
    return std::make_unique<Const>(value_);
}

std::string Const::to_string() const {
    return value_.str();
}

// Variable
Variable::Variable(const std::string& name) : name_(name) {}

Int128 Variable::eval(const std::map<std::string, Int128>& vars) const {
    auto it = vars.find(name_);
    if (it != vars.end()) {
        return it->second;
    }
    return Int128(0);
}

std::unique_ptr<Expression> Variable::clone() const {
    return std::make_unique<Variable>(name_);
}

std::string Variable::to_string() const {
    return name_;
}

// Negate
Negate::Negate(std::unique_ptr<Expression> expr) : expr_(std::move(expr)) {}

Negate::Negate(const Expression& expr) : expr_(expr.clone()) {}

Int128 Negate::eval(const std::map<std::string, Int128>& vars) const {
    return -expr_->eval(vars);
}

std::unique_ptr<Expression> Negate::clone() const {
    return std::make_unique<Negate>(*expr_);
}

std::string Negate::to_string() const {
    return "(-" + expr_->to_string() + ")";
}

// Add
Add::Add(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
    : left_(std::move(left)), right_(std::move(right)) {}

Add::Add(const Expression& left, const Expression& right)
    : left_(left.clone()), right_(right.clone()) {}

Int128 Add::eval(const std::map<std::string, Int128>& vars) const {
    return left_->eval(vars) + right_->eval(vars);
}

std::unique_ptr<Expression> Add::clone() const {
    return std::make_unique<Add>(*left_, *right_);
}

std::string Add::to_string() const {
    return "(" + left_->to_string() + " + " + right_->to_string() + ")";
}

// Subtract
Subtract::Subtract(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
    : left_(std::move(left)), right_(std::move(right)) {}

Subtract::Subtract(const Expression& left, const Expression& right)
    : left_(left.clone()), right_(right.clone()) {}

Int128 Subtract::eval(const std::map<std::string, Int128>& vars) const {
    return left_->eval(vars) - right_->eval(vars);
}

std::unique_ptr<Expression> Subtract::clone() const {
    return std::make_unique<Subtract>(*left_, *right_);
}

std::string Subtract::to_string() const {
    return "(" + left_->to_string() + " - " + right_->to_string() + ")";
}

// Multiply
Multiply::Multiply(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
    : left_(std::move(left)), right_(std::move(right)) {}

Multiply::Multiply(const Expression& left, const Expression& right)
    : left_(left.clone()), right_(right.clone()) {}

Int128 Multiply::eval(const std::map<std::string, Int128>& vars) const {
    return left_->eval(vars) * right_->eval(vars);
}

std::unique_ptr<Expression> Multiply::clone() const {
    return std::make_unique<Multiply>(*left_, *right_);
}

std::string Multiply::to_string() const {
    return "(" + left_->to_string() + " * " + right_->to_string() + ")";
}

// Divide
Divide::Divide(std::unique_ptr<Expression> left, std::unique_ptr<Expression> right)
    : left_(std::move(left)), right_(std::move(right)) {}

Divide::Divide(const Expression& left, const Expression& right)
    : left_(left.clone()), right_(right.clone()) {}

Int128 Divide::eval(const std::map<std::string, Int128>& vars) const {
    return left_->eval(vars) / right_->eval(vars);
}

std::unique_ptr<Expression> Divide::clone() const {
    return std::make_unique<Divide>(*left_, *right_);
}

std::string Divide::to_string() const {
    return "(" + left_->to_string() + " / " + right_->to_string() + ")";
}

// Операторы
std::unique_ptr<Expression> operator+(const Expression& left, const Expression& right) {
    return std::make_unique<Add>(left, right);
}

std::unique_ptr<Expression> operator-(const Expression& left, const Expression& right) {
    return std::make_unique<Subtract>(left, right);
}

std::unique_ptr<Expression> operator*(const Expression& left, const Expression& right) {
    return std::make_unique<Multiply>(left, right);
}

std::unique_ptr<Expression> operator/(const Expression& left, const Expression& right) {
    return std::make_unique<Divide>(left, right);
}

std::unique_ptr<Expression> operator-(const Expression& expr) {
    return std::make_unique<Negate>(expr);
}

std::ostream& operator<<(std::ostream& os, const Expression& expr) {
    os << expr.to_string();
    return os;
}