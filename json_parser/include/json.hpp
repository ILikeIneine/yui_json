#pragma once
#include <type_traits>
#include <vector>
#include <map>
#include <string>
#include <memory>

#include "json.hpp"
#include "json.hpp"
#include "json.hpp"

namespace ine
{
class json_value;

// basic Json
class Json final
{
public:
  // https://www.json.org/json-en.html
  enum class Type { STRING, NUMBER, OBJECT, ARRAY, BOOL, NUL };

  using array_type = std::vector<Json>;
  using object_type = std::map<std::string, Json>;

private:
  std::shared_ptr<json_value> value_ptr_;

public:
  // Type::STRING 
  Json(const char* value);
  Json(const std::string& value);
  Json(std::string&& value) noexcept;

  // Type::NUMBER
  Json(double value);
  Json(int value);

  // Type::OBJECT
  Json(const object_type& value);
  Json(object_type&& value) noexcept;

  // Type::ARRAY
  Json(const array_type& value);
  Json(array_type&& value) noexcept;

  // Type:: NUL
  Json(std::nullptr_t) noexcept;
  Json() noexcept;


  template <typename T, typename = decltype(&T::to_json)>
  Json(const T& t) : Json(t.to_json())
  {
  }

  // map-like object_type
  template <typename M, std::enable_if_t<
              std::is_constructible_v<
                std::string, decltype(std::declval<M>().begin()->first)> &&
              std::is_constructible_v<
                Json, decltype(std::declval<M>().begin()->second)>, int>  = 0>
  Json(const M& m) : Json(object_type(m.begin(), m.end()))
  {
  }

  // vector-like object_type
  template <typename V, std::enable_if_t<
              std::is_constructible_v<
                Json, decltype(*std::declval<V>().begin())>, int>  = 0>
  Json(const V& v) : Json(array_type(v.begin(), v.end()))
  {
  }

  Json(void*) = delete;

  // type tag
  [[nodiscard]] Type type() const;
  [[nodiscard]] bool is_string() const { return type() == Type::STRING; }
  [[nodiscard]] bool is_number() const { return type() == Type::NUMBER; }
  [[nodiscard]] bool is_object() const { return type() == Type::OBJECT; }
  [[nodiscard]] bool is_array() const { return type() == Type::ARRAY; }
  [[nodiscard]] bool is_bool() const { return type() == Type::BOOL; }
  [[nodiscard]] bool is_null() const { return type() == Type::NUL; }

  // accessors 
  [[nodiscard]] double number_value() const;
  [[nodiscard]] int int_value() const;
  [[nodiscard]] bool bool_value() const;
  [[nodiscard]] const std::string& string_value() const;
  [[nodiscard]] const array_type& array_items() const;
  [[nodiscard]] const object_type& object_items() const;
  const Json& operator[](size_t i) const;
  const Json& operator[](const std::string& key) const;

  // dump
  void dump(std::string& out) const;

  [[nodiscard]] std::string dump() const
  {
    std::string out;
    dump(out);
    return out;
  }

  // Comparisons 
  bool operator==(const Json& rhs) const;
  bool operator<(const Json& rhs) const;
  bool operator!=(const Json& rhs) const { return !(*this == rhs); }
  bool operator>(const Json& rhs) const { return rhs < *this; }
  bool operator>=(const Json& rhs) const { return !(*this < rhs); }
  bool operator<=(const Json& rhs) const { return !(rhs < *this); }

  using shape = std::initializer_list<std::pair<std::string, Type>>;
  bool is_valid(const shape& types, std::string& err) const;

  // parser
  static Json parse(const std::string& in, std::string& err);

  static Json parse(const char* in, std::string& err)
  {
    if (in)
    {
      return parse(std::string(in), err);
    }
    err = "json pattern is null";
    return nullptr;
  }

  static array_type multi_parse(const std::string& in,
      std::string::size_type& parser_stop_pos,
      std::string& err);

  static array_type multi_parse(const std::string& in,
      std::string& err)
  {
    std::string::size_type parser_stop_pos;
    return multi_parse(in, parser_stop_pos, err);
  }
};

struct null_helper
{
  bool operator==(const null_helper&) const { return true; }
  bool operator<(const null_helper&) const { return false; }
};


class json_value
{
protected:
  friend class Json;
  friend class json_int;
  friend class json_double;

  [[nodiscard]] virtual Json::Type type() const = 0;
  [[nodiscard]] virtual bool equal(const json_value* other) const = 0;
  [[nodiscard]] virtual bool less(const json_value* other) const = 0;
  virtual void dump(std::string& out) const = 0;

  [[nodiscard]] virtual double number_value() const { return 0; }
  [[nodiscard]] virtual int int_value() const { return 0; }
  [[nodiscard]] virtual bool bool_value() const { return false; }

  [[nodiscard]] virtual const std::string& string_value() const;
  [[nodiscard]] virtual const Json::array_type& array_items() const;
  [[nodiscard]] virtual const Json& operator[](size_t i) const;

  [[nodiscard]] virtual const Json::object_type& object_item() const;
  [[nodiscard]] virtual const Json& operator[](const std::string& key) const;


  virtual ~json_value() = default;
};

template <Json::Type Tag, typename T>
class value_impl : public json_value
{
  using json_type = value_impl;

protected:
  const T value_;

protected:
  explicit value_impl(const T& value)
    : value_(value)
  {
  }

  explicit value_impl(T&& value)
    : value_(std::move(value))
  {
  }

  // Type tag
  [[nodiscard]]
  Json::Type
  type() const override { return Tag; }

  // Comparisons
  [[nodiscard]]
  bool
  equal(const json_value* other) const override
  {
    return value_ == static_cast<const json_type*>(other)->value_;
  }

  [[nodiscard]]
  bool
  less(const json_value* other) const override
  {
    return value_ < static_cast<const json_type*>(other)->value_;
  }

  void dump(std::string& out) const override

  {
    // todo: to be implemented
    // ine::dump(value_, out);
  }
};


class json_int final : public value_impl<Json::Type::NUMBER, int>
{
  [[nodiscard]]
  double
  number_value() const override { return value_; }

  [[nodiscard]]
  int
  int_value() const override { return value_; }

  bool
  equal(const json_value* other) const override
  {
    return value_ == other->number_value();
  }

  bool
  less(const json_value* other) const override
  {
    return value_ < other->number_value();
  }

public:
  explicit json_int(int value) : value_impl(value)
  {
  }
};

class json_double final : public value_impl<Json::Type::NUMBER, double>
{
  [[nodiscard]]
  double
  number_value() const override
  {
    return value_;
  }

  [[nodiscard]]
  int
  int_value() const override
  {
    return static_cast<int>(value_);
  }

  bool
  equal(const json_value* other) const override
  {
    return value_ == other->number_value();
  }

  bool
  less(const json_value* other) const override
  {
    return value_ < other->number_value();
  }

public:
  explicit json_double(double value) : value_impl(value)
  {
  }
};

class json_bolean final : public value_impl<Json::Type::BOOL, bool>
{
  [[nodiscard]]
  bool
  bool_value() const override
  {
    return value_;
  }

public:
  explicit json_bolean(bool value) : value_impl(value)
  {
  }
};

class json_string final : public value_impl<Json::Type::STRING, std::string>
{
  [[nodiscard]]
  const std::string&
  string_value() const override
  {
    return value_;
  }

public:
  explicit json_string(const std::string& value)
    : value_impl(value)
  {
  }

  explicit json_string(std::string&& value) noexcept
    : value_impl(std::move(value))
  {
  }
};

class json_array final : public value_impl<Json::Type::ARRAY, Json::array_type>
{
  [[nodiscard]]
  const Json::array_type&
  array_items() const override
  {
    return value_;
  }

  const Json&
  operator[](size_t i) const override
  {
    if (i > value_.size())
      return nullptr;

    return value_[i];
  }

public:
  explicit json_array(const Json::array_type& value)
    : value_impl(value)
  {
  }

  explicit json_array(Json::array_type&& value) noexcept
    : value_impl(std::move(value))
  {
  }
};

class json_object final : public value_impl<Json::Type::OBJECT, Json::object_type>
{
  [[nodiscard]]
  const Json::object_type&
  object_item() const override
  {
    return value_;
  }

  [[nodiscard]]
  const Json&
  operator[](const std::string& key) const override
  {
    const auto& iter = value_.find(key);
    return iter == value_.end() ? nullptr : iter->second;
  }

public:
  explicit json_object(const Json::object_type& value)
    : value_impl(value)
  {
  }

  explicit json_object(Json::object_type&& value) noexcept
    : value_impl(std::move(value))
  {
  }
};

class json_null final : public value_impl<Json::Type::NUL, null_helper>
{
public:
  json_null() : value_impl({})
  {
  }
};

// some static for const T& accessor
struct json_statics
{
  inline static const std::shared_ptr<json_value> null = std::make_shared<json_null>();
  inline static const std::shared_ptr<json_value> t    = std::make_shared<json_bolean>(true);
  inline static const std::shared_ptr<json_value> f    = std::make_shared<json_bolean>(false);
  inline static const std::string null_string {};
  inline static const Json::array_type null_array {};
  inline static const Json::object_type null_object {};
};

static inline const Json null_json;

// constructors of Json
inline Json::Json() noexcept : value_ptr_(json_statics::null)
{
}

inline Json::Json(std::nullptr_t) noexcept : value_ptr_(json_statics::null)
{
}

inline Json::Json(double value) : value_ptr_(std::make_shared<json_double>(value))
{
}

inline Json::Json(int value) : value_ptr_(std::make_shared<json_int>(value))
{
}

inline Json::Json(const char* value) : value_ptr_(std::make_shared<json_string>(value))
{
}

inline Json::Json(const std::string& value) : value_ptr_(std::make_shared<json_string>(value))
{
}

inline Json::Json(std::string&& value) noexcept : value_ptr_(std::make_shared<json_string>(std::move(value)))
{
}

inline Json::Json(const array_type& value) : value_ptr_(std::make_shared<json_array>(value))
{
}

inline Json::Json(array_type&& value) noexcept : value_ptr_(std::make_shared<json_array>(std::move(value)))
{
}

inline Json::Json(const object_type& value) : value_ptr_(std::make_shared<json_object>(value))
{
}

inline Json::Json(object_type&& value) noexcept : value_ptr_(std::make_shared<json_object>(std::move(value)))
{
}

// accessor
inline Json::Type
Json::type() const
{
  return value_ptr_->type();
}

inline double
Json::number_value() const
{
  return value_ptr_->number_value();
}

inline int
Json::int_value() const
{
  return value_ptr_->int_value();
}

inline bool
Json::bool_value() const
{
  return value_ptr_->bool_value();
}

inline const std::string&
Json::string_value() const
{
  return value_ptr_->string_value();
}

inline const Json::array_type&
Json::array_items() const
{
  return value_ptr_->array_items();
}

inline const Json::object_type&
Json::object_items() const
{
  return value_ptr_->object_item();
}

inline const Json&
Json::operator[](size_t i) const
{
  return (*value_ptr_)[i];
}

inline const Json&
Json::operator[](const std::string& key) const
{
  return (*value_ptr_)[key];
}

inline const std::string&
json_value::string_value() const
{
  return json_statics::null_string;
}

inline const Json::array_type& 
json_value::array_items() const
{
  return json_statics::null_array;
}

inline const Json::object_type& 
json_value::object_item() const
{
  return json_statics::null_object;
}

inline const Json& 
json_value::operator[](const std::string& key) const
{
  return null_json;
}



}
