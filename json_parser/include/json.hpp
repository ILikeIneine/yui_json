#pragma once
#include <type_traits>
#include <vector>
#include <map>
#include <string>
#include <memory>


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
  std::shared_ptr<json_value> inner_value_;

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
  Json() noexcept;
  Json(std::nullptr_t) noexcept;


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
  bool operator<=(const Json& rhs) const { return rhs >= *this; }

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

// serialization
static void dump(null_helper, std::string& out) { out += "null"; }

static void dump(double value, std::string& out)
{
  if (std::isfinite(value))
  {
    char buf[32];
    (void)snprintf(buf, sizeof buf, "%.17g", value);
    out += buf;
    // out += std::to_string(value);
  }
  else
  {
    out += "invalid float";
  }
}

static void dump(int value, std::string& out) { out += std::to_string(value); }

static void dump(bool value, std::string& out) { out += value ? "true" : "false"; }

static void dump(const std::string& value, std::string& out)
{
  out += '"';

  for (size_t i = 0; i < value.size(); ++i)
  {
    if (const char ch = value[i]; ch == '\\')
    {
      out += R"(\\)";
    }
    else if (ch == '"')
    {
      out += R"(\")";
    }
    else if (ch == '\b')
    {
      out += R"(\b)";
    }
    else if (ch == '\f')
    {
      out += R"(\f)";
    }
    else if (ch == '\n')
    {
      out += R"(\n)";
    }
    else if (ch == '\r')
    {
      out += R"(\r)";
    }
    else if (ch == '\t')
    {
      out += R"(\t)";
    }
    else if (static_cast<uint8_t>(ch) <= 0x1f)
    {
      char buf[8];
      (void)snprintf(buf, sizeof buf, "\\u%04x", ch);
      out += buf;
    }
    else if (static_cast<uint8_t>(ch) == 0xe2 &&
      static_cast<uint8_t>(value[i + 1]) == 0x80 &&
      static_cast<uint8_t>(value[i + 2]) == 0xa8)
    {
      // LS
      out += R"(\u2028)";
      i += 2;
    }
    else if (static_cast<uint8_t>(ch) == 0xe2 &&
      static_cast<uint8_t>(value[i + 1]) == 0x80 &&
      static_cast<uint8_t>(value[i + 2]) == 0xa9)
    {
      // PS
      out += R"(\u2029)";
      i += 2;
    }
    else
    {
      out += ch;
    }
  }
  out += '"';
}

static void dump(const Json::array_type& values, std::string& out)
{
  bool first = true;
  out += '[';
  for(const auto& value : values)
  {
    if (!first)
      out += ", ";
    first = false;
    value.dump(out);
  }
  out += ']';
}
static void dump(const Json::object_type& values, std::string& out)
{
  bool first = true;
  out += '{';
  for(const auto& [name, value] : values)
  {
    if (!first)
      out += ", ";
    first = false;

    dump(name, out);
    out += ": ";
    value.dump(out);
  }
  out += '}';
}

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
    ine::dump(value_, out);
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

  const Json& operator[](size_t i) const override;

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
  const Json& operator[](const std::string& key) const override;

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
  inline static const std::string null_string{};
  inline static const Json::array_type null_array{};
  inline static const Json::object_type null_object{};
};

inline const Json null_json;

// constructors of Json
inline Json::Json() noexcept : inner_value_(json_statics::null)
{
}

inline Json::Json(std::nullptr_t) noexcept : inner_value_(json_statics::null)
{
}

inline Json::Json(double value) : inner_value_(std::make_shared<json_double>(value))
{
}

inline Json::Json(int value) : inner_value_(std::make_shared<json_int>(value))
{
}

inline Json::Json(const char* value) : inner_value_(std::make_shared<json_string>(value))
{
}

inline Json::Json(const std::string& value) : inner_value_(std::make_shared<json_string>(value))
{
}

inline Json::Json(std::string&& value) noexcept : inner_value_(std::make_shared<json_string>(std::move(value)))
{
}

inline Json::Json(const array_type& value) : inner_value_(std::make_shared<json_array>(value))
{
}

inline Json::Json(array_type&& value) noexcept : inner_value_(std::make_shared<json_array>(std::move(value)))
{
}

inline Json::Json(const object_type& value) : inner_value_(std::make_shared<json_object>(value))
{
}

inline Json::Json(object_type&& value) noexcept : inner_value_(std::make_shared<json_object>(std::move(value)))
{
}

// accessor
inline Json::Type
Json::type() const
{
  return inner_value_->type();
}

inline double
Json::number_value() const
{
  return inner_value_->number_value();
}

inline int
Json::int_value() const
{
  return inner_value_->int_value();
}

inline bool
Json::bool_value() const
{
  return inner_value_->bool_value();
}

inline const std::string&
Json::string_value() const
{
  return inner_value_->string_value();
}

inline const Json::array_type&
Json::array_items() const
{
  return inner_value_->array_items();
}

inline const Json::object_type&
Json::object_items() const
{
  return inner_value_->object_item();
}

inline const Json&
Json::operator[](size_t i) const
{
  return (*inner_value_)[i];
}

inline const Json&
Json::operator[](const std::string& key) const
{
  return (*inner_value_)[key];
}

inline void
Json::dump(std::string& out) const
{
  inner_value_->dump(out);
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
json_value::operator[](const std::string&) const
{
  return null_json;
}

inline const Json&
json_value::operator[](size_t i) const
{
  return null_json;
}

inline const Json&
json_object::operator[](const std::string& key) const
{
  const auto iter = value_.find(key);
  return iter == value_.end() ? null_json : iter->second;
}

inline const Json&
json_array::operator[](size_t i) const
{
  if (i >= value_.size()) return null_json;
  return value_[i];
}

inline bool Json::operator==(const Json& rhs) const
{
  if (inner_value_ == rhs.inner_value_)
    return true;
  if (inner_value_->type() != rhs.inner_value_->type())
    return false;

  return inner_value_->equal(rhs.inner_value_.get());
}

inline bool Json::operator<(const Json& rhs) const
{
  if (inner_value_ == rhs.inner_value_)
    return false;
  if (inner_value_->type() != rhs.inner_value_->type())
    return inner_value_->type() < rhs.inner_value_->type();

  return inner_value_->less(rhs.inner_value_.get());
}

// Parsing helper
static std::string
esc(char c)
{
  char buf[12];
  if(static_cast<uint8_t>(c) >= 0x20 && static_cast<uint8_t>(c) <= 0x7f)
  {
    (void)snprintf(buf, sizeof buf, "'%c' (%d)", c, c);
  }
  else
  {
    (void)snprintf(buf, sizeof buf, "(%d)", c);
  }
  return { buf };
}


// Parsing
struct JsonParser final
{
  const std::string& str;
  size_t i;
  std::string& err;
  bool failed;
  const int strategy;

  Json fail(std::string&& msg) { return fail(std::move(msg), Json()); }

  template <typename T>
  auto fail(std::string&& msg, const T err_ret)
  {
    if (!failed)
      err = std::move(msg);
    failed = true;
    return err_ret;
  }

  // eat until str[i] is not space charactors
  void consume_whitespace();

  // todo: eat comments
  bool consume_comment();

  // eat until str[i] is not space charactors
  // adn comments
  void consume_garbage();

  char get_next_token();

  void encode_utf8(long pt, std::string& out)
  {
    if (pt < 0)
      return;

    if (pt < 0x80)
    {
      out += static_cast<char>(pt);
    }
    else if (pt < 0x800)
    {
      out += static_cast<char>((pt >> 6) | 0xC0);
      out += static_cast<char>((pt & 0x3F) | 0x80);
    }
    else if (pt < 0x10000)
    {
      out += static_cast<char>((pt >> 12) | 0xE0);
      out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
      out += static_cast<char>((pt & 0x3F) | 0x80);
    }
    else
    {
      out += static_cast<char>((pt >> 18) | 0xF0);
      out += static_cast<char>(((pt >> 12) & 0x3F) | 0x80);
      out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
      out += static_cast<char>((pt & 0x3F) | 0x80);
    }
  }


  std::string parse_string();

  Json parse_number();

  Json expect(const std::string& expected, Json res);

  Json parse_json(int depth);

};


inline Json
Json::parse(const std::string& in, std::string& err)
{
  JsonParser parser(in, 0, err, false, 0);
  Json result = parser.parse_json(0);

  parser.consume_garbage();
  if (parser.failed)
    return {};
  if (parser.i != in.size())
    return parser.fail("unexpected trailing" + esc(in[parser.i]));
  
}


}
