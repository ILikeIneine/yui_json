#pragma once
#include <cassert>
#include <type_traits>
#include <vector>
#include <map>
#include <string>
#include <memory>


namespace ine
{
constexpr int max_depth = 200;

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
  for (const auto& value : values)
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
  for (const auto& [name, value] : values)
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
  if (static_cast<uint8_t>(c) >= 0x20 && static_cast<uint8_t>(c) <= 0x7f)
  {
    (void)snprintf(buf, sizeof buf, "'%c' (%d)", c, c);
  }
  else
  {
    (void)snprintf(buf, sizeof buf, "(%d)", c);
  }
  return {buf};
}

template <typename T, typename U, typename L>
constexpr static bool
in_range(T x, L lower, U upper)
{
  return x >= lower && x <= upper;
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
  void consume_whitespace()
  {
    while (str[i] == ' ' || str[i] == '\n' || str[i] == '\t' || str[i] == '\r') ++i;
  }

  // todo: eat comments
  bool consume_comment();

  // eat until str[i] is not space charactors
  // adn comments
  void consume_garbage()
  {
    consume_whitespace();
    // todo: consume comment? (c-like)
  }

  char get_next_token()
  {
    consume_garbage();
    if (failed)
      return fail("unexpected token", static_cast<char>(0));
    if (i == str.size())
      return fail("unexpected end of json", static_cast<char>(0));

    return str[i++];
  }

  void encode_as_utf8(long pt, std::string& out)
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


  std::string parse_string()
  {
    std::string token_str;
    long last_escaped = -1;
    while (true)
    {
      if (i >= str.size())
        return fail("unexpected end while parsing string", "");

      char ch = str[i++];
      if (ch == '"')
      {
        encode_as_utf8(last_escaped, token_str);
        return token_str;
      }
      if (ch >= 0 && ch <= 0x1f)
        return fail("unexpected string token :" + esc(ch), "");

      if (ch != '\\')
      {
        encode_as_utf8(last_escaped, token_str);
        last_escaped = -1;
        token_str += ch;
        continue;
      }

      // from here is escaped chars
      if (i == str.size())
        return fail("unexpected end of string", "");

      // get next token after '//'
      ch = get_next_token();

      // unicode!!!!!!!!
      // todo: got something fucking error with this shit
      if (ch == 'u')
      {
        auto esc = str.substr(i, 4);

        if (esc.size() < 4)
        {
          return fail("unexpected \\u escape(error length): " + esc, "");
        }

        for (size_t j = 0; j < 4; ++j)
        {
          if ((esc[j] < 'a' || esc[j] > 'f')
            || (esc[j] < 'A' || esc[j] > 'F')
            || (esc[j] < '0' || esc[j] > '9'))
            return fail("unexpected \\u escape(error content): " + esc, "");
        }

        // JSON specifies that characters outside the BMP shall be encoded as a
        // pair of 4-hex-digit \u escapes encoding their surrogate pair
        // components. Check whether we're in the middle of such a beast: the
        // previous codepoint was an escaped lead (high) surrogate, and this is
        // a trail (low) surrogate.
        // ref: https://zhuanlan.zhihu.com/p/370601172
        if (const auto code_point = strtol(esc.data(), nullptr, 16);
          in_range(last_escaped, 0xD800, 0xDBFF) && in_range(code_point, 0xDC00, 0xDFFF))
        {
          // plain section
          // x' = yyyyyyyyyyxxxxxxxxxx   // x - 0x10000
          // x1' = 110110yyyyyyyyyy      // 0xD800 + yyyyyyyyyy
          // x2' = 110111xxxxxxxxxx      // 0xDC00 + xxxxxxxxxx
          encode_as_utf8(((last_escaped - 0xD800) << 10 | (code_point - 0xDC00)) + 0x10000, token_str);
          last_escaped = -1;
        }
        else
        {
          encode_as_utf8(last_escaped, token_str);
          last_escaped = code_point;
        }

        i += 4;
        continue;
      }

      encode_as_utf8(last_escaped, token_str);
      last_escaped = -1;

      // \b
      if (ch == 'b')
      {
        token_str += '\b';
      }
      // \f
      else if (ch == 'f')
      {
        token_str += '\f';
      }
      // \n
      else if (ch == 'n')
      {
        token_str += '\n';
      }
      // \r
      else if (ch == 'r')
      {
        token_str += '\r';
      }
      // \t
      else if (ch == 't')
      {
        token_str += '\t';
      }
      // " \ /
      else if (ch == '"' || ch == '\\' || ch == '/')
      {
        token_str += ch;
      }
      else
      {
        return fail("invalid escape char: " + esc(ch), "");
      }
    }
  }

  Json parse_number()
  {
    const auto start_pos = i;
    if (str[i] == '-')
      i++;

    if(str[i] == '0')
    {
      i++;
      if (in_range(str[i], '0', '9'))
        return fail("leading zero is not valid in numbers");
    }
    else if(in_range(str[i], '1', '9'))
    {
      while (in_range(str[++i], '0', '9')) ;
    }
    else
    {
      return fail("invalid " + esc(str[i]) + " in number");
    }

    if(str[i] != '.' 
      && str[i] != 'e' 
      && str[i] != 'E' 
      && (i-start_pos) <= static_cast<size_t>(std::numeric_limits<int>::digits10))
    {
      // todo: bugs here
      return std::atoi(str.c_str() + start_pos);
    }

    // 1.2E+10
    //  ^----------- Decimal
    if(str[i] == '.')
    {
      i++;
      if(!in_range(str[i], '0', '9'))
      {
        return fail("not valid Decimal-Part number consistance");
      }

      while (in_range(str[i], '0', '9')) i++;
    }

    // 1.2E+10
    //    ^-------  Exp
    if(str[i] == 'e' || str[i] == 'E')
    {
      i++;
      if (str[i] == '+' || str[i] == '-')
        i++;

      if (!in_range(str[i], '0', '9'))
        return fail("at least one digit required in exponencial part");

      while (in_range(str[i], '0', '9')) ++i;
    }
    return strtod(str.c_str() + start_pos, nullptr);
  }

  Json expect(const std::string& expected, Json res)
  {
    // there must be a char for we to expect
    // in another word, we can't expected at beginning
    assert(i != 0);
    i--;
    if (str.compare(i, expected.size(), expected) == 0)
    {
      i += str.size();
      return res;
    }

    return fail("parse error, expected: " + expected + " but got: " + str.substr(i, expected.length()));
  }

  Json parse_json(int depth)
  {
    if (depth > max_depth)
    {
      return fail("exceeded max depth");
    }

    char ch = get_next_token();
    if (failed) return {};

    // numeric
    if (ch == '-' || (ch >= '0' && ch <= '9'))
    {
      i--;
      return parse_number();
    }

    // boolean
    if (ch == 't')
      return expect("true", true);
    if (ch == 'f')
      return expect("false", false);

    // null
    if (ch == 'n')
      return expect("null", {});

    // string
    if (ch == '"')
      return parse_string();

    // object
    if (ch == '{')
    {
      Json::object_type data;
      ch = get_next_token();

      if (ch == '}')
        return data;
      while (true)
      {
        // abstract the key of string type
        if (ch != '"')
          return fail("expected '\"' in object, got " + esc(ch));

        std::string key = parse_string();
        if (failed)
          return {};

        ch = get_next_token();
        if (ch != ':')
          return fail("expected ':' in object, got " + esc(ch));

        data[std::move(key)] = parse_json(depth + 1);
        if (failed)
          return {};

        ch = get_next_token();
        if (ch == '}')
          break;
        if (ch != ',')
          return fail("expected ',' in object, got " + esc(ch));

        ch = get_next_token();
      }
      return data;
    }

    if (ch == '[')
    {
      Json::array_type data;
      ch = get_next_token();
      if (ch == ']')
        return data;

      while (true)
      {
        i--;
        data.push_back(parse_json(depth + 1));
        if (failed)
          return {};

        ch = get_next_token();
        if (ch == ']')
          break;

        if (ch != ',')
          return fail("expected ',' in list, got " + esc(ch));

        get_next_token();
      }
      return data;
    }

    return fail("expected value, got " + esc(ch));
  }
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

  return result;
}

inline bool
Json::is_valid(const shape& types, std::string& err) const
{
  if (!is_object())
  {
    err = "expected JSON object, got " + dump();
    return false;
  }

  const auto& obj_items = object_items();
  for (auto& [name, type] : types)
  {
    const auto it = obj_items.find(name);
    if (it == object_items().end() || type != it->second.type())
    {
      err = "bad type for " + name + "in " + dump();
      return false;
    }
  }
  return true;
}
}
