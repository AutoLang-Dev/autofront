export module autofront:lex;

import std;
import :common;

using namespace std::literals;

export namespace autofront
{

class lexing_exception : public std::runtime_error
{
    using runtime_error::runtime_error;
};

enum struct lexeme : std::uint8_t
{
    DivWrap,
    DivSat,
    DivEq,
    Slash,
    LeftShiftEq,
    LeftShift,
    Spaceship,
    LessEq,
    Less,
    RightShiftEq,
    RightShift,
    GreaterEq,
    Greater,
    Inc,
    AddWrap,
    AddSat,
    AddEq,
    Plus,
    Dec,
    SubWrap,
    SubSat,
    SubEq,
    Minus,
    Arrow,
    OrEq,
    Or,
    BitOrEq,
    Pipe,
    AndEq,
    And,
    MulWrap,
    MulSat,
    MulEq,
    Star,
    RemEq,
    Rem,
    BitAndEq,
    Ampersand,
    XorEq,
    Caret,
    BitNotEq,
    Tilde,
    Eq,
    NotEq,
    Assign,
    Not,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftAngle,
    RightAngle,
    Scope,
    Colon,
    Semicolon,
    Comma,
    Dot,
    MethodAt,
    PipeCall,
    Ellipsis,
    OpenInterval,
    ClosedInterval,
    QuestionMark,
    At,
    Hash,
    Dollar,
    Underscore,
    FloatLit,
    BinLit,
    DecLit,
    OctLit,
    HexLit,
    StrLit,
    CharLit,
    LitSuf,
    Ident,
    Auto,
    As,
    If,
    Else,
    While,
    For,
    In,
    Break,
    Continue,
    Try,
    Catch,
    Throw,
    Resume,
    Return,
    Const,
    Mut,
    Match,
    Indep,
    This,
    True,
    False,
    Public,
    Protected,
    Private,
    Import,
    Using,
    Reloc,
    Mov,
    Trustme,
    Asm,
    LineComment   = 252,
    StreamComment = 253,
    Group         = 254,
    None          = 255,
};

auto is_literal(lexeme l) -> bool
{
    switch (l) {
    case lexeme::FloatLit:
    case lexeme::BinLit:
    case lexeme::OctLit:
    case lexeme::DecLit:
    case lexeme::HexLit:
    case lexeme::StrLit:
    case lexeme::CharLit:
        return true;
    default:
        return false;
    }
}

auto close_paren_type(lexeme l) -> lexeme
{
    switch (l) {
        break;
    case lexeme::LeftBrace:
        return lexeme::RightBrace;
        break;
    case lexeme::LeftBracket:
        return lexeme::RightBracket;
        break;
    case lexeme::LeftParen:
        return lexeme::RightParen;
        break;
    case lexeme::LeftAngle:
        return lexeme::RightAngle;
        break;
    default:
        return lexeme::None;
    }
}

auto is_operator(lexeme l) -> bool
{
    return l <= lexeme::Not;
}

}

template <>
struct std::formatter<autofront::lexeme, char> : public std::formatter<std::string_view, char>
{
    auto format(const autofront::lexeme& l, auto&& ctx) const;
};

#define AUTOFRONT_LEXEME_TO_STRING(l)                                                                                  \
    case lexeme::l:                                                                                                    \
        return std::format_to(ctx.out(), #l);

auto std::formatter<autofront::lexeme, char>::format(const autofront::lexeme& l, auto&& ctx) const
{
    using autofront::lexeme;
    switch (l) {
        AUTOFRONT_LEXEME_TO_STRING(DivWrap);
        AUTOFRONT_LEXEME_TO_STRING(DivSat);
        AUTOFRONT_LEXEME_TO_STRING(DivEq);
        AUTOFRONT_LEXEME_TO_STRING(Slash);
        AUTOFRONT_LEXEME_TO_STRING(LeftShiftEq);
        AUTOFRONT_LEXEME_TO_STRING(LeftShift);
        AUTOFRONT_LEXEME_TO_STRING(Spaceship);
        AUTOFRONT_LEXEME_TO_STRING(LessEq);
        AUTOFRONT_LEXEME_TO_STRING(Less);
        AUTOFRONT_LEXEME_TO_STRING(RightShiftEq);
        AUTOFRONT_LEXEME_TO_STRING(RightShift);
        AUTOFRONT_LEXEME_TO_STRING(GreaterEq);
        AUTOFRONT_LEXEME_TO_STRING(Greater);
        AUTOFRONT_LEXEME_TO_STRING(Inc);
        AUTOFRONT_LEXEME_TO_STRING(AddWrap);
        AUTOFRONT_LEXEME_TO_STRING(AddSat);
        AUTOFRONT_LEXEME_TO_STRING(AddEq);
        AUTOFRONT_LEXEME_TO_STRING(Plus);
        AUTOFRONT_LEXEME_TO_STRING(Dec);
        AUTOFRONT_LEXEME_TO_STRING(SubWrap);
        AUTOFRONT_LEXEME_TO_STRING(SubSat);
        AUTOFRONT_LEXEME_TO_STRING(SubEq);
        AUTOFRONT_LEXEME_TO_STRING(Arrow);
        AUTOFRONT_LEXEME_TO_STRING(Minus);
        AUTOFRONT_LEXEME_TO_STRING(OrEq);
        AUTOFRONT_LEXEME_TO_STRING(Or);
        AUTOFRONT_LEXEME_TO_STRING(BitOrEq);
        AUTOFRONT_LEXEME_TO_STRING(Pipe);
        AUTOFRONT_LEXEME_TO_STRING(AndEq);
        AUTOFRONT_LEXEME_TO_STRING(And);
        AUTOFRONT_LEXEME_TO_STRING(MulWrap);
        AUTOFRONT_LEXEME_TO_STRING(MulSat);
        AUTOFRONT_LEXEME_TO_STRING(MulEq);
        AUTOFRONT_LEXEME_TO_STRING(Star);
        AUTOFRONT_LEXEME_TO_STRING(RemEq);
        AUTOFRONT_LEXEME_TO_STRING(Rem);
        AUTOFRONT_LEXEME_TO_STRING(BitAndEq);
        AUTOFRONT_LEXEME_TO_STRING(Ampersand);
        AUTOFRONT_LEXEME_TO_STRING(XorEq);
        AUTOFRONT_LEXEME_TO_STRING(Caret);
        AUTOFRONT_LEXEME_TO_STRING(BitNotEq);
        AUTOFRONT_LEXEME_TO_STRING(Tilde);
        AUTOFRONT_LEXEME_TO_STRING(Eq);
        AUTOFRONT_LEXEME_TO_STRING(NotEq);
        AUTOFRONT_LEXEME_TO_STRING(Assign);
        AUTOFRONT_LEXEME_TO_STRING(Not);
        AUTOFRONT_LEXEME_TO_STRING(LeftBrace);
        AUTOFRONT_LEXEME_TO_STRING(RightBrace);
        AUTOFRONT_LEXEME_TO_STRING(LeftParen);
        AUTOFRONT_LEXEME_TO_STRING(RightParen);
        AUTOFRONT_LEXEME_TO_STRING(LeftBracket);
        AUTOFRONT_LEXEME_TO_STRING(RightBracket);
        AUTOFRONT_LEXEME_TO_STRING(LeftAngle);
        AUTOFRONT_LEXEME_TO_STRING(RightAngle);
        AUTOFRONT_LEXEME_TO_STRING(Scope);
        AUTOFRONT_LEXEME_TO_STRING(Colon);
        AUTOFRONT_LEXEME_TO_STRING(Semicolon);
        AUTOFRONT_LEXEME_TO_STRING(Comma);
        AUTOFRONT_LEXEME_TO_STRING(Dot);
        AUTOFRONT_LEXEME_TO_STRING(MethodAt);
        AUTOFRONT_LEXEME_TO_STRING(PipeCall);
        AUTOFRONT_LEXEME_TO_STRING(Ellipsis);
        AUTOFRONT_LEXEME_TO_STRING(OpenInterval);
        AUTOFRONT_LEXEME_TO_STRING(ClosedInterval);
        AUTOFRONT_LEXEME_TO_STRING(QuestionMark);
        AUTOFRONT_LEXEME_TO_STRING(At);
        AUTOFRONT_LEXEME_TO_STRING(Hash);
        AUTOFRONT_LEXEME_TO_STRING(Dollar);
        AUTOFRONT_LEXEME_TO_STRING(Underscore);
        AUTOFRONT_LEXEME_TO_STRING(FloatLit);
        AUTOFRONT_LEXEME_TO_STRING(BinLit);
        AUTOFRONT_LEXEME_TO_STRING(OctLit);
        AUTOFRONT_LEXEME_TO_STRING(DecLit);
        AUTOFRONT_LEXEME_TO_STRING(HexLit);
        AUTOFRONT_LEXEME_TO_STRING(StrLit);
        AUTOFRONT_LEXEME_TO_STRING(CharLit);
        AUTOFRONT_LEXEME_TO_STRING(LitSuf);
        AUTOFRONT_LEXEME_TO_STRING(Auto);
        AUTOFRONT_LEXEME_TO_STRING(As);
        AUTOFRONT_LEXEME_TO_STRING(If);
        AUTOFRONT_LEXEME_TO_STRING(Else);
        AUTOFRONT_LEXEME_TO_STRING(While);
        AUTOFRONT_LEXEME_TO_STRING(For);
        AUTOFRONT_LEXEME_TO_STRING(In);
        AUTOFRONT_LEXEME_TO_STRING(Break);
        AUTOFRONT_LEXEME_TO_STRING(Continue);
        AUTOFRONT_LEXEME_TO_STRING(Try);
        AUTOFRONT_LEXEME_TO_STRING(Catch);
        AUTOFRONT_LEXEME_TO_STRING(Throw);
        AUTOFRONT_LEXEME_TO_STRING(Resume);
        AUTOFRONT_LEXEME_TO_STRING(Return);
        AUTOFRONT_LEXEME_TO_STRING(Const);
        AUTOFRONT_LEXEME_TO_STRING(Mut);
        AUTOFRONT_LEXEME_TO_STRING(Match);
        AUTOFRONT_LEXEME_TO_STRING(Indep);
        AUTOFRONT_LEXEME_TO_STRING(This);
        AUTOFRONT_LEXEME_TO_STRING(True);
        AUTOFRONT_LEXEME_TO_STRING(False);
        AUTOFRONT_LEXEME_TO_STRING(Public);
        AUTOFRONT_LEXEME_TO_STRING(Protected);
        AUTOFRONT_LEXEME_TO_STRING(Private);
        AUTOFRONT_LEXEME_TO_STRING(Import);
        AUTOFRONT_LEXEME_TO_STRING(Using);
        AUTOFRONT_LEXEME_TO_STRING(Reloc);
        AUTOFRONT_LEXEME_TO_STRING(Mov);
        AUTOFRONT_LEXEME_TO_STRING(Trustme);
        AUTOFRONT_LEXEME_TO_STRING(Asm);
        AUTOFRONT_LEXEME_TO_STRING(Ident);
        AUTOFRONT_LEXEME_TO_STRING(LineComment);
        AUTOFRONT_LEXEME_TO_STRING(StreamComment);
    case lexeme::Group:
        return std::format_to(ctx.out(), "(GROUP)");
    case lexeme::None:
        return std::format_to(ctx.out(), "(NONE)");
    }
}

export namespace autofront
{

struct token
{
    std::u32string_view view;
    source_position pos;
    lexeme type;

    auto span() const -> source_span
    {
        return source_span::make(pos, view.size());
    }
};

struct peeking
{
    std::ptrdiff_t n;
};

auto peek(std::ptrdiff_t n = 0z) -> peeking
{
    return {.n = n};
}

struct lexing_i_t
{
} lexing_i;

struct lexing_pos_t
{
    std::size_t offset;
};

auto lexing_pos(std::size_t offset = 0uz) -> lexing_pos_t
{
    return {.offset = offset};
}

struct storing_token
{
    std::size_t n;
    lexeme l;
};

auto store(std::size_t n, lexeme l) -> storing_token
{
    return {
        .n = n,
        .l = l,
    };
}

struct source_rem_t
{
} source_rem;

struct comment_depth_t
{
} comment_depth;

struct starting_with
{
    std::u32string_view sv;
};

auto start_with(std::u32string_view sv) -> starting_with
{
    return {
        .sv = sv,
    };
}

struct lexing_failure
{
    std::size_t offset;
    std::string message;
};

template <typename... Args>
auto lex_fail(std::format_string<Args...> fmt, Args&&... args) -> lexing_failure
{
    return {
        .offset  = 0uz,
        .message = std::format(fmt, std::forward<Args>(args)...),
    };
}

template <typename... Args>
auto lex_fail(std::size_t offset, std::format_string<Args...> fmt, Args&&... args) -> lexing_failure
{
    return {
        .offset  = offset,
        .message = std::format(fmt, std::forward<Args>(args)...),
    };
}

template <typename T = token>
struct [[nodiscard]] lexing
{
    struct lexing_promise
    {
        using handle_t = std::coroutine_handle<lexing_promise>;

        std::exception_ptr exception_;
        std::variant<std::monostate, T, error_entry> result_;
        std::u32string_view source_;
        std::size_t i_;
        std::size_t comment_depth_;
        source_position pos_;

        auto get_return_object() -> lexing
        {
            return unique_coro{handle_t::from_promise((*this))};
        }

        auto return_value(T tok) -> void
        {
            result_ = std::move(tok);
        }

        auto return_value(storing_token storing) -> void
            requires std::same_as<T, token>
        {
            auto [n, l] = storing;
            auto slice  = source_.substr(i_, n);

            result_ = token{
                .view = slice,
                .pos  = pos_,
                .type = l,
            };
            pos_.colno += n;
            i_         += n;
        }

        auto return_value(error_entry err, std::source_location l = std::source_location::current()) -> void
        {
            err.from = l;
            if (err.where == source_position{}) {
                err.where = pos_;
            }
            result_ = std::move(err);
        }

        auto return_value(lexing_failure fail, std::source_location l = std::source_location::current()) -> void
        {
            auto err = error_entry{
                .where   = pos_.next(fail.offset),
                .message = std::move(fail.message),
            };
            return_value(std::move(err), l);
        }

        auto initial_suspend() const noexcept -> std::suspend_always
        {
            return {};
        }

        auto final_suspend() const noexcept -> std::suspend_always
        {
            return {};
        }

        auto unhandled_exception() -> void
        {
            exception_ = std::current_exception();
        }

        auto await_transform(this_promise_t) -> just_awaitable<lexing_promise&>
        {
            return {*this};
        }

        auto await_transform(peeking pk) -> just_awaitable<char32_t>
        {
            auto n = pk.n;
            if (n < 0z) {
                auto t = [&] {
                    if (n == std::numeric_limits<std::ptrdiff_t>::max()) {
                        return std::numeric_limits<std::size_t>::max() / 2uz + 1uz;
                    }
                    return static_cast<std::size_t>(-n);
                }();
                if (i_ < t) {
                    return char32_t{};
                }
                assert_(i_ - t < source_.size(), "A");
                return source_[i_ - t];
            }
            if (n >= source_.size() - i_) {
                return char32_t{};
            }
            return source_[i_ + n];
        }

        auto await_transform(lexing_i_t) -> just_awaitable<std::size_t>
        {
            return i_;
        }

        auto await_transform(lexing_pos_t lp) -> just_awaitable<source_position>
        {
            return pos_.next(lp.offset);
        }

        auto await_transform(source_rem_t) -> just_awaitable<std::u32string_view>
        {
            return source_.substr(i_);
        }

        auto await_transform(comment_depth_t) -> just_awaitable<std::size_t>
        {
            return comment_depth_;
        }

        auto await_transform(starting_with staring) -> just_awaitable<bool>
        {
            return source_.substr(i_).starts_with(staring.sv);
        }

        template <typename U>
        struct lexing_transformed_awaitable
        {
            lexing<U> lexing_;
            lexing_promise& promise_;
            std::source_location l;

            auto await_ready() -> bool
            {
                if (!lexing_.is_stateful()) {
                    throw lexing_exception{R"(using co_await for a stateless "lexing")"};
                }
                return false;
            }

            auto await_suspend(handle_t handle) -> bool
            {
                assert_(lexing_.is_stateful(), R"(using co_await for a stateless "lexing")", l);
                assert_(&promise_ == &handle.promise(), "two coro is not equal", l);

                auto&& lp = lexing_.promise();
                lexing_.resume(promise_.source_, promise_.i_, promise_.pos_, promise_.comment_depth_);

                assert_(lexing_.finished(), "lexinng is not finished", l);
                if (lp.exception_) {
                    promise_.exception_ = std::move(lp.exception_);
                    return true;
                }
                if (lexing_.has_error()) {
                    promise_.result_ = std::get<error_entry>(std::move(lp.result_));
                    return true;
                }
                return false;
            }

            auto await_resume() noexcept -> U
            {
                assert_(lexing_.has_value(), "lexing is failed", l);

                auto&& lp               = lexing_.promise();
                promise_.i_             = lp.i_;
                promise_.pos_           = lp.pos_;
                promise_.comment_depth_ = lp.comment_depth_;

                return std::move(std::get<U>(lp.result_));
            }
        };

        template <typename U>
        auto await_transform(lexing<U> l, std::source_location loc = std::source_location::current())
            -> lexing_transformed_awaitable<U>
        {
            return {
                .lexing_  = std::move(l),
                .promise_ = *this,
                .l        = loc,
            };
        }
    };

    using promise_type = lexing_promise;

    unique_coro<promise_type> handle_;

    auto promise() const noexcept -> promise_type&
    {
        assert_(is_stateful(), "stateless lexing");
        return handle_.promise();
    }

    lexing(unique_coro<promise_type> handle) : handle_{std::move(handle)} {}

    auto index() const noexcept -> std::size_t
    {
        return promise().i_;
    }

    auto pos() const noexcept -> source_position
    {
        return promise().pos_;
    }

    auto resume(std::u32string_view source, std::size_t i, source_position pos, std::size_t comment_depth = 0uz)
    {
        auto&& p         = promise();
        p.source_        = source;
        p.i_             = i;
        p.pos_           = pos;
        p.comment_depth_ = comment_depth;
        handle_.resume();
    }

    lexing()         = default;
    lexing(lexing&&) = default;

    auto operator=(lexing&&) -> lexing& = default;

    auto is_stateful() const noexcept -> bool
    {
        return static_cast<bool>(handle_);
    }

    auto finished() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return true;
        return !std::holds_alternative<std::monostate>(promise().result_);
    }

    auto has_value() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return false;
        return std::holds_alternative<T>(promise().result_);
    }

    auto has_error() const noexcept -> bool
    {
        if (!is_stateful()) return false;
        if (promise().exception_) return false;
        return std::holds_alternative<error_entry>(promise().result_);
    }

    auto take_result() const -> std::expected<T, error_entry>
    {
        if (!is_stateful()) {
            throw lexing_exception{"stateless parsing"};
        }
        if (auto& e = promise().exception_) {
            std::rethrow_exception(std::move(e));
        }
        if (!finished()) {
            throw lexing_exception{"parsing is not finished"};
        }
        if (has_value()) {
            return std::move(std::get<T>(promise().result_));
        } else {
            return std::unexpected{std::move(std::get<error_entry>(promise().result_))};
        }
    }

    auto comment_depth() const -> std::size_t
    {
        if (!is_stateful()) {
            throw lexing_exception{"stateless parsing"};
        }
        return promise().comment_depth_;
    }
};

auto lex_ws() -> lexing<unit>
{
    auto& p = co_await this_promise;
    while (p.i_ < p.source_.size()) {
        assert_(p.i_ < p.source_.size(), "A");
        auto pk = p.source_[p.i_];
        if (!is_space(pk)) break;
        if (pk == U'\n') {
            p.pos_ = p.pos_.next_line_front();
        } else {
            ++p.pos_.colno;
        }
        ++p.i_;
    }
    co_return unit{};
}

auto lex_line_comment() -> lexing<>
{
    auto&& p = co_await this_promise;
    auto pos = p.pos_;
    auto rem = p.source_.substr(p.i_);
    auto j   = 0uz;
    auto lf  = false;
    for (; j < rem.size(); ++j) {
        if (rem[j] == U'\n') {
            lf = true;
            break;
        }
    }
    if (lf) {
        p.pos_  = pos.next_line_front();
        p.i_   += j + 1uz;
    } else {
        p.pos_  = pos.next(j);
        p.i_   += j;
    }
    co_return token{
        .view = rem.substr(0uz, j),
        .pos  = pos,
        .type = lexeme::LineComment,
    };
}

auto lex_stream_comment() -> lexing<>
{
    auto&& p      = co_await this_promise;
    auto line_rem = p.source_.substr(p.i_);
    assert_(p.comment_depth_ || line_rem.starts_with(U"/*"sv), "expected /* or non-zero depth");
    auto j = 0uz;
    for (auto& depth = p.comment_depth_; j < line_rem.size(); ++j) {
        auto rem = line_rem.substr(j);
        if (auto begin = rem.starts_with(U"/*"sv); begin || rem.starts_with(U"*/"sv)) {
            if (begin) {
                ++depth;
            } else {
                --depth;
            }
            ++j;
        }
        if (depth == 0uz) {
            ++j;
            break;
        }
    }
    co_return store(j, lexeme::StreamComment);
}

auto lex_skip() -> lexing<unit>
{
    if (co_await comment_depth) {
        co_await lex_stream_comment();
    }
    while (true) {
        auto pk = co_await peek();
        if (is_space(pk)) {
            co_await lex_ws();
        } else if (pk == U'/') {
            auto pk1 = co_await peek(1);
            if (pk1 == U'*') {
                co_await lex_stream_comment();
            } else if (pk1 == U'/') {
                co_await lex_line_comment();
            } else co_return unit{};
        } else co_return unit{};
    }
}

auto lex_slash() -> lexing<>
{
    assert_(co_await peek() == U'/', "expected /");
    auto pk1 = co_await peek(1);
    // if (pk1 == U'/') co_return co_await lex_line_comment();
    // if (pk1 == U'*') co_return co_await lex_stream_comment();
    assert_(pk1 != U'*', "unexpected /*");
    assert_(pk1 != U'/', "unexpected //");
    if (pk1 == U'=') co_return store(2uz, lexeme::DivEq);
    if (pk1 == U'|') co_return store(2uz, lexeme::DivSat);
    if (pk1 == U'%') co_return store(2uz, lexeme::DivWrap);
    co_return store(1uz, lexeme::Slash);
}

auto lex_less() -> lexing<>
{
    assert_(co_await peek() == U'<', "expected <");
    auto pk1 = co_await peek(1);
    auto pk2 = co_await peek(2);
    if (pk1 == U'<') {
        if (pk2 == U'=') {
            co_return store(3uz, lexeme::LeftShiftEq);
        }
        co_return store(2uz, lexeme::LeftShift);
    }
    if (pk1 == U'=') {
        if (pk2 == U'>') {
            co_return store(3uz, lexeme::Spaceship);
        }
        co_return store(2uz, lexeme::LessEq);
    }
    co_return store(1uz, lexeme::Less);
}

auto lex_greater() -> lexing<>
{
    assert_(co_await peek() == U'>', "expected >");
    // auto pk1 = co_await peek(1);
    // auto pk2 = co_await peek(2);
    // if (pk1 == U'>') {
    //     if (pk2 == U'=') {
    //         co_return store_token(3uz, lexeme::RightShiftEq);
    //     }
    //     co_return store_token(2uz, lexeme::RightShift);
    // }
    // if (pk1 == U'=') {
    //     co_return store_token(2uz, lexeme::GreaterEq);
    // }
    co_return storing_token(1uz, lexeme::Greater); // 这是为了简化尖括号对的解析
}

auto lex_plus() -> lexing<>
{
    assert_(co_await peek() == U'+', "expected +");
    auto pk1 = co_await peek(1);
    if (pk1 == U'+') co_return store(2uz, lexeme::Inc);
    if (pk1 == U'=') co_return store(2uz, lexeme::AddEq);
    if (pk1 == U'|') co_return store(2uz, lexeme::AddSat);
    if (pk1 == U'%') co_return store(2uz, lexeme::AddWrap);
    co_return store(1uz, lexeme::Plus);
}

auto lex_minus() -> lexing<>
{
    assert_(co_await peek() == U'-', "expected -");
    auto pk1 = co_await peek(1);
    if (pk1 == U'-') co_return store(2uz, lexeme::Dec);
    if (pk1 == U'=') co_return store(2uz, lexeme::SubEq);
    if (pk1 == U'|') co_return store(2uz, lexeme::SubSat);
    if (pk1 == U'%') co_return store(2uz, lexeme::SubWrap);
    if (pk1 == U'>') co_return store(2uz, lexeme::Arrow);
    co_return store(1uz, lexeme::Minus);
}

auto lex_pipe() -> lexing<>
{
    assert_(co_await peek() == U'|', "expected |");
    auto pk1 = co_await peek(1);
    auto pk2 = co_await peek(2);
    if (pk1 == U'|') {
        if (pk2 == U'=') {
            co_return store(3uz, lexeme::OrEq);
        }
        co_return store(2uz, lexeme::Or);
    }
    if (pk1 == U'=') {
        co_return store(2uz, lexeme::BitOrEq);
    }
    co_return store(1uz, lexeme::Pipe);
}

auto lex_ampersand() -> lexing<>
{
    assert_(co_await peek() == U'&', "expected &");
    auto pk1 = co_await peek(1);
    auto pk2 = co_await peek(2);
    if (pk1 == U'&') {
        if (pk2 == U'=') {
            co_return store(3uz, lexeme::AndEq);
        }
        co_return store(2uz, lexeme::And);
    }
    if (pk1 == U'=') {
        co_return store(2uz, lexeme::BitAndEq);
    }
    co_return store(1uz, lexeme::Ampersand);
}

auto lex_star() -> lexing<>
{
    assert_(co_await peek() == U'*', "expected *");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::MulEq);
    if (pk1 == U'|') co_return store(2uz, lexeme::MulSat);
    if (pk1 == U'%') co_return store(2uz, lexeme::MulWrap);
    co_return store(1uz, lexeme::Star);
}

auto lex_rem() -> lexing<>
{
    assert_(co_await peek() == U'%', "expected %");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::RemEq);
    co_return store(1uz, lexeme::Rem);
}

auto lex_caret() -> lexing<>
{
    assert_(co_await peek() == U'^', "expected ^");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::XorEq);
    co_return store(1uz, lexeme::Caret);
}

auto lex_tilde() -> lexing<>
{
    assert_(co_await peek() == U'~', "expected ~");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::BitNotEq);
    co_return store(1uz, lexeme::Tilde);
}

auto lex_eq() -> lexing<>
{
    assert_(co_await peek() == U'=', "expected =");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::Eq);
    co_return store(1uz, lexeme::Assign);
}

auto lex_not() -> lexing<>
{
    assert_(co_await peek() == U'!', "expected !");
    auto pk1 = co_await peek(1);
    if (pk1 == U'=') co_return store(2uz, lexeme::NotEq);
    co_return store(1uz, lexeme::Not);
}

auto lex_dot() -> lexing<>
{
    assert_(co_await peek() == U'.', "expected .");
    auto pk1 = co_await peek(1);
    auto pk2 = co_await peek(2);
    if (pk1 == U'.') {
        if (pk2 == U'.') co_return store(3uz, lexeme::Ellipsis);
        if (pk2 == U'<') co_return store(3uz, lexeme::OpenInterval);
        if (pk2 == U'=') co_return store(3uz, lexeme::ClosedInterval);
        co_return store(2uz, lexeme::MethodAt);
    }
    if (pk1 == U'|') co_return store(2uz, lexeme::PipeCall);
    co_return store(1uz, lexeme::Dot);
}

auto lex_colon() -> lexing<>
{
    assert_(co_await peek() == U':', "expected :");
    auto pk1 = co_await peek(1);
    if (pk1 == U':') co_return store(2uz, lexeme::Scope);
    co_return store(1uz, lexeme::Colon);
}

auto lex_digit() -> lexing<>;

auto lex_hash() -> lexing<>
{
    assert_(co_await peek() == U'#', "expected #");
    co_return lex_fail("not impl");
}

auto lex_zero() -> lexing<>
{
    assert_(co_await peek() == '0', "expected 0");

    auto is_oct = [](char32_t ch) {
        return U'0' <= ch && ch <= U'7';
    };
    auto is_bin = [](char32_t ch) {
        return U'0' == ch || ch == U'1';
    };

    auto kind = co_await peek(1);

    auto j   = 2uz;
    auto lex = lexeme::None;
    if (kind == U'x' || kind == U'X') {
        while (is_xdigit(co_await peek(j))) ++j;
        lex = lexeme::HexLit;
    } else if (kind == U'd' || kind == U'D') {
        while (is_digit(co_await peek(j))) ++j;
        lex = lexeme::DecLit;
    } else if (kind == U'o' || kind == U'O') {
        while (is_oct(co_await peek(j))) ++j;
        if (auto pk = co_await peek(j); pk == U'8' || pk == U'9') {
            auto ch = static_cast<char>(pk);
            co_return lex_fail(j, "expected a 0~7, but {} found", ch);
        }
        lex = lexeme::OctLit;
    } else if (kind == U'b' || kind == U'B') {
        while (is_bin(co_await peek(j))) ++j;
        if (auto pk = co_await peek(j); U'2' <= pk && pk <= U'9') {
            auto ch = static_cast<char>(pk);
            co_return lex_fail(j, "expected a 0 or 1, but {} found", ch);
        }
        lex = lexeme::BinLit;
    } else co_return co_await lex_digit();

    if (j == 2uz) {
        co_return lex_fail(j, "expected a digit in the corresponding base");
    }
    if (auto pk = co_await peek(j); pk == U'_' || is_xid_start(pk)) {
        ++j;
        while (is_xid_continue(co_await peek(j))) ++j;
    }
    co_return store(j, lex);
}

// 返回值替换原下标
auto lex_esc_seq(std::size_t j) -> lexing<std::size_t>
{
    assert_(co_await peek(j) == '\\', "expected \\");
    ++j;
    auto pk = co_await peek(j);
    if (U"n\\\'\""sv.contains(pk)) {
        co_return j + 1uz;
    }
    if (pk == U'{') {
        ++j;
        auto codepoint = std::uint32_t{};
        for (auto k = 0uz; codepoint <= unicode_limit; ++k, ++j) {
            auto ch = co_await peek(j);
            if (!is_xdigit(ch)) {
                if (k == 0uz) {
                    co_return lex_fail(j, "expected hex digit, but U+{:X} found", static_cast<std::uint32_t>(ch));
                }
                break;
            }
            auto digit = static_cast<std::uint32_t>([ch] -> char32_t {
                if (is_digit(ch)) {
                    return ch - U'0';
                }
                if (U'A' <= ch && ch <= U'F') {
                    return ch - U'A' + 10;
                }
                if (U'a' <= ch && ch <= U'f') {
                    return ch - U'a' + 10;
                }
                std::unreachable();
            }());
            
            codepoint *= 16uz;
            codepoint += digit;
        }
        if (codepoint > unicode_limit) { // 要求在 Unicode 字符集内
            co_return lex_fail(j, "out of the Unicode character set range, should not exceed 0x10FFFF");
        }
        if (auto pk = co_await peek(j); pk != U'}') {
            co_return lex_fail(j, "expected a }}, but U+{:X} found", static_cast<std::uint32_t>(pk));
        }
        co_return j;
    }
    co_return lex_fail(j, "expected a n or \\ or {{, but U+{:X} found", static_cast<std::uint32_t>(pk));
}

auto lex_single_quot() -> lexing<>
{
    assert_(co_await peek() == '\'', "expected :");
    auto j = 1uz;
    if (auto pk = co_await peek(j); pk == char32_t{}) {
        co_return lex_fail(j, "unexpected enf of line");
    } else if (pk == '\\') {
        j = co_await lex_esc_seq(j);
    } else if (pk == U'\'') {
        co_return lex_fail(1uz, "character literal is empty");
    } else {
        ++j;
    }
    if (auto pk = co_await peek(j); pk == char32_t{}) {
        co_return lex_fail(j, "unexpected end of line");
    } else if (pk != U'\'') {
        co_return lex_fail(j, "expected a ', but U+{:X} found", static_cast<std::uint32_t>(pk));
    }
    co_return store(j + 1uz, lexeme::CharLit);
}

auto lex_double_quot() -> lexing<>
{
    assert_(co_await peek() == U'"', "expected \"");
    auto j = 1uz;
    for (;; ++j) {
        auto pk = co_await peek(j);
        if (pk == char32_t{}) {
            co_return lex_fail(j, "unexpected end of file");
        }
        if (pk == U'"') {
            co_return store(j + 1uz, lexeme::StrLit);
        }
        if (pk == U'\n') {
            co_return lex_fail(j, "LF is not allowed in string literal");
        }
    }
}

auto lex_digit() -> lexing<>
{
    assert_(is_digit(co_await peek()), "expected a digit");
    auto j = 1uz;
    while (is_digit(co_await peek(j))) ++j;
    if (auto pk = co_await peek(j); pk == U'_' || is_xid_start(pk)) {
        ++j;
        while (is_xid_continue(co_await peek(j))) ++j;
    }
    co_return store(j, lexeme::DecLit);
}

auto lex_ident() -> lexing<>
{
    auto pk = co_await peek();
    assert_(pk == U'_' || is_xid_start(pk), "expected XID_Start or _");
    auto j = 1uz;
    while (is_xid_continue(co_await peek(j))) {
        ++j;
    }
    co_return store(j, lexeme::Ident);
}

auto match_ident(std::u32string_view ident) -> lexeme
{
    static const auto keywords = std::flat_map<std::u32string_view, lexeme>{
        {U"auto"sv,      lexeme::Auto     },
        {U"as"sv,        lexeme::As       },
        {U"if"sv,        lexeme::If       },
        {U"else"sv,      lexeme::Else     },
        {U"while"sv,     lexeme::While    },
        {U"for"sv,       lexeme::For      },
        {U"in"sv,        lexeme::In       },
        {U"break"sv,     lexeme::Break    },
        {U"continue"sv,  lexeme::Continue },
        {U"try"sv,       lexeme::Try      },
        {U"catch"sv,     lexeme::Catch    },
        {U"throw"sv,     lexeme::Throw    },
        {U"resume"sv,    lexeme::Resume   },
        {U"return"sv,    lexeme::Return   },
        {U"const"sv,     lexeme::Const    },
        {U"mut"sv,       lexeme::Mut      },
        {U"match"sv,     lexeme::Match    },
        {U"indep"sv,     lexeme::Indep    },
        {U"this"sv,      lexeme::This     },
        {U"true"sv,      lexeme::True     },
        {U"false"sv,     lexeme::False    },
        {U"public"sv,    lexeme::Public   },
        {U"protected"sv, lexeme::Protected},
        {U"private"sv,   lexeme::Private  },
        {U"import"sv,    lexeme::Import   },
        {U"using"sv,     lexeme::Using    },
        {U"reloc"sv,     lexeme::Reloc    },
        {U"mov"sv,       lexeme::Mov      },
        {U"trustme"sv,   lexeme::Trustme  },
        {U"asm"sv,       lexeme::Asm      },
    };
    auto it = keywords.find(ident);
    if (it == keywords.end()) return lexeme::Ident;
    return it->second;
}

auto lex_xid_start() -> lexing<>
{
    auto ident = co_await lex_ident();
    ident.type = match_ident(ident.view);
    co_return ident;
}

auto lex_underscore() -> lexing<>
{
    assert_(co_await peek() == U'_', "expected _");
    if (is_xid_continue(co_await peek(1))) {
        co_return co_await lex_ident();
    }
    co_return store(1uz, lexeme::Underscore);
}

auto lex_token() -> lexing<>
{
    auto pk = co_await peek();
    switch (pk) {
    case char32_t{}:
        co_return store(0uz, lexeme::None);
    case U'/':
        co_return co_await lex_slash();
    case U'<':
        co_return co_await lex_less();
    case U'>':
        co_return co_await lex_greater();
    case U'+':
        co_return co_await lex_plus();
    case U'-':
        co_return co_await lex_minus();
    case U'|':
        co_return co_await lex_pipe();
    case U'&':
        co_return co_await lex_ampersand();
    case U'*':
        co_return co_await lex_star();
    case U'%':
        co_return co_await lex_rem();
    case U'^':
        co_return co_await lex_caret();
    case U'~':
        co_return co_await lex_tilde();
    case U'=':
        co_return co_await lex_eq();
    case U'!':
        co_return co_await lex_not();
    case U'.':
        co_return co_await lex_dot();
    case U':':
        co_return co_await lex_colon();
    case U'{':
        co_return store(1uz, lexeme::LeftBrace);
    case U'}':
        co_return store(1uz, lexeme::RightBrace);
    case U'(':
        co_return store(1uz, lexeme::LeftParen);
    case U')':
        co_return store(1uz, lexeme::RightParen);
    case U'[':
        co_return store(1uz, lexeme::LeftBracket);
    case U']':
        co_return store(1uz, lexeme::RightBracket);
    case U';':
        co_return store(1uz, lexeme::Semicolon);
    case U',':
        co_return store(1uz, lexeme::Comma);
    case U'?':
        co_return store(1uz, lexeme::QuestionMark);
    case U'@':
        co_return store(1uz, lexeme::At);
    case U'#':
        co_return co_await lex_hash();
    case U'$':
        co_return store(1uz, lexeme::Dollar);
    case U'0':
        co_return co_await lex_zero();
    case U'"':
        co_return co_await lex_double_quot();
    case U'\'':
        co_return co_await lex_single_quot();
    case U'_':
        co_return co_await lex_underscore();
    default:
        if (is_digit(pk)) co_return co_await lex_digit();
        if (is_xid_start(pk)) co_return co_await lex_xid_start();
        co_return lex_fail("unknown character U+{:X}", static_cast<std::uint32_t>(pk));
    }
}

auto lex_line() -> lexing<std::vector<token>>
{
    auto tokens   = std::vector<token>{};
    auto filtered = std::array{
        lexeme::LineComment,
        lexeme::StreamComment,
    };
    while (!(co_await source_rem).empty()) {
        co_await lex_skip();
        auto tok = co_await lex_token();
        if (std::ranges::contains(filtered, tok.type)) {
            continue;
        }
        if (tok.type == lexeme::None) break;
        tokens.emplace_back(tok);
    }
    co_return tokens;
}

auto lex_all(std::u32string_view source) -> std::expected<std::vector<token>, error_entry>
{
    auto lines = source | std::views::split(U'\n') | std::views::transform([](auto r) {
                     return std::u32string_view{r};
                 })
                 | std::ranges::to<std::vector>();

    auto tokens = std::vector<token>{};

    for (auto lineno = 1uz, depth = 0uz; auto line : lines) {
        auto lex = lex_line();
        lex.resume(line, 0uz, {.lineno = lineno, .colno = 1uz}, depth);
        if (lex.has_error()) {
            return std::unexpected{lex.take_result().error()};
        }
        auto toks = lex.take_result().value();
        tokens.append_range(std::move(toks));
        ++lineno;
        depth = lex.comment_depth();
    }

    return tokens;
}

// 基于语法规则进行一定的预处理，以简化后续的语法分析
// 例如：`:` 后随的 `<` 总是 `LeftAngle`，而非 `Less`
auto preprocess(std::span<token> tokens) -> void
{
    if (tokens.empty()) return;
    for (auto i : std::views::iota(1uz, tokens.size())) {
        auto& cur  = tokens[i];
        auto& prev = tokens[i - 1uz];
        if (prev.type == lexeme::Colon && cur.type == lexeme::Less) {
            cur.type = lexeme::LeftAngle;
        }
    }
}

struct token_tree
{
public:
    struct token
    {
    private:
        source_span span_;
        lexeme type_;
        std::u32string_view text_;

    public:
        token(source_span span, lexeme type, std::u32string_view text) : span_{span}, type_{type}, text_{text} {}

        auto span() const -> source_span
        {
            return span_;
        }

        auto set_span(source_span span) -> void
        {
            span_ = span;
        }

        auto type() const -> lexeme
        {
            return type_;
        }

        auto set_type(lexeme type) -> void
        {
            type_ = type;
        }

        auto text() const -> std::u32string_view
        {
            return text_;
        }

        auto set_text(std::u32string_view text) -> void
        {
            text_ = text;
        }
    };

    struct group
    {
        enum struct delimiter : std::uint8_t
        {
            none,
            paren,
            bracket,
            brace,
            angle,
        };

    private:
        source_span open_;
        source_span close_;
        delimiter delim_;
        std::vector<token_tree> children_;

    public:
        group(source_span open, source_span close, delimiter delim, std::vector<token_tree> children)
            : open_{open}, close_{close}, delim_{delim}, children_{std::move(children)}
        {
        }

        auto span() const -> source_span
        {
            return source_span::make(open_, close_);
        }

        auto open() const -> source_span
        {
            return open_;
        }

        auto close() const -> source_span
        {
            return close_;
        }

        auto delim() const -> delimiter
        {
            return delim_;
        }

        auto children(this auto&& self)
        {
            return std::span{self.children_};
        }

        auto children(this auto& self, std::size_t i)
        {
            auto ret = self.children_.data();
            if (i < self.children_.size()) {
                ret += i;
            } else {
                ret = nullptr;
            }
            return ret;
        }

        auto without_delim(this auto& self)
        {
            auto span = std::span{self.children_};
            if (self.delim_ == delimiter::none) {
                return span;
            }
            assert_(span.size() >= 2uz, "expected token");
            return span.subspan(1uz, span.size() - 2uz);
        }
    };

    enum struct kind : std::uint8_t
    {
        none,
        token,
        group,
    };

private:
    kind kind_;
    union
    {
        std::unique_ptr<token> token_;
        std::unique_ptr<group> group_;
    };

    auto remove()
    {
        if (kind_ == kind::token) {
            std::destroy_at(&token_);
        } else if (kind_ == kind::group) {
            std::destroy_at(&group_);
        }
        kind_ = kind::none;
    }

    auto setup(std::unique_ptr<token> t)
    {
        remove();
        kind_ = kind::token;
        std::construct_at(&token_, std::move(t));
    }

    auto setup(std::unique_ptr<group> g)
    {
        remove();
        kind_ = kind::group;
        std::construct_at(&group_, std::move(g));
    }

public:
    using delimiter = group::delimiter;

    token_tree() : kind_{kind::none} {}

    token_tree(std::unique_ptr<token> t) : kind_{kind::token}
    {
        std::construct_at(&token_, std::move(t));
    }

    token_tree(std::unique_ptr<group> g) : kind_{kind::group}
    {
        std::construct_at(&group_, std::move(g));
    }

    token_tree(source_span span, lexeme type, std::u32string_view text)
        : token_tree{std::make_unique<token>(span, type, text)}
    {
    }

    token_tree(source_span open, source_span close, group::delimiter delim, std::vector<token_tree> children)
        : token_tree(std::make_unique<group>(open, close, delim, std::move(children)))
    {
    }

    token_tree(const token_tree&) noexcept = delete;
    token_tree(token_tree&& other) noexcept : kind_(other.kind_)
    {
        if (kind_ == kind::token) {
            std::construct_at(&token_, other.take_token());
        } else if (kind_ == kind::group) {
            std::construct_at(&group_, other.take_group());
        }
    }

    auto operator=(const token_tree&) noexcept -> token_tree& = delete;
    auto operator=(token_tree&& rhs) noexcept -> token_tree&
    {
        remove();
        kind_ = rhs.kind_;
        if (kind_ == kind::token) {
            setup(rhs.take_token());
        } else if (kind_ == kind::group) {
            setup(rhs.take_group());
        }
        return *this;
    }

    ~token_tree()
    {
        remove();
    }

    auto get_kind() const -> kind
    {
        return kind_;
    }

    auto span() const -> source_span
    {
        if (kind_ == kind::token) {
            return token_->span();
        } else if (kind_ == kind::group) {
            return group_->span();
        } else {
            return {};
        }
    }

    auto get_token() const -> token*
    {
        if (kind_ != kind::token) {
            return nullptr;
        }
        return token_.get();
    }

    auto get_group() const -> group*
    {
        if (kind_ != kind::group) {
            return nullptr;
        }
        return group_.get();
    }

    auto take_token() -> std::unique_ptr<token>
    {
        if (kind_ != kind::token) {
            return nullptr;
        }
        return std::move(token_);
    }

    auto take_group() -> std::unique_ptr<group>
    {
        if (kind_ != kind::group) {
            return nullptr;
        }
        return std::move(group_);
    }
};

auto build_token_tree(std::span<const token> tokens, std::vector<error_entry>& errors) -> token_tree
{
    static const auto lefts = std::flat_map<lexeme, token_tree::delimiter>{
        {lexeme::LeftBrace,   token_tree::delimiter::brace  },
        {lexeme::LeftBracket, token_tree::delimiter::bracket},
        {lexeme::LeftParen,   token_tree::delimiter::paren  },
        {lexeme::LeftAngle,   token_tree::delimiter::angle  },
    };
    static const auto rights = std::flat_map<lexeme, token_tree::delimiter>{
        {lexeme::RightBrace,   token_tree::delimiter::brace  },
        {lexeme::RightBracket, token_tree::delimiter::bracket},
        {lexeme::RightParen,   token_tree::delimiter::paren  },
        {lexeme::RightAngle,   token_tree::delimiter::angle  },
    };

    if (tokens.empty()) {
        return {};
    }
    auto last_pos = tokens.back().span().end;

    auto next = [&tokens](std::size_t n = 1uz) {
        tokens = tokens.subspan(std::min(n, tokens.size()));
    };
    auto build = [&](this auto&& build, token_tree::delimiter delim) -> token_tree {
        auto angles = std::vector<std::size_t>{};
        auto trees  = std::vector<token_tree>{};
        if (delim != token_tree::delimiter::none) {
            assert_(!tokens.empty(), "expected tokens");
            auto&& cur_tok = tokens.front();

            auto [text, pos, type] = cur_tok;
            assert_(lefts.contains(type) && lefts.at(type) == delim, "expected matching delimiter");
            trees.emplace_back(cur_tok.span(), type, text);
            next();
        }
        for (; !tokens.empty(); next()) {
            auto&& cur_tok = tokens.front();

            auto [text, pos, type] = cur_tok;

            if (lefts.contains(type)) {
                if (type == lexeme::LeftBrace) {
                    angles.clear();
                }
                trees.push_back(build(lefts.at(type)));
            } else if (rights.contains(type)) {
                trees.emplace_back(cur_tok.span(), type, text);
                if (rights.at(type) == delim) {
                    return {
                        trees.front().span(),
                        trees.back().span(),
                        delim,
                        std::move(trees),
                    };
                } else {
                    errors.push_back({
                        .where   = trees.back().span().start,
                        .message = std::format("unexcepted {}", type),
                        .from    = std::source_location::current(),
                    });
                }
            } else {
                if (type == lexeme::Greater && !angles.empty()) {
                    auto idx = angles.back();
                    angles.pop_back();
                    auto in_angle = std::vector<token_tree>{
                        std::from_range,
                        trees | views::drop(idx) | views::as_rvalue,
                    };
                    trees.resize(idx);
                    assert_(in_angle.front().get_token(), "expected token");
                    in_angle.front().get_token()->set_type(lexeme::LeftAngle);
                    in_angle.emplace_back(cur_tok.span(), lexeme::RightAngle, text);

                    trees.emplace_back(in_angle.front().span(),
                                       in_angle.back().span(),
                                       token_tree::delimiter::angle,
                                       std::move(in_angle));
                } else if (type == lexeme::Greater && delim == token_tree::delimiter::angle) {
                    trees.emplace_back(cur_tok.span(), type, text);
                    return {
                        trees.front().span(),
                        trees.back().span(),
                        delim,
                        std::move(trees),
                    };
                } else {
                    auto merge = [&](lexeme prev, lexeme cur, lexeme merged) {
                        if (trees.empty()) return;
                        if (type != cur) return;
                        if (auto prev_tok = trees.back().get_token()) {
                            if (prev_tok->type() != prev) return;
                            if (prev_tok->span().end != pos) return;
                            text = {prev_tok->text().begin(), text.end()};
                            pos  = prev_tok->span().start;
                            type = merged;
                            trees.pop_back();
                        }
                    };
                    merge(lexeme::Greater, lexeme::Greater, lexeme::RightShift);
                    merge(lexeme::Greater, lexeme::Assign, lexeme::GreaterEq);
                    merge(lexeme::RightShift, lexeme::Assign, lexeme::RightShiftEq);
                    if (type == lexeme::Less) {
                        angles.emplace_back(trees.size());
                    }
                    if (type == lexeme::Semicolon) {
                        angles.clear();
                    }
                    trees.emplace_back(cur_tok.span(), type, text);
                }
            }
        }
        if (delim != token_tree::delimiter::none) {
            auto expected = [&] {
                for (auto [k, v] : rights) {
                    if (v == delim) return k;
                }
                std::unreachable();
            }();
            errors.push_back({
                .where   = last_pos,
                .message = std::format("excepted {}", expected),
                .from    = std::source_location::current(),
            });
            return {};
        }
        return {
            source_span::make(trees.front().span().start, 0uz),
            source_span::make(last_pos, 0uz),
            delim,
            std::move(trees),
        };
    };
    return build(token_tree::delimiter::none);
}

}

// template <>
// struct std::formatter<autofront::token, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::token& token, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}", token.view);
//     }
// };

// template <>
// struct std::formatter<autofront::comment, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::comment& comment, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}", comment.text);
//     }
// };

// template <>
// struct std::formatter<autofront::error_entry, char> : public std::formatter<std::string_view, char>
// {
//     auto format(const autofront::error_entry& err, auto&& ctx) const
//     {
//         return std::format_to(ctx.out(), "{}: error: {}", err.where, err.message);
//     }
// };
