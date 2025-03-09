/*
* Copyright (c) 2025 Calvin Rose and contributors
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to
* deal in the Software without restriction, including without limitation the
* rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
* sell copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
* IN THE SOFTWARE.
*/

#include <janet.h>

int64_t _mod_impl(int64_t a, int64_t m) {
    if (m == 0) return a;

    int64_t x = a % m;
    return (((a ^ m) < 0) && (x != 0)) ? x + m : x;
}

int64_t _jacobi_impl(int64_t a, int64_t m) {
    int64_t res = 2;
    a = _mod_impl(a, m);
    while (a != 0) {
        int64_t l = a & -a;
        a /= l;
        res ^= (a & m) ^ ((l % 3) & (m ^ (m >> 1)));
        int64_t tmpa = a;
        a = _mod_impl(m, a);
        m = tmpa;
    }
    return (m == 1) ? (res & 2) - 1 : 0;
}

int64_t _invmod_impl(int64_t a, int64_t m) {
    int64_t x = 0;
    int64_t u = 1;
    int64_t n = (m < 0) ? -m : m;
    a = _mod_impl(a, n);
    while (a != 0) {
        int64_t tmpx = x;
        x = u;
        u = tmpx - (n / a) * u;
        int64_t tmpa = a;
        a = _mod_impl(n, a);
        n = tmpa;
    }
    return (n == 1) ? _mod_impl(x, m) : 0;
}

#if defined(__SIZEOF_INT128__)

int64_t _mulmod_impl(int64_t a, int64_t b, int64_t m) {
    if (m == 0) return a * b;

    int64_t x = (((signed __int128)a) * b) % m;
    return (((a ^ b ^ m) < 0) && (x != 0)) ? x + m : x;
}

#elif defined(_WIN64) && defined(_MSC_VER) && !defined(_M_ARM64)

#include <intrin.h>
int64_t _mulmod_impl(int64_t a, int64_t b, int64_t m) {
    if (m == 0) return a * b;

    int64_t r;
    int64_t s = _mul128(a, b, &r);
    (void)_div128(r, s, m, &r);
    return (((a ^ b ^ m) < 0) && (r != 0)) ? r + m : r;
}

#else

int64_t _add_impl(int64_t a, int64_t b, int64_t c) {
    int64_t room = (c - 1) - a;
    if (b <= room) {
        a += b;
    } else {
        a = b - room - 1;
    }
    return a;
}

int64_t _mulmod_impl(int64_t a, int64_t b, int64_t m) {
    int64_t res = 0;
    a = _mod_impl(a, m);
    b = _mod_impl(b, m);
    if (a < b) {
        int64_t tmpa = a;
        a = b;
        b = tmpa;
    }
    if (b < 0) b -= m;
    while (b > 0) {
        if ((b & 1))
            res = _add_impl(res, a, m);
        a = _add_impl(a, a, m);
        b >>= 1;
    }
    return res;
}

#endif

Janet wrap_nan() {
#ifdef NAN
    return janet_wrap_number(NAN);
#elif defined(_MSC_VER)
    return janet_wrap_number(nan("nan"));
#else
    return janet_wrap_number(0.0 / 0.0);
#endif
}

Janet wrap_result(int64_t a, Janet m) {
    if (!janet_checktype(m, JANET_ABSTRACT))
        return janet_wrap_number(a);

    const JanetAbstractType *at = janet_abstract_type(janet_unwrap_abstract(m));
    int64_t *box = janet_abstract(at, sizeof(int64_t));
    *box = a;
    return janet_wrap_abstract(box);
}

JANET_FN(cfun_cmath_jacobi,
        "(math/jacobi a m)",
        "Computes the Jacobi Symbol (a|m).") {
    janet_fixarity(argc, 2);
    int64_t a = janet_getinteger64(argv, 0);
    int64_t m = janet_getinteger64(argv, 1);

    return janet_wrap_number(_jacobi_impl(a, m));
}

JANET_FN(cfun_cmath_invmod,
        "(math/invmod a m)",
        "Modular multiplicative inverse of `a` mod `m`. "
        "Both arguments must be integer. The return value has the same type as `m`. "
        "If no inverse exists, returns `math/nan` instead.") {
    janet_fixarity(argc, 2);
    int64_t a = janet_getinteger64(argv, 0);
    int64_t m = janet_getinteger64(argv, 1);

    int64_t res = _invmod_impl(a, m);
    if (res == 0)
        return wrap_nan();

    return wrap_result(res, argv[1]);
}

JANET_FN(cfun_cmath_mulmod,
        "(math/mulmod a b m)",
        "Modular multiplication of `a` and `b` mod `m`. "
        "All arguments must be integer. The return value has the same type as `m`.") {
    janet_fixarity(argc, 3);
    int64_t a = janet_getinteger64(argv, 0);
    int64_t b = janet_getinteger64(argv, 1);
    int64_t m = janet_getinteger64(argv, 2);

    return wrap_result(_mulmod_impl(a, b, m), argv[2]);
}

JANET_FN(cfun_cmath_powmod,
        "(math/powmod a b m)",
        "Modular exponentiation of `a` to the power of `b` mod `m`. "
        "All arguments must be integer. The return value has the same type as `m`.") {
    janet_fixarity(argc, 3);
    int64_t a = janet_getinteger64(argv, 0);
    int64_t b = janet_getinteger64(argv, 1);
    int64_t m = janet_getinteger64(argv, 2);

    int64_t res = 1;
    if (b < 0) {
        a = _invmod_impl(a, m);
        if (a == 0)
            return wrap_nan();
        b = -b;
    }
    while (b > 0) {
        if ((b & 1) == 1)
            res = _mulmod_impl(res, a, m);
        a = _mulmod_impl(a, a, m);
        b >>= 1;
    }

    return wrap_result(res, argv[2]);
}

JANET_MODULE_ENTRY(JanetTable *env) {
    JanetRegExt cfuns[] = {
        JANET_REG("jacobi", cfun_cmath_jacobi),
        JANET_REG("invmod", cfun_cmath_invmod),
        JANET_REG("mulmod", cfun_cmath_mulmod),
        JANET_REG("powmod", cfun_cmath_powmod),
        JANET_REG_END
    };
    janet_cfuns_ext(env, "cmath", cfuns);
}
