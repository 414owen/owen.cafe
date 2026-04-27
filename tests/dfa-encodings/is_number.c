#include <stdio.h>
#include <stdbool.h>
#include <assert.h>
#include <stdint.h>

bool is_number_table(char *input);
bool is_number_switchy(char *input);
bool is_number_table_equiv(char *input);
bool is_number_direct(char *input);
bool is_number_manual(char *input);

static void run_tests(bool (*test)(char*)) {
    // Valid integers
    assert(test("0") == true);
    assert(test("9") == true);
    assert(test("123") == true);
    assert(test("-1") == true);
    assert(test("-999") == true);

    // Valid decimals
    assert(test("0.1") == true);
    assert(test("9.1") == true);
    assert(test("-0.5") == true);
    assert(test("123.456") == true);

    // Invalid: leading zeros
    assert(test("01") == false);
    assert(test("-01") == false);

    // Invalid: trailing zeros in decimal
    assert(test("1.0") == false);
    assert(test("1.10") == false);

    // Invalid formats
    assert(test("") == false);
    assert(test("-") == false);
    assert(test(".1") == false);
    assert(test("1.") == false);
    assert(test("abc") == false);
    assert(test("1a") == false);
    assert(test("--1") == false);
    assert(test("+-1") == false);

    // Leading zeros
    assert(test("00") == false);
    assert(test("01") == false);
    assert(test("09") == false);
    assert(test("004.12") == false);
    assert(test("-0") == false);

    // Trailing zeros
    assert(test("0.0") == false);
    assert(test("-1.50200") == false);
    assert(test("4.0") == false);
}

int main() {
    printf("Checking switchy implementation\n");
    run_tests(is_number_switchy);
    printf("Checking table implementation\n");
    run_tests(is_number_table);
    printf("Checking table implementation w/ equivalence classes\n");
    run_tests(is_number_table_equiv);
    printf("Checking direct interpretation\n");
    run_tests(is_number_direct);
    printf("Checking manual (non-dfa) implementation\n");
    run_tests(is_number_manual);
    printf("All tests passed.\n");
    return 0;
}
