<?php

namespace AdventOfCode2017\Day1;

// Functions that PHP needs
function first(array $sequence)
{
    return current(array_values($sequence));
}

function rest(array $sequence)
{
    return array_slice($sequence, 1);
}

function interleave(array $sequence1, array $sequence2)
{
    if (empty($sequence1) && empty($sequence2)) {
        return [];
    }

    $conj = [$sequence1, $sequence2];
    return array_merge(
        array_map('first', $conj),
        call_user_func_array('interleave', array_map('rest', $conj))
    );
}

function rotate(array $sequence, $places = 1)
{
    $cyclicPlaces = ($places % count($sequence));

    return array_merge(
        array_slice($sequence, $cyclicPlaces),
        array_slice($sequence, 0, $cyclicPlaces)
    );
}

// Solution
function splitDigit($digits)
{
    return array_map(function ($char) {
        return (int) $char;
    }, str_split((string) $digits));
}

function groupRightDigits(array $sequence, $places = 1)
{
    return array_chunk(interleave($sequence, rotate($sequence, $places)), 2);
}

function digitsWithRightMatch(array $sequence)
{
    return array_map('first', array_filter($sequence, function ($group) {
        return $group[0] == $group[1];
    }));
}

function captcha($digit, callable $placeFn)
{
    $digits = splitDigit($digit);

    return array_sum(digitsWithRightMatch(groupRightDigits($digits, $placeFn($digits))));
}

// captcha($digit, function () { return 1; });
// captcha($digit, function($x) { return $count / 2; });
