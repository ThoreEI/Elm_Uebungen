module DateTests exposing (..)

import Date exposing (..)
import Expect exposing (..)
import Test exposing (..)

year : Test
year =
    test "getting a year value from a date-list"
        (\_ ->
        Except.equal
          1234 (year (1,1,1234))
        )


month : Test
month =
    test "getting a month value from a date-list"
        (\_ ->
            Expect.equal
            12 (month(1,12,1))
        )

day : Test
day =
      test "getting a day value from a date-list"
          (\_ ->
              Expect.equal
              28 (day(28,31,31))
          )


lt : Test
lt =
    test "getting True if the first parameter is the older date."
        (\_ ->
          Expect.equal
            True (lt (1,1,1) (2,2,2))
        )


eq : Test
eq =
    test "getting True if the dates are equal to each other."
        (\_ ->
            Expect.equal
              True (eq (1,1,1) (1,1,1))
        )


gt : Test
gt =
    test "getting False if the first parameter is the younger date."
        (\_ ->
            Expect.equal
              False (lt (1,1,1) (2,2,2))
        )


toString : Test
toString =
  test "printing a date with leading zeros"
      (\_ ->
        Expect.equal
          "01.02.0003" (toString(1,2,3))
      )

next : Test
next =
  test "getting the correct date of tomorrow"
      (\_ ->
        Expect.equal
          (29,2,400) (next (28,2,400))
      )

prev : Test
prev =
  test "getting the correct date of yesterday"
      (\_ ->
        Expect.equal
          (31,12,1999) (prev(1,1,2000))
      )
