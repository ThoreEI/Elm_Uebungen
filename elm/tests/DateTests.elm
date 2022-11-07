module DateTests exposing (..)

import Expect exposing (..)
import Date exposing (..)
import Test exposing (..)

year : Test
year =
    test "getting a year value from a date-list"
        (\_ ->
        Expect.equal
          1234 (Date.year (1,1,1234))
        )


month : Test
month =
    test "getting a month value from a date-list"
        (\_ ->
            Expect.equal
            12 (Date.month (1,12,1))
        )

day : Test
day =
      test "getting a day value from a date-list"
          (\_ ->
              Expect.equal
              28 (Date.day (28, 1, 31))
          )


lt : Test
lt =
    test "getting True if the first parameter is the older date."
        (\_ ->
          Expect.equal
            True (Date.lt (1,1,1) (2,2,2))
        )


eq : Test
eq =
    test "getting True if the dates are equal to each other."
        (\_ ->
            Expect.equal
              True (Date.eq (1,1,1) (1,1,1))
        )


gt : Test
gt =
    test "getting False if the first parameter is the younger date."
        (\_ ->
            Expect.equal
              False (Date.gt (1,1,1) (2,2,2))
        )


toString : Test
toString =
  test "printing a date with leading zeros"
      (\_ ->
        Expect.equal
          "01.02.0003" (Date.toString(1,2,3))
      )

next : Test
next =
  test "getting the correct date of tomorrow"
      (\_ ->
        Expect.equal
          (29,2,400) (Date.next (28,2,400))
      )

prev : Test
prev =
  test "getting the correct date of yesterday"
      (\_ ->
        Expect.equal
          (31,12,1999) (Date.prev(1,1,2000))
      )


leapyear : Test
leapyear =
    test "check if it's a leapyear or not"
      (\_ ->
        Expect.equal
          (True) (Date.leapyear 1200)
      )

sub : Test
sub =
    test "check if the period of time (in days) between two dates is correct"
      (\_ ->
        Expect.equal
          (-364) (Date.sub(1,1,1)(31,12,1))
      )
