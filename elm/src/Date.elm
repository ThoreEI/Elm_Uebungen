module Date exposing (year, month, day, lt, eq, gt, toString, next, prev, leapyear, sub)
import Html
type alias Day = Int
type alias Month = Int
type alias Year = Int
type alias Date = (Day, Month, Year)

year : Date -> Year
year (_,_,currentYear) = currentYear

month : Date -> Month
month (_,currentMonth,_) = currentMonth

day : Date -> Day
day (currentDay,_,_) = currentDay

-- date1 < date2 -> True
lt : Date -> Date -> Bool
lt date1 date2 =
            year date1 < year date2
          ||year date1 == year date2 && month date1 < month date2
          ||year date1 == year date2 && month date1 == month date2 && day date1 < day date2

eq : Date -> Date -> Bool
eq date1 date2 = date1 == date2

gt : Date -> Date -> Bool
gt date1 date2=
    lt date2 date1

toString : Date -> String
toString date =
          dayOrMonthToString (day date)++"."
          ++dayOrMonthToString(month date)++"."
          ++yearToString(year date)

dayOrMonthToString: Int -> String
dayOrMonthToString dayOrMonth =
            if  dayOrMonth < 10 then
                "0"++String.fromInt dayOrMonth
            else
                String.fromInt dayOrMonth

yearToString currentYear =
            if  currentYear < 10 then
                "000"++String.fromInt currentYear
            else if currentYear < 100 then
                "00"++String.fromInt currentYear
            else if currentYear < 1000 then
              "0"++String.fromInt currentYear
            else
                String.fromInt currentYear

next: Date -> Date
next (d,m,y) =
      if d < getDaysOfMonth m y then
          (d+1, m, y)
      else if m < 12 then
          (1,m+1, y)
      else
          (1,1,y+1)

getDaysOfMonth: Month -> Year -> Day
getDaysOfMonth m y  =
      if m == 2 then
        if leapyear y then
          29
        else
          28
      else if m==1||m==3||m==5||m==7||m==8||m==10||m==12 then
          31
      else
          30

getDaysOfYear: Year -> Day
getDaysOfYear y =
    if leapyear y then
        366
    else
        365

prev : Date -> Date
prev (d,m,y) =
      if d > 1 then
        (d-1, m, y)
      else if m < 12 then
        (getDaysOfMonth(m-1)(y),(m-1),y)
      else
        (1,1,y+1)

leapyear : Year -> Bool
leapyear currentYear = modBy currentYear 4 == 0 && modBy currentYear 100 /= 0 || modBy currentYear 400 == 0

sub : Date -> Date -> Day
sub date1 date2 =
          if gt date1 date2 then
              -(sub date1 date2)
          else
              day date1 - day date2
            + monthsToDays (month date1) (year date1) - monthsToDays (month date2) (year date2)
            + List.sum(List.map(\thisYear -> getDaysOfYear thisYear) (List.range(year date1+1) (year date2-1)))

monthsToDays: Month -> Year -> Day
monthsToDays currentMonth currentYear =
              List.sum (List.map(\thisMonth -> getDaysOfMonth thisMonth currentYear)(List.range 1 (currentMonth-1)))

result = String.fromInt (sub (1,1,1)(1,1,1000))

main =
  Html.text result
