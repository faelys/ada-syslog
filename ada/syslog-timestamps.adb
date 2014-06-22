------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

package body Syslog.Timestamps is

   Months : constant array (Ada.Calendar.Month_Number) of String (1 .. 3)
     := ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   Digit : constant array (0 .. 9) of Character
     := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9');


   function RFC_3164 return String is
      Result : String (1 .. 15);
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;
   begin
      Ada.Calendar.Formatting.Split
        (Now,
         Year, Month, Day,
         Hour, Minute, Second, Sub_Second,
         Ada.Calendar.Time_Zones.UTC_Time_Offset (Now));

      Result (1 .. 3) := Months (Month);
      Result (4) := ' ';

      if Day < 10 then
         Result (5) := ' ';
         Result (6) := Digit (Day);
      else
         Result (5) := Digit (Day / 10);
         Result (6) := Digit (Day mod 10);
      end if;

      Result (7 .. 15) :=
        (' ',
         Digit (Hour / 10),
         Digit (Hour mod 10),
         ':',
         Digit (Minute / 10),
         Digit (Minute mod 10),
         ':',
         Digit (Second / 10),
         Digit (Second mod 10));

      return Result;
   end RFC_3164;


   function RFC_5424 (With_Frac : Boolean := True) return String is
      use type Ada.Calendar.Time_Zones.Time_Offset;

      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Offset : constant Ada.Calendar.Time_Zones.Time_Offset
        := Ada.Calendar.Time_Zones.UTC_Time_Offset (Now);

      Year : Ada.Calendar.Year_Number;
      Month : Ada.Calendar.Month_Number;
      Day : Ada.Calendar.Day_Number;
      Hour : Ada.Calendar.Formatting.Hour_Number;
      Minute : Ada.Calendar.Formatting.Minute_Number;
      Second : Ada.Calendar.Formatting.Second_Number;
      Sub_Second : Ada.Calendar.Formatting.Second_Duration;

      Prefix : String (1 .. 19);
      Frac : String (1 .. 7);
      Time_Zone : String (1 .. 6);
   begin
      Ada.Calendar.Formatting.Split
        (Now,
         Year, Month, Day,
         Hour, Minute, Second, Sub_Second,
         Offset);

      Prefix :=
        (Digit (Year / 1000),
         Digit ((Year / 100) mod 10),
         Digit ((Year / 10) mod 10),
         Digit (Year mod 10),
         '-',
         Digit (Month / 10),
         Digit (Month mod 10),
         '-',
         Digit (Day / 10),
         Digit (Day mod 10),
         'T',
         Digit (Hour / 10),
         Digit (Hour mod 10),
         ':',
         Digit (Minute / 10),
         Digit (Minute mod 10),
         ':',
         Digit (Second / 10),
         Digit (Second mod 10));

      if With_Frac then
         Frac :=
           ('.',
            Digit (Natural (Sub_Second * 10)),
            Digit (Natural (Sub_Second * 100) mod 10),
            Digit (Natural (Sub_Second * 1000) mod 10),
            Digit (Natural (Sub_Second * 10000) mod 10),
            Digit (Natural (Sub_Second * 100000) mod 10),
            Digit (Natural (Sub_Second * 1000000) mod 10));
      end if;

      if Offset = 0 then
         if With_Frac then
            return Prefix & Frac & 'Z';
         else
            return Prefix & 'Z';
         end if;
      else
         declare
            Off_Minute : constant Natural := Natural (abs Offset mod 60);
            Off_Hour : constant Natural := Natural (abs Offset / 60);
         begin
            if Offset < 0 then
               Time_Zone (1) := '-';
            else
               Time_Zone (1) := '+';
            end if;

            Time_Zone (2 .. 6) :=
              (Digit (Off_Hour / 10),
               Digit (Off_Hour mod 10),
               ':',
               Digit (Off_Minute / 10),
               Digit (Off_Minute mod 10));
         end;

         if With_Frac then
            return Prefix & Frac & Time_Zone;
         else
            return Prefix & Time_Zone;
         end if;
      end if;
   end RFC_5424;

end Syslog.Timestamps;
