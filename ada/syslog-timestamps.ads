with Ada.Calendar;
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

------------------------------------------------------------------------------
-- Syslog.Timestamps provides functions that output a String                --
-- reprensentation of the current time.                                     --
-- They are kept in a dedicated in a dedicated package so they can be       --
-- easily replaced by another implementation. For example, this is the only --
-- part in ada-syslog core that doesn't fit in Ravenscar restrictions.      --
------------------------------------------------------------------------------

package Syslog.Timestamps is

   function RFC_3164 return String;

   function RFC_5424 (With_Frac : Boolean := True) return String;
   function RFC_5424 (Time_Stamp : Ada.Calendar.Time; With_Frac : Boolean := True) return String;

end Syslog.Timestamps;
