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
-- Syslog.RFC_5424 outputs messages formatted according to the              --
-- RFC 5424 ("The Syslog Protocol").                                        --
------------------------------------------------------------------------------

package Syslog.RFC_5424 is

   procedure Build_Message
     (Output : out String;
      Last : out Natural;
      Pri : in Priority;
      Message : in String;
      Hostname : in String := "-";
      App_Name : in String := "-";
      Proc_ID : in String  := "-";
      Msg_ID : in String   := "-";
      With_Time_Frac : in Boolean := True);

   procedure Build_Message
     (Output : out String;
      Last : out Natural;
      Pri : in Priority;
      Message : in String;
      Hostname : in String := "-";
      App_Name : in String := "-";
      Proc_ID : in String  := "-";
      Msg_ID : in String   := "-";
      Timestamp : Ada.Calendar.Time;
      With_Time_Frac : Boolean := True);


end Syslog.RFC_5424;
