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

with Syslog.Timestamps;

package body Syslog.RFC_5424 is

   procedure Build_Message
     (Output : out String;
      Last : out Natural;
      Pri : in Priority;
      Message : in String;
      Hostname : in String := "-";
      App_Name : in String := "-";
      Proc_ID : in String  := "-";
      Msg_ID : in String   := "-";
      With_Time_Frac : in Boolean := True)
   is
      Timestamp : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   begin
      Build_Message(Output    => Output,
                    Last      => Last,
                    Pri       => Pri,
                    Message   => Message,
                    Hostname  => Hostname,
                    App_Name  => App_Name,
                    Proc_ID   => Proc_ID,
                    Msg_ID    => Msg_ID,
                    Timestamp => Timestamp,
                    With_Time_Frac => With_Time_Frac);
   end Build_Message;

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
      With_Time_Frac : Boolean := True)
   is
      RFC_5424_Timestamp : constant String
        := Syslog.Timestamps.RFC_5424(Time_Stamp => Timestamp,
                                      With_Frac  => With_Time_Frac);
   begin
      Last := Output'First - 1;

      declare
         Priority_Image : String := Priority'Image (Pri);
      begin
         pragma Assert (Priority_Image (Priority_Image'First) = ' ');
         Priority_Image (Priority_Image'First) := '<';
         Append (Output, Last, Priority_Image);
      end;

      Append (Output, Last, ">1 ");
      Append (Output, Last, RFC_5424_Timestamp);
      Append (Output, Last, " ");
      Append (Output, Last, Check_Nil (Hostname));
      Append (Output, Last, " ");
      Append (Output, Last, Check_Nil (App_Name));
      Append (Output, Last, " ");
      Append (Output, Last, Check_Nil (Proc_ID));
      Append (Output, Last, " ");
      Append (Output, Last, Check_Nil (Msg_ID));
      Append (Output, Last, " ");
      Append (Output, Last, Message);
   end Build_Message;


end Syslog.RFC_5424;
