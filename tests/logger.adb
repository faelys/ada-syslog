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

with Ada.Command_Line;
with Ada.Text_IO;

with Syslog.Guess.App_Name;
with Syslog.Guess.Hostname;
with Syslog.Transport.Send_Task;
with Syslog.Transport.UDP;

procedure Logger is
begin
   Syslog.Guess.App_Name;
   Syslog.Guess.Hostname;
   Syslog.Transport.Send_Task.Set_Backend (Syslog.Transport.UDP.Transport);
   Syslog.Set_Transport (Syslog.Transport.Send_Task.Transport);

   case Ada.Command_Line.Argument_Count is
      when 1 =>
         Syslog.Transport.UDP.Connect ("127.0.0.1");
         Syslog.Log (Syslog.Severities.Notice, Ada.Command_Line.Argument (1));

      when 2 =>
         Syslog.Transport.UDP.Connect (Ada.Command_Line.Argument (1));
         Syslog.Log (Syslog.Severities.Notice, Ada.Command_Line.Argument (2));

      when 3 =>
         Syslog.Transport.UDP.Connect
           (Ada.Command_Line.Argument (1),
            Natural'Value (Ada.Command_Line.Argument (2)));
         Syslog.Log (Syslog.Severities.Notice, Ada.Command_Line.Argument (3));

      when others =>
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Current_Error,
            "Usage: " & Ada.Command_Line.Command_Name
           & " [target-host [port]] message");
   end case;
end Logger;
