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

package body Syslog.RFC_3164 is

   procedure Build_Message
     (Output : out String;
      Last : out Natural;
      Pri : in Priority;
      Message : in String;
      Hostname : in String := "-";
      App_Name : in String := "";
      Proc_ID : in String := "") is
   begin
      Last := Output'First - 1;

      declare
         Priority_Image : String := Priority'Image (Pri);
      begin
         pragma Assert (Priority_Image (Priority_Image'First) = ' ');
         Priority_Image (Priority_Image'First) := '<';
         Append (Output, Last, Priority_Image);
      end;

      Append (Output, Last, ">");
      Append (Output, Last, Timestamps.RFC_3164);
      Append (Output, Last, " ");
      Append (Output, Last, Check_Nil (Hostname));
      Append (Output, Last, " ");

      if App_Name /= "" then
         Append (Output, Last, App_Name);
         if Proc_ID /= "" then
            Append (Output, Last, "[");
            Append (Output, Last, Proc_ID);
            Append (Output, Last, "]");
         end if;
         Append (Output, Last, ": ");
      end if;

      Append (Output, Last, Message);
   end Build_Message;

end Syslog.RFC_3164;
