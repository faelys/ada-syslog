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

with Syslog.RFC_3164;
with Syslog.RFC_5424;

package body Syslog is

   ------------------------
   -- Helper Subprograms --
   ------------------------

   procedure Append
     (Output : in out String;
      Last : in out Natural;
      Data : in String)
   is
      Written : Natural;
   begin
      if Last + 1 not in Output'Range then
         return;
      end if;

      Written := Natural'Min (Data'Length, Output'Last - Last);
      Output (Last + 1 .. Last + Written)
        := Data (Data'First .. Data'First + Written - 1);
      Last := Last + Written;
   end Append;


   function Check_Nil (Raw_Value : in String) return String is
   begin
      if Raw_Value'Length > 0 then
         return Raw_Value;
      else
         return "-";
      end if;
   end Check_Nil;


   function To_Priority
     (Facility : Facilities.Code;
      Severity : Severities.Code)
     return Priority is
   begin
      return Facilities.Code'Pos (Facility) * 8
        + Severities.Code'Pos (Severity);
   end To_Priority;



   ----------------------
   -- Public Interface --
   ----------------------

   procedure Log
     (Facility : in Facilities.Code;
      Severity : in Severities.Code;
      Message : in String) is
   begin
      Log (To_Priority (Facility, Severity), Message);
   end Log;


   procedure Log
     (Severity : in Severities.Code;
      Message : in String) is
   begin
      Log (To_Priority (Context.Default_Facility, Severity), Message);
   end Log;


   procedure Log
     (Facility : in Facilities.Code;
      Message : in String) is
   begin
      Log (To_Priority (Facility, Context.Default_Severity), Message);
   end Log;


   procedure Log
     (Pri : in Priority;
      Message : in String) is
   begin
      if Context.Transport = null then
         return;
      end if;

      case Context.Format is
         when Formats.RFC_3164 =>
            declare
               Packet : String (1 .. 1024);
               Last : Natural;
            begin
               RFC_3164.Build_Message
                 (Packet, Last,
                  Pri => Pri,
                  Message => Message,
                  Hostname => Get (Context.Hostname),
                  App_Name => Get (Context.App_Name),
                  Proc_ID => Get (Context.Proc_ID));

               Context.Transport (Packet (1 .. Last));
            end;

         when Formats.RFC_5424 =>
            declare
               Packet : String (1 .. 2048);
               Last : Natural;
            begin
               RFC_5424.Build_Message
                 (Packet, Last,
                  Pri => Pri,
                  Message => Message,
                  Hostname => Get (Context.Hostname),
                  App_Name => Get (Context.App_Name),
                  Proc_ID => Get (Context.Proc_ID));

               Context.Transport (Packet (1 .. Last));
            end;
      end case;
   end Log;



   procedure Log (Message : in String) is
   begin
      Log
        (To_Priority
           (Context.Default_Facility,
            Context.Default_Severity),
         Message);
   end Log;


   procedure Set_App_Name (App_Name : in String) is
   begin
      Set (Context.App_Name, App_Name);
   end Set_App_Name;


   procedure Set_Default_Facility (Facility : in Facilities.Code) is
   begin
      Context.Default_Facility := Facility;
   end Set_Default_Facility;


   procedure Set_Default_Severity (Severity : in Severities.Code) is
   begin
      Context.Default_Severity := Severity;
   end Set_Default_Severity;


   procedure Set_Format (Format : in Formats.Format) is
   begin
      Context.Format := Format;
   end Set_Format;


   procedure Set_Hostname (Hostname : in String) is
   begin
      Set (Context.Hostname, Hostname);
   end Set_Hostname;


   procedure Set_Proc_ID (Proc_ID : in String) is
   begin
      Set (Context.Proc_ID, Proc_ID);
   end Set_Proc_ID;


   procedure Set_Transport (Send : in Transporter) is
   begin
      Context.Transport := Send;
   end Set_Transport;



   -------------------------------
   -- Bounded String Primitives --
   -------------------------------

   procedure Set (Str : out Bounded_String; Value : in String) is
   begin
      Str.Length := Natural'Min (Value'Length, Str.Max_Length);
      Str.Data (1 .. Str.Length)
        := Value (Value'First .. Value'First + Str.Length - 1);
   end Set;


   function Get (Str : Bounded_String) return String is
   begin
      return Str.Data (1 .. Str.Length);
   end Get;

end Syslog;
