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
-- Syslog package is the main entry point for the library. It defines       --
-- useful types, export logging procedures, and holds the common state used --
-- to craft messages (send procedure, hostname, appname, etc).              --
-- Note that the common state is not thread-safe: the expected usage is to  --
-- set it up before anything has a chance to call a Log procedure.          --
-- However, since Transporter type is atomic, it should be safe to change   --
-- it concurrently with Log calls.                                          --
------------------------------------------------------------------------------

package Syslog is

   -- Support Types --

   package Facilities is
      type Code is
        (Kernel,
         User_Level,
         Mail,
         Daemon,
         Authorization_4,
         Syslog_Internal,
         Line_Printer,
         News,
         UUCP,
         Clock_9,
         Authorization_10,
         FTP,
         NTP,
         Log_Audit,
         Log_Alert,
         Clock_15,
         Local_0,
         Local_1,
         Local_2,
         Local_3,
         Local_4,
         Local_5,
         Local_6,
         Local_7);
   end Facilities;


   package Severities is
      type Code is
        (Emergency,
         Alert,
         Critical,
         Error,
         Warning,
         Notice,
         Informational,
         Debug);
   end Severities;


   type Priority is range 0 .. 191;

   function To_Priority
     (Facility : Facilities.Code;
      Severity : Severities.Code)
     return Priority;


   package Formats is
      type Format is (RFC_3164, RFC_5424);
   end Formats;


   type Transporter is private;


   -- Write To Syslog --

   procedure Log
     (Facility : in Facilities.Code;
      Severity : in Severities.Code;
      Message : in String);

   procedure Log
     (Severity : in Severities.Code;
      Message : in String);

   procedure Log
     (Facility : in Facilities.Code;
      Message : in String);

   procedure Log
     (Pri : in Priority;
      Message : in String);

   procedure Log
     (Message : in String);


   -- Setup Writing --

   procedure Set_App_Name (App_Name : in String);
   procedure Set_Default_Facility (Facility : in Facilities.Code);
   procedure Set_Default_Severity (Severity : in Severities.Code);
   procedure Set_Format (Format : in Formats.Format);
   procedure Set_Hostname (Hostname : in String);
   procedure Set_Proc_ID (Proc_ID : in String);
   procedure Set_Transport (Send : in Transporter);

private

   -- Helper Subprograms --

   procedure Append
     (Output : in out String;
      Last : in out Natural;
      Data : in String);

   function Check_Nil (Raw_Value : in String) return String;


   type Transporter is access procedure (Packet : in String);
   pragma Atomic (Transporter);

   type Bounded_String (Max_Length : Natural) is record
      Data : String (1 .. Max_Length);
      Length : Natural := 0;
   end record;

   procedure Set (Str : out Bounded_String; Value : in String);
   function Get (Str : Bounded_String) return String;

   type Context_Record is record
      Hostname : Bounded_String (255);
      App_Name : Bounded_String (48);
      Proc_ID : Bounded_String (128);
      Default_Facility : Facilities.Code := Facilities.Daemon;
      Default_Severity : Severities.Code := Severities.Emergency;
      Format : Formats.Format;
      Transport : Transporter := null;
   end record;

   Context : Context_Record;

end Syslog;
