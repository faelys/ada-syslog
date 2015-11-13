-------------------------------------------------------------------------------
--      File:   syslog-guess-pid.adb
--
--
--              Copyright (C) Piller
--
--              Florian Fischer, LMS
--------------------------------------------------------------------------------


with Ada.Text_IO;
procedure Syslog.Guess.PID is
   function Get_PID return Integer;
   -- http://man7.org/linux/man-pages/man2/getpid.2.html
   pragma Import(C,Get_PID,"getpid");

   DEBUG : constant Boolean := true;

   procedure Put_Line (Text : String)
   is
   begin
      if(DEBUG) then
         Ada.Text_IO.Put_Line(Text);
      else
         return;
      end if;
   end;

begin
   Put_Line("Syslog.Guess.PID: " & Integer'Image(Get_PID));
   Set_Proc_ID(Integer'Image(Get_PID));
end Syslog.Guess.PID;
