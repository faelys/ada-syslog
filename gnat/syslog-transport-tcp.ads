-------------------------------------------------------------------------------
--      File:   syslog-transport-tcp.ads
--
--
--              Copyright (C) Piller
--
--              Florian Fischer, LMS
--------------------------------------------------------------------------------
package Syslog.Transport.TCP is
   Transport : constant Transporter;

   procedure Connect
     (Hostname : in String;
      Port : in Natural := 514);

private

   procedure Send (Message : in String);

   Transport : constant Transporter := Send'Access;
end;
